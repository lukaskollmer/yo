//
//  IRGen.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

// NOTE: Most (all?) of this is crap and will be rewritten eventually, once i know how codegen is actually supposed to be implemented (ideally some time after passing cc?)

#include "IRGen.h"

#include <optional>
#include <limits>

#include "Mangling.h"
#include "util_llvm.h"
#include "TemplateResolution.h"
#include "Attributes.h"

using namespace yo;
using namespace yo::irgen;
using namespace yo::util::llvm_utils;


inline constexpr unsigned kInstanceMethodCallArgumentOffset = 1;
static const std::string kRetvalAllocaIdentifier = "%retval";

#define unhandled_node(node) \
{ std::cout << __PRETTY_FUNCTION__ << ": Unhandled Node: " << util::typeinfo::GetTypename(*(node)) << std::endl; \
throw; }


// IRGen

llvm::LLVMContext IRGenerator::C;

IRGenerator::IRGenerator(const std::string ModuleName)
    : Module(llvm::make_unique<llvm::Module>(ModuleName, C)),
    M(Module.get()),
    Builder(C),
    DIBuilder(*Module)
{
    i8  = llvm::Type::getInt8Ty(C);
    i16 = llvm::Type::getInt16Ty(C);
    i32 = llvm::Type::getInt32Ty(C);
    i64 = llvm::Type::getInt64Ty(C);
    
    i8_ptr = i8->getPointerTo();
    Void = llvm::Type::getVoidTy(C);
    i1 = Bool = llvm::Type::getInt1Ty(C);
    Double = llvm::Type::getDoubleTy(C);
    
    CompileUnit = DIBuilder.createCompileUnit(llvm::dwarf::DW_LANG_C,
                                              DIBuilder.createFile("main.yo", "."),
                                              "yo", false, "", 0);
    DILexicalBlocks.push_back(CompileUnit);
    
    Module->addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
}



void IRGenerator::EmitDebugLocation(const std::shared_ptr<ast::Node> &Node) {
    if (!Node) {
        Builder.SetCurrentDebugLocation(llvm::DebugLoc());
        return;
    }
    
    auto &SL = Node->getSourceLocation();
    Builder.SetCurrentDebugLocation(llvm::DebugLoc::get(SL.Line, SL.Column, DILexicalBlocks.back()));
}




void IRGenerator::Codegen(ast::AST &Ast) {
    Preflight(Ast);
    
    VerifyDeclarations();
    
    for (auto &Node : Ast) {
        Codegen(Node);
    }
    
    DIBuilder.finalize();
}



std::string MangleFullyResolved(const std::shared_ptr<ast::FunctionSignature> &signature) {
    if (signature->attributes->no_mangle) {
        return signature->Name;
    } else if (!signature->attributes->mangledName.empty()) {
        return signature->attributes->mangledName;
    }
    return mangling::MangleFullyResolvedNameForSignature(signature);
}

void IRGenerator::Preflight(ast::AST &Ast) {
    // Q: Why collect the different kinds of top level decls first and then process them, instead of simply processing them all in a single for loop?
    // A: What if a function uses a type that is declared at some later point, or in another module? it's important all of these are processed in the correct order
    std::vector<std::shared_ptr<ast::TypealiasDecl>>        Typealiases;
    std::vector<std::shared_ptr<ast::FunctionDecl>>         FunctionDecls;
    std::vector<std::shared_ptr<ast::StructDecl>>           StructDecls;
    std::vector<std::shared_ptr<ast::ImplBlock>>            ImplBlocks;
    
#define HANDLE(T, Dest) if (auto X = std::dynamic_pointer_cast<ast::T>(Node)) { Dest.push_back(X); continue; }
    
    for (auto &Node : Ast) {
        HANDLE(TypealiasDecl, Typealiases)
        HANDLE(FunctionDecl, FunctionDecls)
        HANDLE(StructDecl, StructDecls)
        HANDLE(ImplBlock, ImplBlocks)
        
        unhandled_node(Node)
    }
    
#undef HANDLE
    
    for (auto &Typealias : Typealiases) {
        // TODO is this a good idea?
        // TODO prevent circular aliases!
        TypeCache.Insert(Typealias->Typename, TypeInfo::MakeTypealias(Typealias->Typename, Typealias->Type));
    }
    
    for (auto &StructDecl : StructDecls) {
        RegisterStructDecl(StructDecl);
    }
    
    for (auto &FunctionDecl : FunctionDecls) {
        if (FunctionDecl->Signature->attributes->extern_) {
            FunctionDecl->Signature->attributes->no_mangle = true;
        }
        RegisterFunction(FunctionDecl);
    }
    
    for (auto &ImplBlock : ImplBlocks) {
        RegisterImplBlock(ImplBlock);
    }
}


void IRGenerator::RegisterFunction(std::shared_ptr<ast::FunctionDecl> Function) {
    auto Signature = Function->Signature;
    
    if (Signature->Name == "main") {
        Function->Signature->attributes->no_mangle = true;
        // TODO run some checks to make sure main fulfills the requirements (correct return & parameter types, no other attributes, etc)
//        LKAssert(Signature->ReturnType->Equals(TypeInfo::i32));
//        LKAssert(Signature->Parameters.size() == 0 || Signature->Parameters.size() == 2);
//        if (Signature->Parameters.size() == 2) {
//            LKAssert(Signature->Parameters[0]->Type->Equals(TypeInfo::))
//        }
    }
    
    
    if (Signature->IsTemplateFunction && !Signature->IsFullSpecialization()) {
        auto CanonicalName = mangling::MangleCanonicalNameForSignature(Signature);
        Functions[CanonicalName].push_back(ResolvedFunction(Function, nullptr));
        return;
    }
    
    std::vector<llvm::Type *> ParameterTypes;
    
    for (auto &P : Signature->Parameters) {
        ParameterTypes.push_back(GetLLVMType(P->Type));
    }
    
    std::string canonicalName, resolvedName;
    
    if (Signature->attributes->extern_) {
        canonicalName = resolvedName = mangling::MangleCanonicalNameForSignature(Signature);
    } else {
        canonicalName = mangling::MangleCanonicalNameForSignature(Signature);
        resolvedName = MangleFullyResolved(Signature);
    }
    
    
    if (auto OtherSignature = GetResolvedFunctionWithName(resolvedName)) {
        LKAssert(Signature->attributes->extern_ && "only extern functions are allowed to have multiple declarations");
        LKAssert(Signature->ReturnType->Equals(OtherSignature->ReturnType));
        LKAssert(Signature->Parameters.size() == OtherSignature->Parameters.size());
        LKAssert(*Signature->attributes == *OtherSignature->attributes);
        return;
    }
    
    LKAssertMsg(M->getFunction(resolvedName) == nullptr, util::fmt_cstr("Redefinition of function '%s'", resolvedName.c_str())); // TODO print the signature instead!
    
    auto FT = llvm::FunctionType::get(GetLLVMType(Signature->ReturnType), ParameterTypes, Function->Signature->attributes->variadic);
    auto F = llvm::Function::Create(FT, llvm::Function::LinkageTypes::ExternalLinkage, resolvedName, M);
    
    ResolvedFunctions[resolvedName] = Function->Signature;
    Functions[canonicalName].push_back(ResolvedFunction(Function, F));
}


std::shared_ptr<ast::FunctionSignature> IRGenerator::GetResolvedFunctionWithName(const std::string &Name) {
    if (auto It = ResolvedFunctions.find(Name); It != ResolvedFunctions.end()) {
        return It->second;
    }
    return nullptr;
}


void IRGenerator::RegisterStructDecl(std::shared_ptr<ast::StructDecl> structDecl) {
    auto Name = structDecl->Name->Value;
    auto T = TypeInfo::MakeComplex(Name);
    TypeCache.Insert(Name, T);
    TypeCache.RegisterStruct(Name, structDecl);
    
    auto LKYOObjectBase = TypeCache.GetStruct("LKMetadataAccessor");
    
    if (EnableARC && structDecl->attributes->arc) {
        structDecl->Members.insert(structDecl->Members.begin(), LKYOObjectBase->Members.begin(), LKYOObjectBase->Members.end());
    }
    
    // TODO forward-declare the struct's default initializer!
    
    
    if (!structDecl->attributes->no_init) {
        auto initializer = std::make_shared<ast::FunctionDecl>();
        initializer->Signature = std::make_shared<ast::FunctionSignature>();
        initializer->Signature->attributes = std::make_shared<attributes::FunctionAttributes>();
        initializer->Body = std::make_shared<ast::Composite>();
        initializer->Signature->Name = "init";
        initializer->Signature->Kind = ast::FunctionSignature::FunctionKind::StaticMethod;
        initializer->Signature->ReturnType = TypeInfo::MakePointer(T);
        initializer->Signature->ImplType = structDecl;
        
        initializer->Signature->Parameters = structDecl->Members;
        
        if (EnableARC && structDecl->attributes->arc) {
            auto &Params = initializer->Signature->Parameters;
            Params.erase(Params.begin(), Params.begin() + LKYOObjectBase->Members.size());
        }
        
//        if (!structDecl->attributes->arc) {
//            initializer->Signature->Parameters = structDecl->Members;
//        } else {
//            initializer->Signature->Parameters = std::vector<std::shared_ptr<ast::VariableDecl>>(structDecl->Members.begin() + LKYOObjectBase->Members.size(),
//                                                                                                 structDecl->Members.end());
//        }
        
        RegisterFunction(initializer);
    }
}



void IRGenerator::RegisterImplBlock(std::shared_ptr<ast::ImplBlock> ImplBlock) {
    using FK = ast::FunctionSignature::FunctionKind;
    
    auto Typename = ImplBlock->Typename;
    LKAssert(TypeCache.Contains(Typename));
    auto T = TypeInfo::MakeComplex(Typename);
    
    for (auto &F : ImplBlock->Methods) {
        LKAssert(!F->Signature->attributes->no_mangle && "invalid attribute for function in impl block: no_mangle");
        auto Kind = FK::StaticMethod;
        if (!F->Signature->Parameters.empty()) {
            auto First = F->Signature->Parameters[0];
            if (First->Name->Value == "self" && First->Type->Equals(T->getPointerTo())) { // TODO remove the T check, just look for self?
                Kind = FK::InstanceMethod;
            }
        }
        F->Signature->Kind = Kind;
        F->Signature->ImplType = TypeCache.GetStruct(Typename);
        RegisterFunction(F);
    }
}






#pragma mark - Decl Verification


void IRGenerator::VerifyDeclarations() {
    // TODO
}




# pragma mark - Codegen


#define HANDLE(node, T, ...) \
if (auto X = std::dynamic_pointer_cast<ast::T>(node)) return Codegen(X, ## __VA_ARGS__);

#define IGNORE(node, T) \
if (std::dynamic_pointer_cast<ast::T>(node)) return nullptr;


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::TopLevelStmt> TLS) {
    HANDLE(TLS, FunctionDecl)
    HANDLE(TLS, StructDecl)
    HANDLE(TLS, ImplBlock)
    IGNORE(TLS, TypealiasDecl)
    
    unhandled_node(TLS)
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::LocalStmt> LocalStmt) {
    HANDLE(LocalStmt, Composite)
    HANDLE(LocalStmt, VariableDecl)
    HANDLE(LocalStmt, IfStmt)
    HANDLE(LocalStmt, Assignment)
    HANDLE(LocalStmt, WhileStmt)
    HANDLE(LocalStmt, ForLoop)
    HANDLE(LocalStmt, ExprStmt)
    
    unhandled_node(LocalStmt);
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Expr> Expr, CodegenReturnValueKind ReturnValueKind) {
    HANDLE(Expr, NumberLiteral)
    HANDLE(Expr, BinaryOperation)
    HANDLE(Expr, Identifier, ReturnValueKind)
    HANDLE(Expr, Comparison)
    HANDLE(Expr, LogicalOperation)
    HANDLE(Expr, Typecast)
    HANDLE(Expr, StringLiteral)
    HANDLE(Expr, UnaryExpr)
    HANDLE(Expr, MatchExpr)
    HANDLE(Expr, RawLLVMValueExpr)
    HANDLE(Expr, MemberExpr, ReturnValueKind)
    HANDLE(Expr, SubscriptExpr, ReturnValueKind)
    HANDLE(Expr, CallExpr)
    
    unhandled_node(Expr)
}

#undef HANDLE
#undef IGNORE


llvm::DIFile *_DIFileForNode(llvm::DIBuilder &DIBuilder, const std::shared_ptr<ast::Node> &Node) {
    auto &SL = Node->getSourceLocation();
    auto [Directory, Filename] = util::string::extractPathAndFilename(SL.Filepath);
    return DIBuilder.createFile(Filename, Directory);
}



llvm::DISubroutineType *IRGenerator::_ToDISubroutineType(ast::FunctionSignature *Signature) {
    // Looking at [godbolt](https://godbolt.org/z/EKfzqi ), it seems like the first element should be the function's return type?
    
    std::vector<llvm::Metadata *> Types;
    
    Types.push_back(GetDIType(Signature->ReturnType));
    for (auto &Param : Signature->Parameters) {
        Types.push_back(GetDIType(Param->Type));
    }
    
    return DIBuilder.createSubroutineType(DIBuilder.getOrCreateTypeArray(Types));
}


#pragma mark - Top Level Statements

llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::FunctionDecl> FunctionDecl) {
    auto &Sig = FunctionDecl->Signature;
    
    if (Sig->attributes->extern_) {
        return nullptr;
    }
    
    if (Sig->IsTemplateFunction && !Sig->IsFullSpecialization()) {
        return nullptr;
    }
    
    LKAssert(Scope.isEmpty());
    auto ResolvedName = MangleFullyResolved(Sig);
    
    auto F = M->getFunction(ResolvedName);
    if (!F) {
        LKFatalError("Unable to find function '%s'", ResolvedName.c_str());
    }
    
    if (Sig->attributes->inline_) {
        F->addFnAttr(llvm::Attribute::InlineHint);
    }
    if (Sig->attributes->always_inline) {
        F->addFnAttr(llvm::Attribute::AlwaysInline);
    }
    
    
    auto Unit = _DIFileForNode(DIBuilder, Sig);
    auto SP = DIBuilder.createFunction(Unit, Sig->Name, ResolvedName, Unit,
                                       Sig->getSourceLocation().Line,
                                       _ToDISubroutineType(Sig.get()),
                                       Sig->getSourceLocation().Line,
                                       llvm::DINode::FlagZero,
                                       llvm::DISubprogram::DISPFlags::SPFlagDefinition);
    
    auto EntryBB = llvm::BasicBlock::Create(C, "entry", F);
    auto ReturnBB = llvm::BasicBlock::Create(C, "return");
    Builder.SetInsertPoint(EntryBB);
    
    EmitDebugLocation(nullptr);
    
    F->setSubprogram(SP);
    DIBuilder.finalizeSubprogram(SP);
    DILexicalBlocks.push_back(SP);
    
    
    std::vector<llvm::AllocaInst *> ParamAllocas;
    
    for (auto &Param : Sig->Parameters) {
        auto Alloca = Builder.CreateAlloca(GetLLVMType(Param->Type));
        auto &Name = Param->Name->Value;
        Alloca->setName(Name);
        Scope.Insert(Name, Param->Type, ValueBinding(Alloca, [=]() {
            return Builder.CreateLoad(Alloca);
        }, [=](llvm::Value *V) {
            LKFatalError("Function arguments are read-only (%s in %s)", Name.c_str(), ResolvedName.c_str());
        }));
        ParamAllocas.push_back(Alloca);
    }
    
    
    for (size_t I = 0; I < Sig->Parameters.size(); I++) {
        auto Alloca = ParamAllocas.at(I);
        Builder.CreateStore(&F->arg_begin()[I], Alloca);
        
        auto &Param = Sig->Parameters.at(I);
        auto VarInfo = DIBuilder.createParameterVariable(SP, Alloca->getName(), I + 1, Unit,
                                                         Param->getSourceLocation().Line,
                                                         GetDIType(Param->Type));
        DIBuilder.insertDeclare(Alloca, VarInfo, DIBuilder.createExpression(),
                                llvm::DILocation::get(C, Param->getSourceLocation().Line, Param->getSourceLocation().Column, SP),
                                EntryBB);
    }
    
    
    
    
    llvm::Value *RetvalAlloca = nullptr;
    auto ReturnType = Sig->ReturnType;
    
    if (!ReturnType->IsVoidType()) {
        RetvalAlloca = Builder.CreateAlloca(F->getFunctionType()->getReturnType());
        auto RetvalBinding = ValueBinding(RetvalAlloca, []() {
            LKFatalError("retval is write-only");
            return nullptr;
        }, [this, RetvalAlloca](llvm::Value *V) {
            Builder.CreateStore(V, RetvalAlloca);
        });
        Scope.Insert(kRetvalAllocaIdentifier, ReturnType, RetvalBinding);
        
        
        // Create Debug Metadata
        auto D = DIBuilder.createAutoVariable(SP, kRetvalAllocaIdentifier,
                                              Unit,
                                              Sig->getSourceLocation().Line,
                                              GetDIType(ReturnType));
        DIBuilder.insertDeclare(RetvalAlloca, D,
                                DIBuilder.createExpression(),
                                llvm::DebugLoc::get(Sig->getSourceLocation().Line, 0, SP),
                                EntryBB);
    }
    
    CurrentFunction = FunctionState(FunctionDecl, F, ReturnBB, RetvalAlloca);
    
    Codegen(FunctionDecl->Body);
    
    F->getBasicBlockList().push_back(ReturnBB);
    Builder.SetInsertPoint(ReturnBB);
    
    if (ReturnType->IsVoidType()) {
        Builder.CreateRetVoid();
    } else {
        Builder.CreateRet(Builder.CreateLoad(RetvalAlloca));
    }
    
    LKAssert(Scope.size() == Sig->Parameters.size() + static_cast<uint8_t>(!ReturnType->IsVoidType()));
    
    for (auto &Entry : Scope.GetEntriesSinceMarker(0)) {
        //std::cout << std::get<0>(Entry) << std::endl;
        // TODO: release
        Scope.Remove(Entry.Ident);
    }
    
    CurrentFunction = FunctionState();
    DILexicalBlocks.pop_back(); // TODO maybe add a check that the lexical blocks weren't somehow modified?
    return F;
}




llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::StructDecl> structDecl) {
    if (!structDecl->attributes->no_init) {
        GenerateStructInitializer(structDecl);
    }
    return nullptr;
}



llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::ImplBlock> ImplBlock) {
    auto Typename = ImplBlock->Typename;
    for (auto &M : ImplBlock->Methods) {
        Codegen(M);
    }
    return nullptr;
}




#pragma mark - Local Statements


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::VariableDecl> Decl) {
    TypeInfo *Type = nullptr /*TI::Unreoslved*/;
    bool HasInferredType = false;
    
    if ((Type = Decl->Type) == nullptr /*TI::Unreoslved*/) {
        // If no type is specified, there _has_ to be an initial value
        LKAssert(Decl->InitialValue);
        Type = GuessType(Decl->InitialValue);
        HasInferredType = true;
    }
    
    LKAssert(Type);
    auto Alloca = Builder.CreateAlloca(GetLLVMType(Type));
    Alloca->setName(Decl->Name->Value);
    
    // Create Debug Metadata
    auto D = DIBuilder.createAutoVariable(CurrentFunction.LLVMFunction->getSubprogram(),
                                          Decl->Name->Value,
                                          DILexicalBlocks.back()->getFile(),
                                          Decl->getSourceLocation().Line,
                                          GetDIType(Type));
    DIBuilder.insertDeclare(Alloca, D,
                            DIBuilder.createExpression(),
                            llvm::DebugLoc::get(Decl->getSourceLocation().Line, 0, CurrentFunction.LLVMFunction->getSubprogram()),
                            Builder.GetInsertBlock());
    
    auto Binding = ValueBinding(Alloca, [=] () {
        return Builder.CreateLoad(Alloca);
    }, [=] (llvm::Value *V) {
        LKAssert(V->getType() == Alloca->getType()->getPointerElementType());
        Builder.CreateStore(V, Alloca);
    });
    
    Scope.Insert(Decl->Name->Value, Type, Binding);
    
    if (auto Expr = Decl->InitialValue) {
        // Q: Why create and handle an assignment to set the initial value, instead of just calling Binding.Write?
        // A: The Assignment codegen also includes the trivial type transformations, whish we'd otherwise have to implement again in here
        Codegen(std::make_shared<ast::Assignment>(Decl->Name, Decl->InitialValue));
    }
    
    return Alloca;
}







llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Composite> composite) {
    EmitDebugLocation(composite);
    
    auto marker = Scope.GetMarker();
    bool didReturn = false;
    
    for (auto it = composite->Statements.begin(); !didReturn && it != composite->Statements.end(); it++) {
        auto &stmt = *it;
        if (auto returnStmt = std::dynamic_pointer_cast<ast::ReturnStmt>(stmt)) {
            Codegen(returnStmt);
            didReturn = true;
        } else {
            Codegen(stmt);
        }
    }
    
    for (auto &entry : Scope.GetEntriesSinceMarker(marker)) {
        Scope.Remove(entry.Ident);
    }
    
    return nullptr;
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::ReturnStmt> ReturnStmt) {
    EmitDebugLocation(ReturnStmt);
    
    auto FName = Builder.GetInsertBlock()->getParent()->getName().str();
    auto F = ResolvedFunctions[FName];
    
    if (auto Expr = ReturnStmt->Expression) {
        TypeInfo *T;
        if (!TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(&Expr, F->ReturnType, &T)) {
            LKFatalError("Error: Can't return value of type '%s' from function '%s' returning '%s'", T->Str().c_str(), FName.c_str(), F->ReturnType->Str().c_str());
        }
        
        Codegen(std::make_shared<ast::Assignment>(std::make_shared<ast::Identifier>(kRetvalAllocaIdentifier), Expr));
        return Builder.CreateBr(CurrentFunction.ReturnBB);
    }
    
    LKAssert(F->ReturnType->Equals(TypeInfo::Void));
    //return Builder.CreateRetVoid();
    return Builder.CreateBr(CurrentFunction.ReturnBB);
}


template <typename T>
bool value_fits_in_type(uint64_t Value) {
    auto Min = std::numeric_limits<T>::min();
    auto Max = std::numeric_limits<T>::max();
    return static_cast<T>(Value) >= Min && static_cast<T>(Value) <= Max;
}


bool IntegerLiteralFitsInType(uint64_t Value, TypeInfo *TI) {
    #define CASE(sizetype, signed_t, unsigned_t) case TypeInfo::sizetype: return Signed ? value_fits_in_type<signed_t>(Value) : value_fits_in_type<unsigned_t>(Value);
    
    LKAssert(TI->IsIntegerType());
    bool Signed = TI->IsSigned();
    
    switch (TI->getSize()) {
        CASE(kSizeof_u8,  int8_t,  uint8_t)
        CASE(kSizeof_u16, int16_t, uint16_t)
        CASE(kSizeof_u32, int32_t, uint32_t)
        CASE(kSizeof_u64, int64_t, uint64_t)
        default:
            LKFatalError("should not reach here");
    }
#undef CASE
}


bool IRGenerator::TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *Expr, TypeInfo *ExpectedType, TypeInfo **InitialTypeOfExpr) {
    auto Type = GuessType(*Expr);
    if (InitialTypeOfExpr) *InitialTypeOfExpr = Type;
    
    if (Type->Equals(ExpectedType)) return true;
    
    // at this point, both are integers
    if (auto NumberLiteral = std::dynamic_pointer_cast<ast::NumberLiteral>(*Expr)) {
        LKAssert(ExpectedType->IsIntegerType());
        LKAssert(IntegerLiteralFitsInType(NumberLiteral->Value, ExpectedType));
        
        *Expr = std::make_shared<ast::Typecast>(*Expr, ExpectedType, ast::Typecast::CastKind::StaticCast);
        return true;
    }
    
    std::cout << "input: " << Type->Str() << ", expected: " << ExpectedType->Str() << std::endl;
    throw;
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Assignment> assignment) {
    EmitDebugLocation(assignment);
    // TODO should assignments return something?
    // TODO rewrite this so that it doesn't rely on GuessType for function calls!
    
    auto expr = assignment->Value;
    auto destTy = GuessType(assignment->Target);
    
    TypeInfo *T;
    if (!TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(&expr, destTy, &T)) {
        LKFatalError("type mismatch: cannot assign '%s' to '%s'", T->Str().c_str(), destTy->Str().c_str());
    }
    
    auto target = Codegen(assignment->Target, CodegenReturnValueKind::Address);
    Builder.CreateStore(Codegen(expr, CodegenReturnValueKind::Value), target);
    
    return nullptr;
}








#pragma mark - Expressions


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::RawLLVMValueExpr> RawExpr) {
    return RawExpr->Value;
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::ExprStmt> exprStmt) {
    EmitDebugLocation(exprStmt->expr);
    return Codegen(exprStmt->expr);
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::NumberLiteral> NumberLiteral) {
    EmitDebugLocation(NumberLiteral);
    
    using NT = ast::NumberLiteral::NumberType;
    
    switch (NumberLiteral->Type) {
        case NT::Boolean: {
            return llvm::ConstantInt::get(i1, NumberLiteral->Value);
        }
        case NT::Character: {
            LKAssert(IntegerLiteralFitsInType(NumberLiteral->Value, TypeInfo::i8));
            return llvm::ConstantInt::get(i8, NumberLiteral->Value);
        }
        case NT::Integer: {
            return llvm::ConstantInt::get(llvm::Type::getInt64Ty(C), NumberLiteral->Value);
        }
        case NT::Double: {
            LKFatalError("TODO: implement");
        }
    }
}



llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::StringLiteral> stringLiteral) {
    using SLK = ast::StringLiteral::StringLiteralKind;
    
    EmitDebugLocation(stringLiteral);

    switch (stringLiteral->Kind) {
        case SLK::ByteString:
            return Builder.CreateGlobalStringPtr(stringLiteral->Value);
        case SLK::NormalString: {
            LKAssert(TypeCache.Contains("String"));
            stringLiteral->Kind = SLK::ByteString;
            auto target = std::make_shared<ast::Identifier>(mangling::MangleCanonicalName("String", "new", ast::FunctionSignature::FunctionKind::StaticMethod));
            auto call = std::make_shared<ast::CallExpr>(target, std::vector<std::shared_ptr<ast::Expr>>(1, stringLiteral));
            return Codegen(call);
        }
    }
}



// If TakeAddress is true, this returns a pointer to the identifier, instead of the value stored
llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Identifier> Ident, CodegenReturnValueKind ReturnValueKind) {
    EmitDebugLocation(Ident);
    
    if (auto Binding = Scope.GetBinding(Ident->Value)) {
        switch (ReturnValueKind) {
            case CodegenReturnValueKind::Value:
                return Binding->Read();
            case CodegenReturnValueKind::Address:
                return const_cast<llvm::Value *>(Binding->Value);
        }
    }
    
    std::cout << "Unable to find identifier " << Ident->Value << std::endl;
    throw;
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Typecast> Cast) {
    EmitDebugLocation(Cast);
    
    auto SrcTy = GuessType(Cast->Expression);
    auto DestTy = Cast->DestType;
    
    if (SrcTy->Equals(DestTy)) {
        return Codegen(Cast->Expression);
    }
    
    llvm::Instruction::CastOps Op;
    switch (Cast->Kind) {
        case ast::Typecast::CastKind::Bitcast: {
            LKAssert(M->getDataLayout().getTypeSizeInBits(GetLLVMType(SrcTy)) == M->getDataLayout().getTypeSizeInBits(GetLLVMType(DestTy)));
            if (SrcTy->IsPointer() && DestTy->IsIntegerType()) {
                Op = llvm::Instruction::CastOps::PtrToInt;
            } else if (SrcTy->IsIntegerType() && DestTy->IsPointer()) {
                Op = llvm::Instruction::CastOps::IntToPtr;
            } else {
                Op = llvm::Instruction::CastOps::BitCast;
            }
            break;
        }
        case ast::Typecast::CastKind::StaticCast: {
            if (SrcTy->IsIntegerType() && DestTy->IsIntegerType()) {
                auto SrcIntWidth  = GetLLVMType(SrcTy)->getIntegerBitWidth();
                auto DestIntWidth = GetLLVMType(DestTy)->getIntegerBitWidth();
                
                if (SrcIntWidth > DestIntWidth) {
                    // casting to a smaller type
                    Op = llvm::Instruction::CastOps::Trunc;
                } else {
                    // casting to a larger type
                    if (SrcTy->IsSigned()) {
                        Op = llvm::Instruction::CastOps::SExt;
                    } else {
                        Op = llvm::Instruction::CastOps::ZExt;
                    }
                }
                break;
            }
            throw;
        }
    }
    
    return Builder.CreateCast(Op, Codegen(Cast->Expression), GetLLVMType(DestTy));
}







llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::MemberExpr> memberExpr, CodegenReturnValueKind returnValueKind) {
    EmitDebugLocation(memberExpr);
    
    auto targetTy = GuessType(memberExpr->target);
    LKAssert(targetTy->IsPointer() && targetTy->getPointee()->IsComplex());
    
    auto [memberIndex, memberType] = TypeCache.GetMember(targetTy->getPointee()->getName(), memberExpr->memberName);
    
    
    llvm::Value *offsets[] = {
        llvm::ConstantInt::get(i64, 0),
        llvm::ConstantInt::get(i32, memberIndex),
    };
    
    auto V = Builder.CreateGEP(Codegen(memberExpr->target), offsets);
    
    switch (returnValueKind) {
        case CodegenReturnValueKind::Address:
            return V;
        case CodegenReturnValueKind::Value:
            return Builder.CreateLoad(V);
    }
}




llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::SubscriptExpr> subscript, CodegenReturnValueKind returnValueKind) {
    EmitDebugLocation(subscript);
    
    auto target = Codegen(subscript->target, CodegenReturnValueKind::Value);
    LKAssert(target->getType()->isPointerTy());
    auto offset = Codegen(subscript->offset, CodegenReturnValueKind::Value);
    LKAssert(offset->getType()->isIntegerTy());
    
    auto GEP = Builder.CreateGEP(target, Codegen(subscript->offset));
    
    switch (returnValueKind) {
        case CodegenReturnValueKind::Address:
            return GEP;
        case CodegenReturnValueKind::Value:
            return Builder.CreateLoad(GEP);
    }
}




bool _IsValidMatchPatternForMatchedExprType(std::shared_ptr<ast::Expr> patternExpr, TypeInfo *matchedExprType) {
    // Only patterns that are trivially and can be matched w/out side effects are allowed
    // TODO add the side effect checking
    
    if (dynamic_cast<ast::Identifier *>(patternExpr.get())) {
        throw;
        return true;
    }
    
    if (matchedExprType->IsIntegerType()) {
        return std::dynamic_pointer_cast<ast::NumberLiteral>(patternExpr) != nullptr;
    } else if (matchedExprType->Equals(TypeInfo::Bool)) {
        auto numberExpr = std::dynamic_pointer_cast<ast::NumberLiteral>(patternExpr);
        return numberExpr && numberExpr->Type == ast::NumberLiteral::NumberType::Boolean;
    } else {
        return false;
    }
}


llvm::Value *IRGenerator::Codegen_HandleMatchPatternExpr(MatchExprPatternCodegenInfo Info) {
    EmitDebugLocation(Info.PatternExpr);
    
    auto TT = Info.TargetType;
    auto PE = Info.PatternExpr;
    auto PT = GuessType(PE);
    
    if (TT->IsIntegerType()) {
        if (auto NumberLiteral = std::dynamic_pointer_cast<ast::NumberLiteral>(PE)) {
            if (ValueIsTriviallyConvertibleTo(NumberLiteral, TT)) {
                return Codegen(std::make_shared<ast::Comparison>(ast::Comparison::Operation::EQ,
                                                                 std::make_shared<ast::RawLLVMValueExpr>(Info.TargetLLVMValue, TT),
                                                                 NumberLiteral));
            }
        } else {
            LKFatalError("Incompatible Match types: cannot match %s against %s", TT->Str().c_str(), PT->Str().c_str());
        }
    }
    throw;
}



bool _LastBranchIsWildcard(const std::shared_ptr<ast::MatchExpr> &MatchExpr) {
    auto LastBranch = MatchExpr->Branches.back();
    if (LastBranch->Patterns.size() > 1) return false;
    if (auto Ident = std::dynamic_pointer_cast<ast::Identifier>(LastBranch->Patterns[0])) {
        return Ident->Value == "_";
    }
    return false;
}



// TODO should this go in the control flow section?
llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::MatchExpr> MatchExpr) {
    EmitDebugLocation(MatchExpr);
    
    // TODO require that match patterns cannot contain side effects? (this should go in _IsValidMatchPatternForMatchedExprType!)
    auto F = CurrentFunction.LLVMFunction;
    auto MatchedExprType = GuessType(MatchExpr->Target);
    auto ResultType = GuessType(MatchExpr->Branches.front()->Expression);
    auto MatchTargetValue = Codegen(MatchExpr->Target);
    
    
    std::map<llvm::BasicBlock *, llvm::Value *> BranchMappings;
    
    auto MergeBB = llvm::BasicBlock::Create(C);
    auto NextCondBB = llvm::BasicBlock::Create(C);
    auto NextValueBB = llvm::BasicBlock::Create(C);
    
    // TODO get rid of this and just have the first condition be part of the BB containing the match expression
    Builder.CreateBr(NextCondBB);
    
    auto LastBranchIsWildcard = _LastBranchIsWildcard(MatchExpr);
    
    for (size_t I = 0; I < MatchExpr->Branches.size(); I++) {
        auto &Branch = MatchExpr->Branches[I];
        auto ValueBB = NextValueBB;
        NextValueBB = llvm::BasicBlock::Create(C);
        
        bool IsLastBranchBeforeWildcard = LastBranchIsWildcard && I + 2 == MatchExpr->Branches.size();
        
        for (auto It = Branch->Patterns.begin(); It != Branch->Patterns.end(); It++) {
            auto &PatternExpr = *It;
            if (auto Ident = std::dynamic_pointer_cast<ast::Identifier>(PatternExpr)) {
                LKAssert(It + 1 == Branch->Patterns.end() && Branch->Patterns.size() == 1);
                LKAssert(Ident->Value == "_");
                break;
            } else {
                // Not a wildcard
                F->getBasicBlockList().push_back(NextCondBB);
                Builder.SetInsertPoint(NextCondBB);
                NextCondBB = llvm::BasicBlock::Create(C);
                
                auto Cond = Codegen_HandleMatchPatternExpr({MatchedExprType, MatchExpr->Target, MatchTargetValue, PatternExpr});
                // If we reach here and the pattern didn't match and the next pattern is a wildcard, go directly to the value branch
                Builder.CreateCondBr(Cond, ValueBB,
                                     IsLastBranchBeforeWildcard && It + 1 == Branch->Patterns.end() ? NextValueBB : NextCondBB);
            }
        }
        
        TypeInfo *_InitialTy = nullptr;
        if (!TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(&Branch->Expression, ResultType, &_InitialTy)) {
            LKFatalError("Invalid match branch result value: Type %s not compatible with expected type %s",
                         _InitialTy->Str().c_str(), ResultType->Str().c_str());
        }
        
        F->getBasicBlockList().push_back(ValueBB);
        Builder.SetInsertPoint(ValueBB);
        BranchMappings[ValueBB] = Codegen(Branch->Expression);
        Builder.CreateBr(MergeBB);
    }
    
    
    
    F->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    
    auto PHI = Builder.CreatePHI(GetLLVMType(ResultType), BranchMappings.size());
    for (auto [BB, V] : BranchMappings) {
        PHI->addIncoming(V, BB);
    }
    
    return PHI;
}





// MARK: Binops




llvm::Instruction::BinaryOps GetLLVMBinaryOpInstruction_Int(ast::BinaryOperation::Operation Op, bool IsSigned) {
    using Operation = ast::BinaryOperation::Operation;
    using BinaryOps = llvm::Instruction::BinaryOps;
    
    switch (Op) {
        case Operation::Add: return BinaryOps::Add;
        case Operation::Sub: return BinaryOps::Sub;
        case Operation::Mul: return BinaryOps::Mul;
        case Operation::Div: return IsSigned ? BinaryOps::SDiv : BinaryOps::UDiv;
        case Operation::Mod: return IsSigned ? BinaryOps::SRem : BinaryOps::URem;
        case Operation::And: return BinaryOps::And;
        case Operation::Or:  return BinaryOps::Or;
        case Operation::Xor: return BinaryOps::And;
        case Operation::Shl: return BinaryOps::Shl;
        case Operation::Shr: return BinaryOps::LShr; // TODO (important) arithmetic or logical right shift?
    }
    
    llvm_unreachable("should never reach here");
}




llvm::Instruction::BinaryOps GetLLVMBinaryOpInstruction_Double(ast::BinaryOperation::Operation Op) {
    using Operation = ast::BinaryOperation::Operation;
    using BinaryOps = llvm::Instruction::BinaryOps;
    
    switch (Op) {
        case Operation::Add: return BinaryOps::FAdd;
        case Operation::Sub: return BinaryOps::FSub;
        case Operation::Mul: return BinaryOps::FMul;
        case Operation::Div: return BinaryOps::FDiv;
        case Operation::Mod: return BinaryOps::FRem;
        default: llvm_unreachable("nope");
    }
}


bool IRGenerator::TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *Lhs, std::shared_ptr<ast::Expr> *Rhs, TypeInfo **LhsTy_out, TypeInfo **RhsTy_out) {
    LKAssert(LhsTy_out && RhsTy_out);
    
    auto LhsTy = GuessType(*Lhs);
    auto RhsTy = GuessType(*Rhs);
    
    *LhsTy_out = LhsTy;
    *RhsTy_out = RhsTy;
    
    if (LhsTy->Equals(RhsTy)) {
        return true;
    }
    
    // TODO add some kind of "types are compatible for this kind of binary operation" check
    
    if (!LhsTy->IsIntegerType() || !RhsTy->IsIntegerType()) {
        LKFatalError("oh no");
    }
    
    if (std::dynamic_pointer_cast<ast::NumberLiteral>(*Lhs)) {
        // lhs is literal, cast to type of ths
        *Lhs = std::make_shared<ast::Typecast>(*Lhs, RhsTy, ast::Typecast::CastKind::StaticCast);
        *LhsTy_out = RhsTy;
    } else if (std::dynamic_pointer_cast<ast::NumberLiteral>(*Rhs)) {
        // rhs is literal, cast to type of lhs
        *Rhs = std::make_shared<ast::Typecast>(*Rhs, LhsTy, ast::Typecast::CastKind::StaticCast);
        *RhsTy_out = LhsTy;
    } else {
        return false;
    }
    
    return true;
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::BinaryOperation> Binop) {
    EmitDebugLocation(Binop);
    
    auto Lhs = Binop->LHS;
    auto Rhs = Binop->RHS;
    TypeInfo *LhsTy, *RhsTy;
    
    if (!TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(&Lhs, &Rhs, &LhsTy, &RhsTy)) {
        LKFatalError("unable to create binop for supplied operand types '%s' and '%s'", LhsTy->Str().c_str(), RhsTy->Str().c_str());
    }
    
    LKAssert(LhsTy->Equals(RhsTy));
    return Builder.CreateBinOp(GetLLVMBinaryOpInstruction_Int(Binop->Op, LhsTy->IsSigned()), Codegen(Lhs), Codegen(Rhs));
}



std::optional<std::map<std::string, TypeInfo *>> IRGenerator::AttemptToResolveTemplateArgumentTypesForCall(std::shared_ptr<ast::FunctionDecl> templateFunction, std::shared_ptr<ast::CallExpr> call, unsigned argumentOffset) {
    auto sig = templateFunction->Signature;
    
    if (sig->Parameters.size() != call->arguments.size()) {
        return std::nullopt;
    }
    
    std::map<std::string, TypeInfo *> templateArgumentMapping;
    
    for (size_t idx = 0; idx < sig->TemplateArgumentNames.size(); idx++) {
        auto name = sig->TemplateArgumentNames[idx];
        if (idx < call->explicitTemplateArgumentTypes.size()) {
            templateArgumentMapping[name] = call->explicitTemplateArgumentTypes[idx];
        } else {
            templateArgumentMapping[name] = TypeInfo::Unresolved;
        }
    }
    
    for (size_t idx = argumentOffset; idx < call->arguments.size(); idx++) {
        std::string paramTypename;
        auto paramType = sig->Parameters[idx]->Type;
        unsigned paramIndirectionCount = 0;
        
        if (paramType->IsPointer()) {
            auto TI = paramType;
            while (TI->IsPointer()) {
                paramIndirectionCount += 1;
                TI = TI->getPointee();
            }
            paramTypename = TI->getName();
        } else {
            paramTypename = paramType->getName();
        }
        
        if (auto mapping = templateArgumentMapping.find(paramTypename); mapping != templateArgumentMapping.end()) {
            auto guessedArgumentType = GuessType(call->arguments[idx]);
            if (mapping->second == TypeInfo::Unresolved) {
                while (paramIndirectionCount-- > 0) {
                    LKAssert(guessedArgumentType->IsPointer());
                    guessedArgumentType = guessedArgumentType->getPointee();
                }
                mapping->second = guessedArgumentType;
            } else {
                LKAssert(mapping->second->Equals(guessedArgumentType));
            }
        }
    }
    
    return templateArgumentMapping;
}



uint8_t _getArgumentOffset(TypeInfo *ty) {
    switch (ty->getFunctionTypeInfo().callingConvention) {
        case yo::TypeInfo::FunctionTypeInfo::CallingConvention::C:
            return 0;
        case yo::TypeInfo::FunctionTypeInfo::CallingConvention::Yo:
            return 1;
    }
}


std::shared_ptr<ast::FunctionSignature> _makeFunctionSignatureFromFunctionTypeInfo(TypeInfo *ty) {
    LKAssert(ty->IsFunction());
    auto functionTyInfo = ty->getFunctionTypeInfo();
    
    auto sig = std::make_shared<ast::FunctionSignature>();
    sig->ReturnType = functionTyInfo.returnType;
    sig->Parameters = util::vector::map(functionTyInfo.parameterTypes, [] (TypeInfo *paramTy) {
        return std::make_shared<ast::VariableDecl>(ast::Identifier::EmptyIdent(), paramTy);
    });
    
    return sig;
}



NEW_ResolvedFunction IRGenerator::ResolveCall(std::shared_ptr<ast::CallExpr> callExpr, bool omitCodegen) {
    std::string targetName;
    uint8_t argumentOffset = 0;
    
    if (auto ident = std::dynamic_pointer_cast<ast::Identifier>(callExpr->target)) {
        targetName = ident->Value;
        
        if (Scope.Contains(targetName)) {
            auto ty = Scope.GetType(targetName);
            LKAssert(ty->IsFunction() && "cannot call a non-function variable");
            if (omitCodegen) {
                return NEW_ResolvedFunction(_makeFunctionSignatureFromFunctionTypeInfo(ty), nullptr, _getArgumentOffset(ty));
            } else {
                return NEW_ResolvedFunction(_makeFunctionSignatureFromFunctionTypeInfo(ty), Codegen(ident), _getArgumentOffset(ty));
            }
        }
        
    } else if (auto staticDeclRefExpr = std::dynamic_pointer_cast<ast::StaticDeclRefExpr>(callExpr->target)) {
        targetName = mangling::MangleCanonicalName(staticDeclRefExpr->typeName, staticDeclRefExpr->memberName, ast::FunctionSignature::FunctionKind::StaticMethod);
        
    } else if (auto memberExpr = std::dynamic_pointer_cast<ast::MemberExpr>(callExpr->target)) {
        // <memberExpr>()
        // two options:
        // - calling a method
        // - calling a property that happens to be a function
        
        auto targetTy = GuessType(memberExpr->target);
        LKAssert(targetTy->IsPointer() && targetTy->getPointee()->IsComplex());
        auto structName = targetTy->getPointee()->getName();
        
        if (TypeCache.StructHasMember(structName, memberExpr->memberName)) {
            auto memberTy = TypeCache.GetMember(structName, memberExpr->memberName).second;
            LKAssert(memberTy->IsFunction() && "cannot call a non-function struct member");
            // struct properties cannot be overloaded, simply return what we found
            if (omitCodegen) {
                return NEW_ResolvedFunction(_makeFunctionSignatureFromFunctionTypeInfo(memberTy), nullptr, _getArgumentOffset(memberTy));
            } else {
                return NEW_ResolvedFunction(_makeFunctionSignatureFromFunctionTypeInfo(memberTy), Codegen(memberExpr), _getArgumentOffset(memberTy));
            }
            
        } else {
            targetName = mangling::MangleCanonicalName(structName, memberExpr->memberName, ast::FunctionSignature::FunctionKind::InstanceMethod);
            argumentOffset = kInstanceMethodCallArgumentOffset;
        }
    } else {
        throw;
    }
    
    auto specializeTemplateFunctionForCall = [this, argumentOffset, omitCodegen] (std::shared_ptr<ast::FunctionDecl> functionDecl, std::map<std::string, TypeInfo *> templateArgumentMapping) -> NEW_ResolvedFunction {
        auto specializedDecl = TemplateResolver::SpecializeWithTemplateMapping(functionDecl, templateArgumentMapping);
        llvm::Function *llvmFunction = nullptr;
        if (!omitCodegen && !specializedDecl->Signature->attributes->intrinsic) {
            RegisterFunction(specializedDecl);
            llvmFunction = WithCleanSlate([&]() { return llvm::dyn_cast<llvm::Function>(Codegen(specializedDecl)); });
        }
        return NEW_ResolvedFunction(specializedDecl->Signature, llvmFunction, argumentOffset);
    };
    
    
    
    
    auto &possibleTargets = Functions[targetName];
    LKAssertMsg(!possibleTargets.empty(), util::fmt_cstr("Unable to resolve call to %s", targetName.c_str()));
    
    
    if (possibleTargets.size() == 1) {
        auto &target = possibleTargets[0];
        if (!target.Decl->Signature->IsTemplateFunction) {
            return NEW_ResolvedFunction(target.Decl->Signature, target.LLVMFunction, argumentOffset);
        }
        
        // is a template function
        
        auto templateArgumentMapping = AttemptToResolveTemplateArgumentTypesForCall(target.Decl, callExpr, argumentOffset);
        LKAssert(templateArgumentMapping.has_value());
        return specializeTemplateFunctionForCall(target.Decl, templateArgumentMapping.value());
    }
    
    
    // more than one potential target
    
    
    struct FunctionResolutionMatchInfo {
        uint32_t score;
        std::shared_ptr<ast::FunctionDecl> decl;
        llvm::Function *llvmFunction; // nullptr if this is a yet to be instantiated template function
        std::map<std::string, TypeInfo *> templateArgumentMapping;
    };
    
    
    // List of uninstantiated function templates that might be potential targets
    std::vector<std::shared_ptr<ast::FunctionDecl>> templateFunctions;
    // list of potential targets, with a score indicating how close they match the call
    std::vector<FunctionResolutionMatchInfo> matches;
    bool hasPerfectMatch = false;
    
    
    for (auto &target : possibleTargets) {
        auto &decl = target.Decl;
        auto signature = decl->Signature;
        
        if (signature->Parameters.size() != callExpr->arguments.size()) {
            continue;
        }
        
        if (signature->IsTemplateFunction && !signature->IsFullSpecialization()) {
            templateFunctions.push_back(decl);
            continue;
        }
        
        uint32_t score = 0;
        
        for (size_t i = 0; i < callExpr->arguments.size(); i++) {
            auto arg = callExpr->arguments[i];
            auto argTy = GuessType(arg);
            auto expectedTy = signature->Parameters[i]->Type;
            
            if (argTy->Equals(expectedTy)) {
                score += 10;
            } else if (auto numberLiteral = std::dynamic_pointer_cast<ast::NumberLiteral>(arg)) {
                LKAssert(numberLiteral->Type == ast::NumberLiteral::NumberType::Integer);
                if (ValueIsTriviallyConvertibleTo(numberLiteral, expectedTy)) {
                    score += 5;
                }
            }
        }
        
        matches.push_back({score, decl, target.LLVMFunction, {}});
        
        if (score == callExpr->arguments.size() * 10) {
            // TODO does this mean we might miss other ambigious functions?
            hasPerfectMatch = true;
            break;
        }
    }
    
    if (!hasPerfectMatch) {
        for (auto &target : templateFunctions) {
            if (auto templateArgMapping = AttemptToResolveTemplateArgumentTypesForCall(target, callExpr, argumentOffset)) {
                auto argc = target->Signature->Parameters.size();
                uint32_t score = argc * 10;
                matches.push_back({score, target, nullptr, templateArgMapping.value()});
            } else {
//                std::cout << "(skipped bc unable to resolve): " << Target->Signature << std::endl;
            }
        }
    }
    
    // TODO this seems like a bad idea
    std::sort(matches.begin(), matches.end(), [](auto &arg0, auto &arg1) { return arg0.score > arg1.score; });
    
#if 0
    std::cout << "Matching overloads:\n";
    for (auto &match : matches) {
        std::cout << "- " << match.score << ": " << match.decl->Signature << std::endl;
    }
#endif
    
    if (matches.size() > 1 && matches[0].score == matches[1].score) {
        std::cout << "Error: ambiguous function call. unable to resolve. Potential candidates are:\n";
        for (auto &match : matches) {
            std::cout << "- " << match.score << ": " << match.decl->Signature << std::endl;
        }
        throw;
    }
    
    auto bestMatch = matches.front();
    
    if (bestMatch.decl->Signature->IsTemplateFunction && !bestMatch.llvmFunction) {
        return specializeTemplateFunctionForCall(bestMatch.decl, bestMatch.templateArgumentMapping);
    }
    return NEW_ResolvedFunction(bestMatch.decl->Signature, bestMatch.llvmFunction, argumentOffset);
}








bool CallerCalleeSideEffectsCompatible(const std::vector<yo::attributes::SideEffect> &callerSideEffects,
                                       const std::vector<yo::attributes::SideEffect> &calleeSideEffects) {
    if (callerSideEffects.size() == 1 && callerSideEffects[0] == yo::attributes::SideEffect::Unknown) {
        return true;
    }
    
    for (auto &sideEffect : calleeSideEffects) {
        if (!util::vector::contains(callerSideEffects, sideEffect)) return false;
    }
    
    return true;
}




llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::CallExpr> call) {
    EmitDebugLocation(call);
    
    auto resolvedTarget = ResolveCall(call, false);
    
    // TODO:
    // - run argument type checks for intrinsics as well
    // - check that the number of supplied explicit template arguments does't exceed the total number of supplied template arguments
    
    if (!CallerCalleeSideEffectsCompatible(CurrentFunction.Decl->Signature->attributes->side_effects, resolvedTarget.signature->attributes->side_effects)) {
        auto targetName = mangling::MangleCanonicalNameForSignature(resolvedTarget.signature);
        LKFatalError("cannot call '%s' because side effects", targetName.c_str());
    }
    
    
    for (size_t i = resolvedTarget.argumentOffset; i < resolvedTarget.signature->Parameters.size(); i++) {
        auto expectedType = resolvedTarget.signature->Parameters[i]->Type;
        auto expr = call->arguments[i - resolvedTarget.argumentOffset];
        TypeInfo *T;
        if (!TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(&expr, expectedType, &T)) {
            LKFatalError("Type mismatch in call to '%s'. Arg #%i: expected '%s', got '%s'",
                         MangleFullyResolved(resolvedTarget.signature).c_str(),
                         i, expectedType->Str().c_str(), T->Str().c_str());
        }
        // TODO is modifying the arguments in-place necessarily a good idea?
        call->arguments[i] = expr;
    }
    
    if (resolvedTarget.signature->attributes->intrinsic) {
        return Codegen_HandleIntrinsic(resolvedTarget.signature, call);
    }
    
    
    
    llvm::Value *llvmFunction = resolvedTarget.llvmValue;
    LKAssert(llvmFunction->getType()->isPointerTy() && llvmFunction->getType()->getContainedType(0)->isFunctionTy());
    auto llvmFunctionTy = llvm::dyn_cast<llvm::FunctionType>(llvmFunction->getType()->getContainedType(0));
    auto isVariadic = llvmFunctionTy->isVarArg();
    
    LKAssert(call->arguments.size() >= llvmFunctionTy->getNumParams() - resolvedTarget.argumentOffset - isVariadic);
    
    std::vector<llvm::Value *> args(resolvedTarget.argumentOffset, nullptr);
    auto numFixedArgs = llvmFunctionTy->getNumParams() - resolvedTarget.argumentOffset;
    
    for (size_t i = resolvedTarget.argumentOffset; i < llvmFunctionTy->getNumParams(); i++) {
        auto expectedType = resolvedTarget.signature->Parameters[i]->Type;
        auto expr = call->arguments[i - resolvedTarget.argumentOffset];
        TypeInfo *T;
        if (!TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(&expr, expectedType, &T)) {
            LKFatalError("Type mismatch in call to '%s'. Arg #%i: expected '%s', got '%s'", llvmFunction->getName().str().c_str(), i, expectedType->Str().c_str(), T->Str().c_str());
        }
        args.push_back(Codegen(expr));
    }
    
    if (auto memberExpr = std::dynamic_pointer_cast<ast::MemberExpr>(call->target); memberExpr != nullptr && resolvedTarget.argumentOffset == kInstanceMethodCallArgumentOffset) {
        // TODO this is a pretty bad assumption to make.
        // what if in the future there are more situations other than member function calls that require special argument offsets
        // maybe a better idea: get rid of the argumentOffset thing, introduce more granular calling conventions (yo.globalFunction, yo.staticmember, yo.instancemember, yo.lambda, etc) and make implicit argument insertion dependent on that!
        args[0] = Codegen(memberExpr->target);
    }
    
    if (isVariadic && GetResolvedFunctionWithName(llvmFunction->getName().str())) {
        for (auto it = call->arguments.begin() + numFixedArgs; it != call->arguments.end(); it++) {
            args.push_back(Codegen(*it));
        }
    } else if (isVariadic) {
        throw; // TODO implement
    }
    
    return Builder.CreateCall(llvmFunction, args);
}



llvm::Value *IRGenerator::Codegen_HandleIntrinsic(std::shared_ptr<ast::FunctionSignature> signature, std::shared_ptr<ast::CallExpr> call) {
    auto name = mangling::MangleCanonicalNameForSignature(signature);
    
    if (name == "static_cast" || name == "reinterpret_cast") {
        // TODO somehow use the SrcTy, if explicitly given?
        auto dstTy = call->explicitTemplateArgumentTypes[0];
        auto arg = call->arguments[0];
        auto castKind = name == "static_cast"
            ? ast::Typecast::CastKind::StaticCast
            : ast::Typecast::CastKind::Bitcast;
        return Codegen(std::make_shared<ast::Typecast>(arg, dstTy, castKind));
    }
    
    if (name == "sizeof") {
        auto T = GetLLVMType(call->explicitTemplateArgumentTypes[0]);
        return llvm::ConstantInt::get(i64, Module->getDataLayout().getTypeAllocSize(T));
    }
    
    std::cout << "Unhandled call to intrinsic: " << name << std::endl;
    LKFatalError("");
}




llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::UnaryExpr> UnaryExpr) {
    EmitDebugLocation(UnaryExpr);
    
    auto Expr = UnaryExpr->Expr;
    
    switch (UnaryExpr->Op) {
        case ast::UnaryExpr::Operation::Negate:
            return Builder.CreateNeg(Codegen(Expr));
        
        case ast::UnaryExpr::Operation::BitwiseNot:
            return Builder.CreateNot(Codegen(Expr));
        
        case ast::UnaryExpr::Operation::LogicalNegation: {
            auto T = GuessType(Expr);
            LKAssert(T->Equals(TypeInfo::Bool) || T->IsPointer() || T->IsIntegerType());
            return Builder.CreateIsNull(Codegen(Expr)); // TODO this seems like a cop-out answer?
        }
    }
}






#pragma mark - Control Flow

llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::IfStmt> If) {
    EmitDebugLocation(If);
    
    using BK = ast::IfStmt::Branch::BranchKind;
    
    auto F = Builder.GetInsertBlock()->getParent();
    auto MergeBB = llvm::BasicBlock::Create(C, "merge");
    bool NeedsMergeBB = false;
    
    // The entry points to each branch's condition
    // Note that if the last branch is a conditionless else branch, this points directly to the branch body
    std::vector<llvm::BasicBlock *> BranchConditionBlocks(1, nullptr);
    std::vector<llvm::BasicBlock *> BranchBodyBlocks;
    
    for (auto &Branch : If->Branches) {
        BranchBodyBlocks.push_back(llvm::BasicBlock::Create(C, "if_body"));
        if (Branch->Kind != BK::Else) {
            BranchConditionBlocks.push_back(llvm::BasicBlock::Create(C, "if_cond"));
        }
    }
    
    if (If->Branches.back()->Kind == BK::Else) {
        BranchConditionBlocks.back() = BranchBodyBlocks.back();
    } else {
        NeedsMergeBB = true;
        BranchConditionBlocks.back() = MergeBB;
    }
    
    
    for (size_t I = 0; I < If->Branches.size(); I++) {
        if (If->Branches[I]->Kind == BK::Else) break;
        if (I > 0) {
            auto BB = BranchConditionBlocks[I];
            F->getBasicBlockList().push_back(BB);
            Builder.SetInsertPoint(BB);
        }
        auto CondV = Codegen(If->Branches[I]->Condition);
        Builder.CreateCondBr(CondV, BranchBodyBlocks[I], BranchConditionBlocks[I + 1]);
    }
    
    
    for (size_t I = 0; I < If->Branches.size(); I++) {
        auto BB = BranchBodyBlocks[I];
        F->getBasicBlockList().push_back(BB);
        Builder.SetInsertPoint(BB);
        
        Codegen(If->Branches[I]->Body);
        if (!Builder.GetInsertBlock()->back().isTerminator()) {
            NeedsMergeBB = true;
            Builder.CreateBr(MergeBB);
        }
    }
    
    if (NeedsMergeBB) {
        F->getBasicBlockList().push_back(MergeBB);
        Builder.SetInsertPoint(MergeBB);
    }
    
    return nullptr;
}



llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::WhileStmt> WhileStmt) {
    EmitDebugLocation(WhileStmt);
    
    // TODO what if there;s a return statement in the body!
    auto F = Builder.GetInsertBlock()->getParent();
    
    // TODO add unique ids to the branch names!, add the current function name?
    auto CondBB = llvm::BasicBlock::Create(C, "while_cond");
    auto BodyBB = llvm::BasicBlock::Create(C, "while_body");
    auto MergeBB = llvm::BasicBlock::Create(C, "while_merge");
    
    F->getBasicBlockList().push_back(CondBB);
    Builder.CreateBr(CondBB);
    Builder.SetInsertPoint(CondBB);
    
    Builder.CreateCondBr(Codegen(WhileStmt->Condition), BodyBB, MergeBB);
    
    F->getBasicBlockList().push_back(BodyBB);
    Builder.SetInsertPoint(BodyBB);
    Codegen(WhileStmt->Body);
    Builder.CreateBr(CondBB);
    
    F->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    
    return nullptr;
}


// TODO move to utils!
template <typename T>
void vec_append(std::vector<T> &dest, const std::vector<T> &src) {
    dest.insert(dest.end(), src.begin(), src.end());
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::ForLoop> forLoop) {
    EmitDebugLocation(forLoop);
    
    // TODO this is disgusting
    
    auto T = GuessType(forLoop->expr);
    LKAssert(T->IsPointer() && T->getPointee()->IsComplex());
    auto iteratorCallTarget = mangling::MangleCanonicalName(T->getPointee()->getName(), "iterator", ast::FunctionSignature::FunctionKind::InstanceMethod);
    
    auto call = std::make_shared<ast::CallExpr>(std::make_shared<ast::Identifier>(iteratorCallTarget),
                                                std::vector<std::shared_ptr<ast::Expr>>{ forLoop->expr });
    
    
    auto forStmtScope = std::make_shared<ast::Composite>();
    auto it_ident = std::make_shared<ast::Identifier>("$it");
    forStmtScope->Statements.push_back(std::make_shared<ast::VariableDecl>(it_ident, TypeInfo::Unresolved, call));
    
    // while loop
    auto callInstanceMethod = [](const std::shared_ptr<ast::Identifier> &target, const std::string &methodName) {
        return std::make_shared<ast::CallExpr>(std::make_shared<ast::MemberExpr>(target, methodName));
    };
    
    auto whileBody = std::make_shared<ast::Composite>();
    whileBody->Statements.push_back(std::make_shared<ast::VariableDecl>(forLoop->ident, TypeInfo::Unresolved, callInstanceMethod(it_ident, "next")));
    vec_append(whileBody->Statements, forLoop->body->Statements);
    forStmtScope->Statements.push_back(std::make_shared<ast::WhileStmt>(callInstanceMethod(it_ident, "hasNext"), whileBody));
    return Codegen(forStmtScope);
}



#pragma mark - Comparisons/Conditions

llvm::CmpInst::Predicate GetMatchingLLVMCmpInstPredicateForComparisonOperator_Float(ast::Comparison::Operation Op) {
    using Operation = ast::Comparison::Operation;
    using Predicate = llvm::CmpInst::Predicate;
    
    switch (Op) {
        case Operation::EQ: return Predicate::FCMP_OEQ;
        case Operation::NE: return Predicate::FCMP_ONE;
        case Operation::LT: return Predicate::FCMP_OLT;
        case Operation::LE: return Predicate::FCMP_OLE;
        case Operation::GT: return Predicate::FCMP_OGT;
        case Operation::GE: return Predicate::FCMP_OGE;
    }
}

llvm::CmpInst::Predicate GetMatchingLLVMCmpInstPredicateForComparisonOperator_Int(ast::Comparison::Operation Op, bool Signed) {
    using Operation = ast::Comparison::Operation;
    using Predicate = llvm::CmpInst::Predicate;
    
    switch (Op) {
        case Operation::EQ: return Predicate::ICMP_EQ;
        case Operation::NE: return Predicate::ICMP_NE;
        case Operation::LT: return Signed ? Predicate::ICMP_SLT : Predicate::ICMP_ULT;
        case Operation::LE: return Signed ? Predicate::ICMP_SLE : Predicate::ICMP_ULE;
        case Operation::GT: return Signed ? Predicate::ICMP_SGT : Predicate::ICMP_UGT;
        case Operation::GE: return Signed ? Predicate::ICMP_SGE : Predicate::ICMP_UGE;
    }
}



llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Comparison> Comparison) {
    EmitDebugLocation(Comparison);
    
    auto LhsTy = GuessType(Comparison->LHS);
    auto RhsTy = GuessType(Comparison->RHS);
    
    llvm::CmpInst::Predicate Pred;
    llvm::Value *LHS, *RHS;
    
    // Floats?
    if (LhsTy->Equals(TypeInfo::Double) && RhsTy->Equals(TypeInfo::Double)) {
        return Builder.CreateFCmp(GetMatchingLLVMCmpInstPredicateForComparisonOperator_Float(Comparison->Op),
                                  Codegen(Comparison->LHS), Codegen(Comparison->RHS));
    }
    
    // Are both integers?
    if (!LhsTy->IsIntegerType() || !RhsTy->IsIntegerType()) {
        LKFatalError("Cannot compare unrelated types '%s' and '%s'", LhsTy->Str().c_str(), RhsTy->Str().c_str());
    }
    
    if (LhsTy->Equals(RhsTy)) {
        Pred = GetMatchingLLVMCmpInstPredicateForComparisonOperator_Int(Comparison->Op, LhsTy->IsSigned());
        LHS = Codegen(Comparison->LHS);
        RHS = Codegen(Comparison->RHS);
    } else {
        // Both are integers, but different types
        
        TypeInfo *CastDestTy = TypeInfo::Unresolved;
        auto LargerSize = std::max(LhsTy->getSize(), RhsTy->getSize());
        
        if (LargerSize <= TypeInfo::kSizeof_i32) {
            CastDestTy = TypeInfo::i32;
        } else {
            LKAssert(LargerSize == TypeInfo::kSizeof_i64);
            CastDestTy = TypeInfo::i64;
        }
        
        LHS = Codegen(std::make_shared<ast::Typecast>(Comparison->LHS, CastDestTy, ast::Typecast::CastKind::StaticCast));
        RHS = Codegen(std::make_shared<ast::Typecast>(Comparison->RHS, CastDestTy, ast::Typecast::CastKind::StaticCast));
        
        Pred = GetMatchingLLVMCmpInstPredicateForComparisonOperator_Int(Comparison->Op, LhsTy->IsSigned() || RhsTy->IsSigned());
    }
    
    return Builder.CreateICmp(Pred, LHS, RHS);
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::LogicalOperation> LogOp) {
    EmitDebugLocation(LogOp);
    
    
    // TODO rewrite
    auto LHS = Codegen(LogOp->LHS);
    auto RHS = Codegen(LogOp->RHS);
    
    LKAssert(LHS->getType() == Bool && RHS->getType() == Bool);
    
    auto Op = LogOp->Op == ast::LogicalOperation::Operation::And
    ? llvm::Instruction::BinaryOps::And
    : llvm::Instruction::BinaryOps::Or;
    
    return Builder.CreateBinOp(Op, LHS, RHS);
}





#pragma mark - Types


llvm::Type *IRGenerator::GetLLVMType(TypeInfo *TI) {
    if (TI->IsTemplatedType()) {
        LKFatalError("TODO");
        return GetLLVMType(InstantiateTemplatedType(TI));
    }
    
    if (auto T = TI->getLLVMType()) return T;

    switch (TI->getKind()) {
        case TypeInfo::Kind::Primitive: {
#define HANDLE(name, _llvmtype) if (TI->Equals(TypeInfo::name)) { TI->setLLVMType(_llvmtype); return _llvmtype; }
            HANDLE(i8, i8)      HANDLE(u8, i8)
            HANDLE(i16, i16)    HANDLE(u16, i16)
            HANDLE(i32, i32)    HANDLE(u32, i32)
            HANDLE(i64, i64)    HANDLE(u64, i64)
            HANDLE(Bool, Bool)
            HANDLE(Double, Double)
            HANDLE(Void, Void)
#undef HANDLE
            LKFatalError("Unhandled primitive type");
        }
        
        case TypeInfo::Kind::Pointer: {
            auto num_indirections = 0;
            auto TI_temp = TI;
            while (TI_temp->getKind() == TypeInfo::Kind::Pointer) {
                num_indirections += 1;
                TI_temp = TI_temp->getPointee();
            }
            auto Type = GetLLVMType(TI_temp);
            
            while (num_indirections--) {
                Type = Type->getPointerTo();
            }
            TI->setLLVMType(Type);
            return Type;
        }
        
        case TypeInfo::Kind::Complex: {
            auto Name = TI->getName();
            LKAssert(TypeCache.Contains(Name));
            
            auto LLVMStructType = llvm::StructType::create(C, Name);
            
            std::vector<llvm::Type *> Types;
            for (auto &Member : TypeCache.GetStruct(Name)->Members) {
                Types.push_back(GetLLVMType(Member->Type));
            }
            
            LLVMStructType->setBody(Types);
            TI->setLLVMType(LLVMStructType);
            return LLVMStructType;
        }
        
        case TypeInfo::Kind::Function: {
            auto functionTypeInfo = TI->getFunctionTypeInfo();
            LKAssert(functionTypeInfo.callingConvention == TypeInfo::FunctionTypeInfo::CallingConvention::C);
            auto paramTypes = util::vector::map(functionTypeInfo.parameterTypes, [this] (auto TI) { return GetLLVMType(TI); });
            auto llvmFnTy = llvm::FunctionType::get(GetLLVMType(functionTypeInfo.returnType), paramTypes, false); // TODO support variadic function types?
            auto T = llvmFnTy->getPointerTo();
            TI->setLLVMType(T);
            return T;
        }
        
        case TypeInfo::Kind::Typealias:
            return GetLLVMType(TI->getPointee());
        
        case TypeInfo::Kind::Unresolved:
            LKFatalError("unresolved type '%s'", TI->getName().c_str());
        
        case TypeInfo::Kind::ComplexTemplated: {
            LKFatalError("should never reach here");
//            auto Name = mangling::MangleTemplatedComplexType(TI);
//            std::cout << Name << std::endl;
//
//            LKAssert(TypeCache.Contains(Name));
            
//            if (auto ResolvedTy = TypeCache.Get(Name)) {
//                return GetLLVMType(ResolvedTy);
//            } else {
//
//            }
//            throw;
        }
    }
    throw;
}




llvm::DIType *IRGenerator::GetDIType(TypeInfo *TI) {
    if (auto DIType = TI->getDIType()) return DIType;
    
    auto byteWidth = Module->getDataLayout().getTypeSizeInBits(i8);
    auto pointerWidth = Module->getDataLayout().getPointerSizeInBits();
    
    
    if (TI->IsVoidType()) {
        return nullptr;
    }
    
    if (TI->IsIntegerType()) {
        auto Ty = DIBuilder.createBasicType(TI->getName(), TI->getSize() * byteWidth,
                                            TI->IsSigned() ? llvm::dwarf::DW_ATE_signed : llvm::dwarf::DW_ATE_unsigned);
        TI->setDIType(Ty);
        return Ty;
    }
    
    if (TI->IsPointer()) {
        auto Ty = DIBuilder.createPointerType(GetDIType(TI->getPointee()), pointerWidth);
        TI->setDIType(Ty);
        return Ty;
    }
    
    if (TI->IsPrimitive()) {
        llvm::DIType *Ty;
        if (TI->Equals(TypeInfo::Bool)) {
            Ty = DIBuilder.createBasicType("bool", byteWidth, llvm::dwarf::DW_ATE_boolean);
        } else {
            goto fail;
        }
        
        TI->setDIType(Ty);
        return Ty;
    }
    
    if (TI->IsComplex()) {
        auto &Name = TI->getName();
        auto StructDecl = TypeCache.GetStruct(Name);
        auto &SL = StructDecl->getSourceLocation();
        
        auto DeclUnit = _DIFileForNode(DIBuilder, StructDecl);
        
        auto &DataLayout = Module->getDataLayout();
        
        auto LLVMTy = llvm::dyn_cast<llvm::StructType>(GetLLVMType(TI));
        auto StructLayout = DataLayout.getStructLayout(LLVMTy);
        
        std::vector<llvm::Metadata *> Elements;
        for (size_t I = 0; I < StructDecl->Members.size(); I++) {
            auto &Member = StructDecl->Members[I];
            auto LLVMMemberTy = GetLLVMType(Member->Type);
            auto MemberTy = DIBuilder.createMemberType(CompileUnit, Member->Name->Value, DeclUnit,
                                                       Member->getSourceLocation().Line,
                                                       DataLayout.getTypeSizeInBits(LLVMMemberTy),
                                                       DataLayout.getPrefTypeAlignment(LLVMMemberTy),
                                                       StructLayout->getElementOffsetInBits(I),
                                                       llvm::DINode::DIFlags::FlagZero, GetDIType(Member->Type));
            Elements.push_back(MemberTy);
        }
        
        auto Ty = DIBuilder.createStructType(CompileUnit, Name,
                                             _DIFileForNode(DIBuilder, StructDecl), SL.Line,
                                             Module->getDataLayout().getTypeSizeInBits(LLVMTy),
                                             Module->getDataLayout().getPrefTypeAlignment(LLVMTy),
                                             llvm::DINode::DIFlags::FlagZero, nullptr, DIBuilder.getOrCreateArray(Elements));
        TI->setDIType(Ty);
        return Ty;
    }
    
    if (TI->IsFunction()) {
        auto &FTI = TI->getFunctionTypeInfo();
        
        std::vector<llvm::Metadata *> ParameterTypes;
        ParameterTypes.push_back(GetDIType(FTI.returnType));
        for (auto &ParamTy : FTI.parameterTypes) {
            ParameterTypes.push_back(GetDIType(ParamTy));
        }
        
        auto Ty = DIBuilder.createPointerType(DIBuilder.createSubroutineType(DIBuilder.getOrCreateTypeArray(ParameterTypes)), pointerWidth);
        TI->setDIType(Ty);
        return Ty;
    }
    
fail:
    LKFatalError("TODO: Create DIType for '%s'", TI->Str().c_str());
    throw;
}


// TODO what does this do / why is it here / is it still needed?
bool IRGenerator::IsTriviallyConvertible(TypeInfo *SrcType, TypeInfo *DestType) {
    if (SrcType->Equals(DestType)) return true;
    
    if (SrcType->IsIntegerType() && DestType->IsIntegerType()) {
        throw;
    }
    
    return false;
}


bool IRGenerator::ValueIsTriviallyConvertibleTo(std::shared_ptr<ast::NumberLiteral> Number, TypeInfo *TI) {
    using NT = ast::NumberLiteral::NumberType;
    
    // Allowed trivial conversions:
    // int literal to any int type (as long as the value fits)
    // int literal to double
    
    if (Number->Type == NT::Boolean) return TI->Equals(TypeInfo::Bool);
    
    if (TI->Equals(TypeInfo::Double)) {
        return Number->Type == NT::Double || Number->Type == NT::Integer;
    }
    
    LKAssert(Number->Type == NT::Integer && TI->IsIntegerType());
    LKAssert(Number->Value >= 0); // TODO whatthefuc? this will never be false since ast::NumberLitera::Value is unsigned!!!!!
    
    auto Value = Number->Value;
    uint8_t BitCount = 0;
    while (Value != 0) { ++BitCount; Value >>= 1; }
    
    return BitCount <= TI->getSize();
}




TypeInfo *IRGenerator::GuessType(std::shared_ptr<ast::Expr> Expr) {
#define IF(name, T) if (auto name = std::dynamic_pointer_cast<T>(Expr))
    
    IF(NumberLiteral, ast::NumberLiteral) {
        return GuessType(NumberLiteral);
    }
    
    IF(BinaryExpr, ast::BinaryOperation) {
        return GuessType(BinaryExpr->LHS); // todo should this check whether lhs and rhs have the same type?
    }
    
    IF(Ident, ast::Identifier) {
        return Scope.GetType(Ident->Value);
    }
    
    IF(Cast, ast::Typecast) {
        return Cast->DestType;
    }
    
    IF(StringLiteral, ast::StringLiteral) {
        switch (StringLiteral->Kind) {
            case ast::StringLiteral::StringLiteralKind::NormalString:
                return TypeInfo::GetWithName("String");
            case ast::StringLiteral::StringLiteralKind::ByteString:
                return TypeInfo::i8_ptr;
        }
    }
    
    IF(UnaryExpr, ast::UnaryExpr) {
        switch (UnaryExpr->Op) {
            case ast::UnaryExpr::Operation::Negate:
            case ast::UnaryExpr::Operation::BitwiseNot:
                return GuessType(UnaryExpr->Expr);
            case ast::UnaryExpr::Operation::LogicalNegation:
                return TypeInfo::Bool;
        }
    }
    
    IF(MatchExpr, ast::MatchExpr) {
        return GuessType(MatchExpr->Branches.front()->Expression); // TODO somehow ensure all branches return the same type
    }
    
    IF(RawLLVMValueExpr, ast::RawLLVMValueExpr) {
        return RawLLVMValueExpr->Type;
    }
    
    IF(memberExpr, ast::MemberExpr) {
        auto targetTy = GuessType(memberExpr->target);
        LKAssert(targetTy->IsPointer() && targetTy->getPointee()->IsComplex());
        return TypeCache.GetMember(targetTy->getPointee()->getName(), memberExpr->memberName).second;
    }
    
    IF(callExpr, ast::CallExpr) {
        return ResolveCall(callExpr, true).signature->ReturnType;
    }
    
    IF(subscript, ast::SubscriptExpr) {
        return GuessType(subscript->target)->getPointee();
    }
    
    IF(comp, ast::Comparison) {
        return TypeInfo::Bool;
    }
    
    LKFatalError("Unhandled node %s", util::typeinfo::GetTypename(*Expr).c_str());
    
#undef IF
}


TypeInfo *IRGenerator::GuessType(std::shared_ptr<ast::NumberLiteral> NumberLiteral) {
    using NT = ast::NumberLiteral::NumberType;
    
    switch (NumberLiteral->Type) {
        case NT::Integer:   return TypeInfo::i64;
        case NT::Boolean:   return TypeInfo::Bool;
        case NT::Double:    return TypeInfo::Double;
        case NT::Character: return TypeInfo::i8;
    }
}







TypeInfo *IRGenerator::InstantiateTemplatedType(TypeInfo *TI) {
    LKFatalError("TODO");
    
    if (!TI->IsTemplatedType()) return TI;
    
    auto TemplateStructDecl = TypeCache.GetStruct(TI->getName());
    LKAssert(TemplateStructDecl->IsTemplateStruct());
    std::map<std::string, TypeInfo *> Mapping;
    
    for (size_t I = 0; I < TemplateStructDecl->TemplateArguments.size(); I++) {
        Mapping[TemplateStructDecl->TemplateArguments[I]] = TI->getTemplateParameterTypes()[I];
    }
    
    auto MangledName = mangling::MangleTemplatedComplexType(TI);
    
    
    
    LKFatalError("TODO");
}










#pragma mark - Synthesized Functions


namespace astgen {
    using namespace ast;
    
    std::shared_ptr<Identifier> Ident(std::string Value) {
        return std::make_shared<Identifier>(Value);
    }
    
    std::vector<std::shared_ptr<Expr>> ExprVec(std::initializer_list<std::shared_ptr<Expr>> E) {
        return E;
    }
    
    std::shared_ptr<NumberLiteral> Number(uint64_t Value) {
        return std::make_shared<NumberLiteral>(Value, NumberLiteral::NumberType::Integer);
    }
    
    std::shared_ptr<Assignment> Assign(std::shared_ptr<Expr> Target, std::shared_ptr<Expr> Value) {
        return std::make_shared<Assignment>(Target, Value);
    }
    
    std::shared_ptr<Typecast> Cast(std::shared_ptr<Expr> Expr, TypeInfo *T) {
        return std::make_shared<Typecast>(Expr, T, Typecast::CastKind::StaticCast);
    }
}


llvm::Value *IRGenerator::GenerateStructInitializer(std::shared_ptr<ast::StructDecl> structDecl) {
    auto structName = structDecl->Name->Value;
    
    auto T = TypeInfo::MakePointer(TypeInfo::MakeComplex(structName));
    auto F = Functions[mangling::MangleCanonicalName(structName, "init", ast::FunctionSignature::FunctionKind::StaticMethod)][0].Decl;
    
    auto self = std::make_shared<ast::Identifier>("self");
    
    // allocate object
    {
        auto allocCall = std::make_shared<ast::CallExpr>(astgen::Ident("alloc"), astgen::ExprVec({
            //astgen::Number(M->getDataLayout().getTypeAllocSize(GetLLVMType(T)))
            astgen::Number(1)
        }));
        allocCall->explicitTemplateArgumentTypes = { T->getPointee() };
        F->Body->Statements.push_back(std::make_shared<ast::VariableDecl>(self, T, allocCall));
    }
    
    // set runtime metadata
    if (EnableARC && structDecl->attributes->arc) {
        auto set_retaincount = std::make_shared<ast::Assignment>(std::make_shared<ast::MemberExpr>(self, "retainCount"),
                                                                 astgen::Number((uint64_t(1) << 60) | 1));
        
        auto sel = mangling::MangleCanonicalName(structName, "dealloc", ast::FunctionSignature::FunctionKind::InstanceMethod);
        auto dealloc_fn = Functions[sel][0].LLVMFunction;
        
        llvm::Type *t[] = { i8_ptr };
        auto dealloc_fn_ty = llvm::FunctionType::get(Void, t, false)->getPointerTo();
        auto dealloc_fn_cast = Builder.CreateBitCast(dealloc_fn, dealloc_fn_ty);
        auto set_deallocFn = std::make_shared<ast::Assignment>(std::make_shared<ast::MemberExpr>(self, "deallocPtr"),
                                                               std::make_shared<ast::RawLLVMValueExpr>(dealloc_fn_cast, structDecl->Members[1]->Type));
        
        F->Body->Statements.push_back(set_retaincount);
        F->Body->Statements.push_back(set_deallocFn);
    }
    
    // set properties
    for (auto &param : F->Signature->Parameters) {
        F->Body->Statements.push_back(std::make_shared<ast::Assignment>(std::make_shared<ast::MemberExpr>(self, param->Name->Value), param->Name));
    }
    F->Body->Statements.push_back(std::make_shared<ast::ReturnStmt>(self));
    
    return Codegen(F);
}
