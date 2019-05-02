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

IRGenerator::IRGenerator(const std::string ModuleName) : Builder(C) {
    Module = llvm::make_unique<llvm::Module>(ModuleName, C);
    M = Module.get();
    
    i8  = llvm::Type::getInt8Ty(C);
    i16 = llvm::Type::getInt16Ty(C);
    i32 = llvm::Type::getInt32Ty(C);
    i64 = llvm::Type::getInt64Ty(C);
    
    i8_ptr = i8->getPointerTo();
    Void = llvm::Type::getVoidTy(C);
    i1 = Bool = llvm::Type::getInt1Ty(C);
    Double = llvm::Type::getDoubleTy(C);
}




void IRGenerator::Codegen(ast::AST &Ast) {
    Preflight(Ast);
    
    for (auto &Node : Ast) {
        Codegen(Node);
    }
}



template <typename T>
std::string MangleFullyResolved(T Function) {
    if (Function->attributes->no_mangle) {
        return Function->Signature->Name;
    }
    return mangling::MangleFullyResolvedNameForSignature(Function->Signature);
}

void IRGenerator::Preflight(ast::AST &Ast) {
    // Q: Why collect the different kinds of top level decls first and then process them, instead of simply processing them all in a single for loop?
    // A: What if a function uses a type that is declared at some later point, or in another module? it's important all of these are processed in the correct order
    std::vector<std::shared_ptr<ast::TypealiasDecl>>        Typealiases;
    std::vector<std::shared_ptr<ast::FunctionDecl>>         FunctionDecls;
    std::vector<std::shared_ptr<ast::StructDecl>>           StructDecls;
    std::vector<std::shared_ptr<ast::ImplBlock>>            ImplBlocks;
    std::vector<std::shared_ptr<ast::ExternFunctionDecl>>   ExternalFunctionDecls;
    
#define HANDLE(T, Dest) if (auto X = std::dynamic_pointer_cast<ast::T>(Node)) { Dest.push_back(X); continue; }
    
    for (auto &Node : Ast) {
        HANDLE(TypealiasDecl, Typealiases)
        HANDLE(FunctionDecl, FunctionDecls)
        HANDLE(ExternFunctionDecl, ExternalFunctionDecls)
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
    
    for (auto &EFD : ExternalFunctionDecls) {
        // No potential conflict w/ other non-external already resolved functions, since their resolved name will be mangled (unlike the external function's name)
        if (auto EFS2Sig = GetResolvedFunctionWithName(EFD->Signature->Name)) {
            precondition(EFD->Signature->ReturnType->equals(EFS2Sig->ReturnType)
                         && EFD->Signature->Parameters.size() == EFS2Sig->Parameters.size()
                         && std::equal(EFD->Signature->Parameters.begin(), EFD->Signature->Parameters.end(), EFS2Sig->Parameters.begin(),
                                       [] (auto Arg1, auto Arg2) -> bool {
                                           return Arg1->Type->equals(Arg2->Type);
                                       })
                         && "Multiple external function declarations w/ same name, but different signatures"
                         );
            continue;
        }
        RegisterFunction(EFD);
    }
    
    for (auto &FunctionDecl : FunctionDecls) {
        RegisterFunction(FunctionDecl);
    }
    
    for (auto &ImplBlock : ImplBlocks) {
        RegisterImplBlock(ImplBlock);
    }
}


void IRGenerator::RegisterFunction(std::shared_ptr<ast::FunctionDecl> Function) {
    if (Function->Signature->IsTemplateFunction && !Function->Signature->IsFullSpecialization()) {
        auto CanonicalName = mangling::MangleCanonicalNameForSignature(Function->Signature);
        Functions[CanonicalName].push_back(ResolvedFunction(Function, nullptr));
        return;
    }
    
    auto Signature = Function->Signature;
    std::vector<llvm::Type *> ParameterTypes;
    
    for (auto &P : Signature->Parameters) {
        ParameterTypes.push_back(GetLLVMType(P->Type));
    }
    
    std::string CanonicalName = mangling::MangleCanonicalNameForSignature(Signature);
    std::string ResolvedName = MangleFullyResolved(Function);
    
    precondition(M->getFunction(ResolvedName) == nullptr, util::fmt_cstr("Redefinition of function '%s'", ResolvedName.c_str())); // TODO print the signature instead!
    
    auto FT = llvm::FunctionType::get(GetLLVMType(Signature->ReturnType), ParameterTypes, Function->attributes->variadic);
    auto F = llvm::Function::Create(FT, llvm::Function::LinkageTypes::ExternalLinkage, ResolvedName, M);
    
    unsigned Idx = 0;
    for (auto &Arg : F->args()) {
        Arg.setName(Signature->Parameters[Idx++]->Name->Value);
    }
    
    ResolvedFunctions[ResolvedName] = Function->Signature;
    Functions[CanonicalName].push_back(ResolvedFunction(Function, F));
}


void IRGenerator::RegisterFunction(std::shared_ptr<ast::ExternFunctionDecl> Function) {
    auto Signature = Function->Signature;
    std::vector<llvm::Type *> ParameterTypes;
    
    for (auto &P : Signature->Parameters) {
        ParameterTypes.push_back(GetLLVMType(P->Type));
    }
    
    precondition(M->getFunction(Signature->Name) == nullptr);
    auto FT = llvm::FunctionType::get(GetLLVMType(Signature->ReturnType), ParameterTypes, Function->attributes->variadic);
    auto F = llvm::Function::Create(FT, llvm::Function::LinkageTypes::ExternalLinkage, Signature->Name, M);
    
    // we can't really append this to the `Functions` vector since that stores an ast::FunctionDecl, but external functions don't have a body :/
    // TODO: come up w/ a solution!
    //ExternalFunctions[Signature->Name] = Signature;
    ResolvedFunctions[Signature->Name] = Signature;
    
    auto Decl = std::make_shared<ast::FunctionDecl>();
    Decl->Signature = Function->Signature;
    Decl->attributes = Function->attributes;
    Functions[Signature->Name].push_back(ResolvedFunction(Decl, F));
}



std::shared_ptr<ast::FunctionSignature> IRGenerator::GetResolvedFunctionWithName(std::string &Name) {
    if (auto It = ResolvedFunctions.find(Name); It != ResolvedFunctions.end()) {
        return It->second;
    }
    return nullptr;
}


void IRGenerator::RegisterStructDecl(std::shared_ptr<ast::StructDecl> Struct) {
    auto Name = Struct->Name->Value;
    TypeCache.Insert(Name, TypeInfo::MakeComplex(Name));
    TypeCache.RegisterStruct(Name, Struct);
    
    // TODO forward-declare the struct's default initializer!
}



void IRGenerator::RegisterImplBlock(std::shared_ptr<ast::ImplBlock> ImplBlock) {
    using FK = ast::FunctionSignature::FunctionKind;
    
    auto Typename = ImplBlock->Typename;
    precondition(TypeCache.Contains(Typename));
    auto T = TypeInfo::MakeComplex(Typename);
    
    for (auto &F : ImplBlock->Methods) {
        precondition(!F->attributes->no_mangle && "invalid attribute for function in impl block: no_mangle");
        auto Kind = FK::StaticMethod;
        if (!F->Signature->Parameters.empty()) {
            auto First = F->Signature->Parameters[0];
            if (First->Name->Value == "self" && First->Type->equals(T)) {
                Kind = FK::InstanceMethod;
            }
        }
        F->Signature->Kind = Kind;
        F->Signature->ImplType = TypeCache.GetStruct(Typename);
        RegisterFunction(F);
    }
}



# pragma mark - Codegen


#define HANDLE(node, T, ...) \
if (auto X = std::dynamic_pointer_cast<ast::T>(node)) return Codegen(X, ## __VA_ARGS__);

#define IGNORE(node, T) \
if (std::dynamic_pointer_cast<ast::T>(node)) return nullptr;


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::TopLevelStmt> TLS) {
    HANDLE(TLS, FunctionDecl)
    IGNORE(TLS, ExternFunctionDecl)
    HANDLE(TLS, StructDecl)
    HANDLE(TLS, ImplBlock)
    IGNORE(TLS, TypealiasDecl)
    
    unhandled_node(TLS)
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::LocalStmt> LocalStmt) {
    HANDLE(LocalStmt, Composite)
    HANDLE(LocalStmt, FunctionCall)
    HANDLE(LocalStmt, VariableDecl)
    HANDLE(LocalStmt, IfStmt)
    HANDLE(LocalStmt, Assignment)
    HANDLE(LocalStmt, MemberAccess, CodegenReturnValueKind::Value);
    HANDLE(LocalStmt, WhileStmt)
    HANDLE(LocalStmt, ForLoop)
    
    unhandled_node(LocalStmt);
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Expr> Expr, CodegenReturnValueKind ReturnValueKind) {
    HANDLE(Expr, NumberLiteral)
    HANDLE(Expr, BinaryOperation)
    HANDLE(Expr, Identifier, ReturnValueKind)
    HANDLE(Expr, FunctionCall)
    HANDLE(Expr, Comparison)
    HANDLE(Expr, LogicalOperation)
    HANDLE(Expr, Typecast)
    HANDLE(Expr, MemberAccess, ReturnValueKind)
    HANDLE(Expr, StringLiteral)
    HANDLE(Expr, UnaryExpr)
    HANDLE(Expr, MatchExpr)
    HANDLE(Expr, RawLLVMValueExpr)
    
    unhandled_node(Expr)
}

#undef HANDLE
#undef IGNORE




#pragma mark - Top Level Statements

llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::FunctionDecl> FunctionDecl) {
    if (FunctionDecl->Signature->IsTemplateFunction && !FunctionDecl->Signature->IsFullSpecialization()) {
        return nullptr;
    }
    precondition(Scope.isEmpty());
    auto ResolvedName = MangleFullyResolved(FunctionDecl);
    
    auto F = M->getFunction(ResolvedName);
    if (!F) {
        LKFatalError("Unable to find function '%s'", ResolvedName.c_str());
    }
    
    for (auto I = 0; I < FunctionDecl->Signature->Parameters.size(); I++) {
        auto &Arg = F->arg_begin()[I];
        auto Binding = ValueBinding(&Arg, [&Arg]() {
            return &Arg;
        }, [=, &Arg](llvm::Value *V) {
            LKFatalError("Function arguments are read-only (%s in %s)", Arg.getName().str().c_str(), ResolvedName.c_str());
        });
        auto Param = FunctionDecl->Signature->Parameters[I];
        Scope.Insert(Param->Name->Value, Param->Type, Binding);
    }
    
    auto EntryBB = llvm::BasicBlock::Create(C, "entry", F);
    auto ReturnBB = llvm::BasicBlock::Create(C, "return");
    Builder.SetInsertPoint(EntryBB);
    
    llvm::Value *RetvalAlloca = nullptr;
    auto ReturnType = FunctionDecl->Signature->ReturnType;
    
    if (!ReturnType->isVoidType()) {
        RetvalAlloca = Builder.CreateAlloca(F->getFunctionType()->getReturnType());
        auto RetvalBinding = ValueBinding(RetvalAlloca, []() {
            LKFatalError("retval is write-only");
            return nullptr;
        }, [this, RetvalAlloca](llvm::Value *V) {
            Builder.CreateStore(V, RetvalAlloca);
        });
        Scope.Insert(kRetvalAllocaIdentifier, ReturnType, RetvalBinding);
    }
    
    CurrentFunction = FunctionState(FunctionDecl, F, ReturnBB, RetvalAlloca);
    
    Codegen(FunctionDecl->Body, true);
    
    F->getBasicBlockList().push_back(ReturnBB);
    Builder.SetInsertPoint(ReturnBB);
    
    if (ReturnType->isVoidType()) {
        Builder.CreateRetVoid();
    } else {
        Builder.CreateRet(Builder.CreateLoad(RetvalAlloca));
    }
    
    precondition(Scope.size() == FunctionDecl->Signature->Parameters.size() + static_cast<uint8_t>(!ReturnType->isVoidType()));
    
    for (auto &Entry : Scope.GetEntriesSinceMarker(0)) {
        //std::cout << std::get<0>(Entry) << std::endl;
        // TODO: release
        Scope.Remove(Entry.Ident);
    }
    
    CurrentFunction = FunctionState();
    return F;
}




llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::StructDecl> Struct) {
    GenerateStructInitializer(Struct);
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
        precondition(Decl->InitialValue);
        Type = GuessType(Decl->InitialValue);
        HasInferredType = true;
    }
    
    precondition(Type);
    auto Alloca = Builder.CreateAlloca(GetLLVMType(Type));
    Alloca->setName(Decl->Name->Value);
    
    auto Binding = ValueBinding(Alloca, [=] () {
        return Builder.CreateLoad(Alloca);
    }, [=] (llvm::Value *V) {
        precondition(V->getType() == Alloca->getType()->getPointerElementType());
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







llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Composite> Composite, bool IsFunctionBody) {
    auto M = Scope.GetMarker();
    
    bool DidReturn = false;
    
//    if (IsFunctionBody && Composite->Statements.empty()) {
//        // Empty body -> no return statement
//        auto F = Builder.GetInsertBlock()->getParent();
//        if (F->getReturnType() == Void) {
//            //Builder.CreateRetVoid();
//            Builder.CreateBr(CurrentFunction.ReturnBB);
//            DidReturn = true;
//        } else {
//            LKFatalError("Missing return statement in function '%s' w/ return type '%s'", F->getName().str().c_str(), util_llvm::to_string(F->getReturnType()).c_str());
//        }
//    }
    
    for (auto It = Composite->Statements.begin(); !DidReturn && It != Composite->Statements.end(); It++) {
        auto &Stmt = *It;
        if (auto ReturnStmt = std::dynamic_pointer_cast<ast::ReturnStmt>(Stmt)) {
            Codegen(ReturnStmt);
            DidReturn = true;
        } else {
            Codegen(Stmt);
//            if (IsFunctionBody && It + 1 == Composite->Statements.end()) {
//                // Reached the end of the composite w/out a return statement
//                auto F = Builder.GetInsertBlock()->getParent();
//                precondition(F->getReturnType() == Void, fmt_c("Function %s doesn't have a return statement", F->getName().str().c_str()));
//                Builder.CreateRetVoid();
//                DidReturn = true;
//            }
        }
    }
    
    for (auto &Entry : Scope.GetEntriesSinceMarker(M)) {
        Scope.Remove(Entry.Ident);
    }
    
//    if (DidReturn) {
//        Scope.Clear();
//    } else {
//        auto Entries = Scope.GetEntriesSinceMarker(M);
//        for (auto &E : Entries) {
//            Scope.Remove(std::get<0>(E));
//        }
//    }
    
    return nullptr;
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::ReturnStmt> ReturnStmt) {
    auto FName = Builder.GetInsertBlock()->getParent()->getName().str();
    auto F = ResolvedFunctions[FName];
    
    if (auto Expr = ReturnStmt->Expression) {
        TypeInfo *T;
        if (!TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(&Expr, F->ReturnType, &T)) {
            LKFatalError("Error: Can't return value of type '%s' from function '%s' returning '%s'", T->str().c_str(), FName.c_str(), F->ReturnType->str().c_str());
        }
        
        Codegen(std::make_shared<ast::Assignment>(std::make_shared<ast::Identifier>(kRetvalAllocaIdentifier), Expr));
        return Builder.CreateBr(CurrentFunction.ReturnBB);
    }
    
    precondition(F->ReturnType->equals(TypeInfo::Void));
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
    
    precondition(TI->isIntegerType());
    bool Signed = TI->isSigned();
    
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
    
    if (Type->equals(ExpectedType)) return true;
    
    // at this point, both are integers
    if (auto NumberLiteral = std::dynamic_pointer_cast<ast::NumberLiteral>(*Expr)) {
        precondition(ExpectedType->isIntegerType());
        precondition(IntegerLiteralFitsInType(NumberLiteral->Value, ExpectedType));
        
        *Expr = std::make_shared<ast::Typecast>(*Expr, ExpectedType, ast::Typecast::CastKind::StaticCast);
        return true;
    }
    
    std::cout << "input: " << Type->str() << ", expected: " << ExpectedType->str() << std::endl;
    throw;
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Assignment> Assignment) {
    // TODO should assignments return something?
    
    auto runTypechecksForStore = [&](std::shared_ptr<ast::Expr> expr, TypeInfo *expectedType) -> auto {
        TypeInfo *T;
        if (!this->TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(&expr, expectedType, &T)) {
            LKFatalError("unable to store value of type '%s' into '%s'", T->str().c_str(), expectedType->str().c_str());
        }
        return expr;
    };
    
    if (auto Ident = std::dynamic_pointer_cast<ast::Identifier>(Assignment->Target)) {
        auto Binding = Scope.GetBinding(Ident->Value);
        if (!Binding) throw;
        
        Binding->Write(Codegen(runTypechecksForStore(Assignment->Value, Scope.GetType(Ident->Value))));
        return nullptr;
    }
    
    if (auto MemberAccess = std::dynamic_pointer_cast<ast::MemberAccess>(Assignment->Target)) {
        // In this case, the member access needs to be an lvalue, and should return an address, instead of a dereferenced
        auto Dest = Codegen(MemberAccess, CodegenReturnValueKind::Address);
        precondition(Dest->getType()->isPointerTy());
        
        auto Expr = runTypechecksForStore(Assignment->Value, GuessType(MemberAccess));
        Builder.CreateStore(Codegen(Expr), Dest);
        return nullptr;
    }
    
    throw;
}








#pragma mark - Expressions


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::RawLLVMValueExpr> RawExpr) {
    return RawExpr->Value;
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::NumberLiteral> NumberLiteral) {
    using NT = ast::NumberLiteral::NumberType;
    
    switch (NumberLiteral->Type) {
        case NT::Boolean: {
            return llvm::ConstantInt::get(i1, NumberLiteral->Value);
        }
        case NT::Character: {
            precondition(IntegerLiteralFitsInType(NumberLiteral->Value, TypeInfo::i8));
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



llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::StringLiteral> StringLiteral) {
    using SLK = ast::StringLiteral::StringLiteralKind;
    
    switch (StringLiteral->Kind) {
        case SLK::ByteString:
            return Builder.CreateGlobalStringPtr(StringLiteral->Value);
        case SLK::NormalString: {
            precondition(TypeCache.Contains("String"));
            StringLiteral->Kind = SLK::ByteString;
            auto Target = mangling::MangleCanonicalName("String", "new", ast::FunctionSignature::FunctionKind::StaticMethod);
            auto Call = std::make_shared<ast::FunctionCall>(Target, std::vector<std::shared_ptr<ast::Expr>>(1, StringLiteral));
            return Codegen(Call);
        }
    }
}



// If TakeAddress is true, this returns a pointer to the identifier, instead of the value stored
llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Identifier> Ident, CodegenReturnValueKind ReturnValueKind) {
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
    auto SrcTy = GuessType(Cast->Expression);
    auto DestTy = Cast->DestType;
    
    if (SrcTy->equals(DestTy)) {
        return Codegen(Cast->Expression);
    }
    
    llvm::Instruction::CastOps Op;
    switch (Cast->Kind) {
        case ast::Typecast::CastKind::Bitcast: {
            precondition(M->getDataLayout().getTypeSizeInBits(GetLLVMType(SrcTy)) == M->getDataLayout().getTypeSizeInBits(GetLLVMType(DestTy)));
            if (SrcTy->isPointer() && DestTy->isIntegerType()) {
                Op = llvm::Instruction::CastOps::PtrToInt;
            } else if (SrcTy->isIntegerType() && DestTy->isPointer()) {
                Op = llvm::Instruction::CastOps::IntToPtr;
            } else {
                Op = llvm::Instruction::CastOps::BitCast;
            }
            break;
        }
        case ast::Typecast::CastKind::StaticCast: {
            if (SrcTy->isIntegerType() && DestTy->isIntegerType()) {
                auto SrcIntWidth  = GetLLVMType(SrcTy)->getIntegerBitWidth();
                auto DestIntWidth = GetLLVMType(DestTy)->getIntegerBitWidth();
                
                if (SrcIntWidth > DestIntWidth) {
                    // casting to a smaller type
                    Op = llvm::Instruction::CastOps::Trunc;
                } else {
                    // casting to a larger type
                    if (SrcTy->isSigned()) {
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




llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::MemberAccess> MemberAccess, CodegenReturnValueKind ReturnValueKind) {
    using MK = ast::MemberAccess::Member::MemberKind;
    
    // TODO: This is almost definitely wrong
    
    llvm::Value *V = nullptr;
    TypeInfo *CurrentType = nullptr;
    
    for (auto &Member : MemberAccess->Members) {
        switch (Member->Kind) {
            case MK::Initial_Identifier: {
                auto Ident = std::dynamic_pointer_cast<ast::Identifier>(Member->Value);
                V = Codegen(Ident);
                CurrentType = Scope.GetType(Ident->Value);
                break;
            }
            
            // TODO initial_staticCall and initial_functioncall seem to be ecaxtly the same. refactor into a single switch case?
            
            case MK::Initial_StaticCall: {
                std::shared_ptr<ast::FunctionSignature> SelectedOverload;
                V = Codegen(std::dynamic_pointer_cast<ast::FunctionCall>(Member->Value), 0, &SelectedOverload);
                CurrentType = SelectedOverload->ReturnType;
                break;
            }
            
            case MK::Initial_FunctionCall: {
                std::shared_ptr<ast::FunctionSignature> Signature;
                V = Codegen(std::dynamic_pointer_cast<ast::FunctionCall>(Member->Value), 0, &Signature);
                CurrentType = Signature->ReturnType;
                break;
            }
            
            case MK::OffsetRead: {
                precondition(CurrentType->isPointer());
                
                if (llvm::isa<llvm::GetElementPtrInst>(V)) {
                    V = Builder.CreateLoad(V);
                }
                
                // We need V to be a pointer
                precondition(V->getType()->isPointerTy());
                V = Builder.CreateGEP(V, Codegen(Member->Value));
                CurrentType = CurrentType->getPointee();
                break;
            }
            
            case MK::MemberFunctionCall: {
                precondition(CurrentType->isComplex());
                auto Call = std::dynamic_pointer_cast<ast::FunctionCall>(Member->Value);
                
                if (!mangling::IsCanonicalInstanceMethodName(Call->Target)) {
                    Call->Target = mangling::MangleCanonicalName(CurrentType->getName(), Call->Target, ast::FunctionSignature::FunctionKind::InstanceMethod);
                }
                
                std::shared_ptr<ast::FunctionSignature> SelectedOverload;
                
                // Call the function, inserting the implicit first argument
                auto CallInst = llvm::dyn_cast<llvm::CallInst>(Codegen(Call, kInstanceMethodCallArgumentOffset, &SelectedOverload));
                CallInst->setOperand(0, V);
                V = CallInst;
                
                CurrentType = SelectedOverload->ReturnType;
                break;
            }
            
            case MK::MemberAttributeRead: {
                precondition(CurrentType->isComplex() && TypeCache.Contains(CurrentType->getName()));
                auto MemberName = std::dynamic_pointer_cast<ast::Identifier>(Member->Value)->Value;
                auto StructName = CurrentType->getName();
                auto StructType = TypeCache.GetStruct(StructName);
                
                uint32_t Offset = 0;
                auto DidFindMember = false;
                for (auto &Member : StructType->Members) {
                    if (Member->Name->Value == MemberName) {
                        DidFindMember = true;
                        break;
                    }
                    Offset++;
                }
                
                if (!DidFindMember) {
                    std::cout << "Unable to find member '" << MemberName << "' in struct '" << StructName << std::endl;
                    throw;
                }
                CurrentType = StructType->Members[Offset]->Type;
                
                llvm::Value *Offsets[] = {
                    llvm::ConstantInt::get(i64, 0),
                    llvm::ConstantInt::get(i32, Offset)
                };
                
                if (llvm::isa<llvm::GetElementPtrInst>(V)) {
                    V = Builder.CreateLoad(V);
                }
        
                V = Builder.CreateGEP(V, Offsets);
                break;
            }
        }
    }
    
    switch (ReturnValueKind) {
        case CodegenReturnValueKind::Address:
            return V;
        case CodegenReturnValueKind::Value:
            if (V && llvm::isa<llvm::GetElementPtrInst>(V)) {
                return Builder.CreateLoad(V);
            } else {
                return V;
            }
    }
}


// TODO should this go in the control flow section?
llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::MatchExpr> MatchExpr) {
    // TODO turn this into a proper function
    auto IsValidPatternForType = [this](std::shared_ptr<ast::Expr> Expr, TypeInfo *TargetType) -> bool {
        // TODO:
        // a) only allow patterns that can be trivially matched and have no side effects
        
        if (std::dynamic_pointer_cast<ast::Identifier>(Expr) != nullptr) {
            return true;
        }
        
        //auto ExprType = GuessType(Expr);
        
        if (TargetType->isIntegerType()) {
            return std::dynamic_pointer_cast<ast::NumberLiteral>(Expr) != nullptr;
        } else {
            return false;
        }
    };
    
    
    
    precondition(!MatchExpr->Branches.empty());
    auto NumBranches = MatchExpr->Branches.size();
    
    auto F = CurrentFunction.LLVMFunction;
    auto T = GuessType(MatchExpr->Branches.front()->Expression);
    auto V = Codegen(MatchExpr->Target);
    
    std::vector<llvm::BasicBlock *> BranchConditionBlocks;
    std::vector<llvm::BasicBlock *> BranchBodyBlocks;
    std::vector<llvm::Value *> BranchValues;
    auto MergeBB = llvm::BasicBlock::Create(C, "match_merge");
    int64_t WildcardBranchIndex = -1;
    
    for (auto I = 0; I < NumBranches; I++) {
        BranchConditionBlocks.push_back(llvm::BasicBlock::Create(C));
        BranchBodyBlocks.push_back(llvm::BasicBlock::Create(C));
    }
    
    
    // Handle branch patterns
    
    for (auto I = 0; I < NumBranches; I++) {
        if (I > 0) {
            auto BB = BranchConditionBlocks[I];
            F->getBasicBlockList().push_back(BB);
            Builder.SetInsertPoint(BB);
        }
        auto &Branch = MatchExpr->Branches[I];
        precondition(!Branch->Patterns.empty() && "match expr branch pattern cannot be empty");
        
        llvm::Value *Cond;
        bool IsWildcardPattern = false;
        
        for (auto I = 0; I < Branch->Patterns.size(); I++) {
            auto Pattern = Branch->Patterns[I];
            precondition(IsValidPatternForType(Pattern, GuessType(MatchExpr->Target)));
            
            if (auto Ident = std::dynamic_pointer_cast<ast::Identifier>(Pattern)) {
                precondition(Branch->Patterns.size() == 1 && "wildcards can only appear in single-pattern branches");
                Cond = llvm::ConstantInt::getTrue(C);
                IsWildcardPattern = true;
            } else {
                llvm::Value *Comp = Codegen(std::make_shared<ast::Comparison>(ast::Comparison::Operation::EQ,
                                                                              std::make_shared<ast::RawLLVMValueExpr>(V, T),
                                                                              Pattern));
                if (I == 0) {
                    Cond = Comp;
                } else {
                    Cond = Builder.CreateBinOp(llvm::Instruction::BinaryOps::Or, Cond, Comp);
                }
            }
        }
        
        if (IsWildcardPattern) {
            WildcardBranchIndex = I;
            Builder.CreateBr(BranchBodyBlocks[I]);
            break;
        } else {
            Builder.CreateCondBr(Cond, BranchBodyBlocks[I], BranchConditionBlocks[I + 1]);
        }
    }
    
    precondition(WildcardBranchIndex >= 0);
    
    
    // Handle branch expressions
    
    for (auto I = 0; I <= WildcardBranchIndex; I++) {
        auto BB = BranchBodyBlocks[I];
        F->getBasicBlockList().push_back(BB);
        Builder.SetInsertPoint(BB);
        
        // TODO assign wildcard ident to local variable for scope of expression?
        BranchValues.push_back(Codegen(MatchExpr->Branches[I]->Expression));
        Builder.CreateBr(MergeBB);
        BranchBodyBlocks[I] = Builder.GetInsertBlock();
    }
    
    F->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    
    auto PHI = Builder.CreatePHI(GetLLVMType(T), MatchExpr->Branches.size());
    for (auto I = 0; I <= WildcardBranchIndex; I++) {
        PHI->addIncoming(BranchValues[I], BranchBodyBlocks[I]);
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
    precondition(LhsTy_out && RhsTy_out);
    
    auto LhsTy = GuessType(*Lhs);
    auto RhsTy = GuessType(*Rhs);
    
    *LhsTy_out = LhsTy;
    *RhsTy_out = RhsTy;
    
    if (LhsTy->equals(RhsTy)) {
        return true;
    }
    
    // TODO add some kind of "types are compatible for this kind of binary operation" check
    
    if (!LhsTy->isIntegerType() || !RhsTy->isIntegerType()) {
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
    auto Lhs = Binop->LHS;
    auto Rhs = Binop->RHS;
    TypeInfo *LhsTy, *RhsTy;
    
    if (!TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(&Lhs, &Rhs, &LhsTy, &RhsTy)) {
        LKFatalError("unable to create binop for supplied operand types '%s' and '%s'", LhsTy->str().c_str(), RhsTy->str().c_str());
    }
    
    precondition(LhsTy->equals(RhsTy));
    return Builder.CreateBinOp(GetLLVMBinaryOpInstruction_Int(Binop->Op, LhsTy->isSigned()), Codegen(Lhs), Codegen(Rhs));
}



std::optional<std::map<std::string, TypeInfo *>> IRGenerator::AttemptToResolveTemplateArgumentTypesForCall(std::shared_ptr<ast::FunctionDecl> TemplateFunction, std::shared_ptr<ast::FunctionCall> Call, unsigned ArgumentOffset) {
    auto Sig = TemplateFunction->Signature;
    
    if (Sig->Parameters.size() != Call->Arguments.size()) {
        return std::nullopt;
    }
    
    std::map<std::string, TypeInfo *> TemplateArgumentMapping;
    
    for (auto Idx = 0; Idx < Sig->TemplateArgumentNames.size(); Idx++) {
        auto Name = Sig->TemplateArgumentNames[Idx];
        
        if (Idx < Call->ExplicitTemplateArgumentTypes.size()) {
            TemplateArgumentMapping[Name] = Call->ExplicitTemplateArgumentTypes[Idx];
        } else {
            TemplateArgumentMapping[Name] = TypeInfo::Unresolved;
        }
    }
    
    for (auto Idx = ArgumentOffset; Idx < Call->Arguments.size(); Idx++) {
        std::string ParamTypename;
        auto ParamType = Sig->Parameters[Idx]->Type;
        unsigned ParamIndirectionCount = 0;
        
        if (ParamType->isPointer()) {
            auto TI = ParamType;
            while (TI->isPointer()) {
                ParamIndirectionCount += 1;
                TI = TI->getPointee();
            }
            ParamTypename = TI->getName();
        } else {
            ParamTypename = ParamType->getName();
        }
        
        if (auto Mapping = TemplateArgumentMapping.find(ParamTypename); Mapping != TemplateArgumentMapping.end()) {
            auto GuessedArgumentType = GuessType(Call->Arguments[Idx]);
            if (Mapping->second == TypeInfo::Unresolved) {
                while (ParamIndirectionCount-- > 0) {
                    precondition(GuessedArgumentType->isPointer());
                    GuessedArgumentType = GuessedArgumentType->getPointee();
                }
                Mapping->second = GuessedArgumentType;
            } else {
                precondition(Mapping->second->equals(GuessedArgumentType));
            }
        }
    }
    
    return TemplateArgumentMapping;
}



ResolvedFunction IRGenerator::InstantiateTemplateFunctionForCall(std::shared_ptr<ast::FunctionDecl> TemplateFunction, std::shared_ptr<ast::FunctionCall> Call, unsigned ArgumentOffset, std::map<std::string, TypeInfo *> TemplateArgumentMapping) {
    // Important here is that resolving the template arguments in one template instantiation doen't affect the template arguments in the template function
    // We have to make sure all objects/properties being mutated are copied!
    
    precondition(!TemplateArgumentMapping.empty());
    
#if 0
    for (auto &[Name, Type] : TemplateArgumentMapping) {
        std::cout << Name << ": " << Type->Str() << std::endl;
    }
#endif
    
    auto SpecializedFunction = std::make_shared<ast::FunctionDecl>();
    SpecializedFunction->Signature = std::make_shared<ast::FunctionSignature>(*TemplateFunction->Signature);
    SpecializedFunction->Body = TemplateFunction->Body; // TODO can we safely copy this pointer? // TODO: make sure function bodies are *never* mutated!!!!
    SpecializedFunction->attributes = TemplateFunction->attributes;
    
    for (auto Idx = ArgumentOffset; Idx < Call->Arguments.size(); Idx++) {
        auto ParameterDecl = std::make_shared<ast::VariableDecl>(*SpecializedFunction->Signature->Parameters[Idx]);
        SpecializedFunction->Signature->Parameters[Idx] = ParameterDecl;
        
        if (auto T = util::map::get_opt(TemplateArgumentMapping, ParameterDecl->Type->getName())) {
            precondition(*T != TypeInfo::Unresolved);
            ParameterDecl->Type = *T;
        }
    }
    
    if (auto T = util::map::get_opt(TemplateArgumentMapping, SpecializedFunction->Signature->ReturnType->getName())) {
        precondition(*T != TypeInfo::Unresolved); // TODO why is this check here? Why not also above when resolving parameter types?
        SpecializedFunction->Signature->ReturnType = *T;
    }
    
    
    for (auto &[Name, Type] : TemplateArgumentMapping) {
        if (Type != TypeInfo::Unresolved) {
            auto It = std::find(SpecializedFunction->Signature->TemplateArgumentNames.begin(),
                                SpecializedFunction->Signature->TemplateArgumentNames.end(), Name);
            SpecializedFunction->Signature->TemplateArgumentNames.erase(It);
        } else {
            LKFatalError("Unresolved argument type in template function: %s (%s)", Name.c_str(), mangling::MangleCanonicalNameForSignature(TemplateFunction->Signature).c_str());
        }
    }
    
#if 0
    for (auto &[N, T] : TemplateArgumentsMappings) {
        std::cout << N << ": " << T->Str() << std::endl;
    }
    std::cout << "TempSig: " << TemplateFunction->Signature << std::endl;
    std::cout << "SpecSig: " << SpecializedFunction->Signature << std::endl;
#endif
    
    llvm::Function *LLVMFunction;
    if (TemplateFunction->attributes->intrinsic) {
        LLVMFunction = nullptr;
    } else {
        RegisterFunction(SpecializedFunction);
        LLVMFunction = WithCleanSlate([&]() { return llvm::dyn_cast<llvm::Function>(Codegen(SpecializedFunction)); });
    }
    return ResolvedFunction(SpecializedFunction, LLVMFunction);
}





// Returns the function most closely matching the call
ResolvedFunction IRGenerator::ResolveCall(std::shared_ptr<ast::FunctionCall> Call, unsigned ArgumentOffset) {
    auto &PossibleTargets = Functions[Call->Target];
    precondition(!PossibleTargets.empty(), util::fmt_cstr("Unable to resolve call to %s", Call->Target.c_str()));
    
    if (PossibleTargets.size() == 1) {
        auto &Target = PossibleTargets[0];
        if (Target.Decl->Signature->IsTemplateFunction) {
            auto TemplateArgumentMapping = AttemptToResolveTemplateArgumentTypesForCall(Target.Decl, Call, ArgumentOffset);
            precondition(TemplateArgumentMapping.has_value());
            //return InstantiateTemplateFunctionForCall(Target.Decl, Call, ArgumentOffset, TemplateArgumentMapping.value());
            
            auto ResolvedDecl = TemplateResolver::SpecializeWithTemplateMapping(Target.Decl, TemplateArgumentMapping.value());
            llvm::Function *LLVMFunction;
            if (ResolvedDecl->attributes->intrinsic) {
                LLVMFunction = nullptr;
            } else {
                RegisterFunction(ResolvedDecl);
                LLVMFunction = WithCleanSlate([&]() { return llvm::dyn_cast<llvm::Function>(Codegen(ResolvedDecl)); });
            }
            return ResolvedFunction(ResolvedDecl, LLVMFunction);
        }
        return Target;
    }
    
    
    // More than one potential target
    
    struct FunctionResolutionMatchInfo {
        uint32_t Score;
        std::shared_ptr<ast::FunctionDecl> Decl;
        llvm::Function *LLVMFunction; // nullptr if this is a not yet instantiated template function
        std::map<std::string, TypeInfo *> TemplateArgumentMapping;
    };
    
    std::vector<std::shared_ptr<ast::FunctionDecl>> TemplateFunctionTargets; // List of uninstantiated template functions that might be potential targets
    std::vector<FunctionResolutionMatchInfo> Matches; // List of potential targets, with a score indicating how close a match they are
    bool HasPerfectMatch = false;
    
    
    for (auto &Target : PossibleTargets) {
        auto &Decl = Target.Decl;
        auto Signature = Decl->Signature;
        
        if (Signature->Parameters.size() != Call->Arguments.size()) {
            continue;
        }
        
        if (Signature->IsTemplateFunction && !Signature->IsFullSpecialization()) {
            TemplateFunctionTargets.push_back(Decl);
            continue;
        }
        
        uint32_t Score = 0;
        
        for (auto I = 0; I < Call->Arguments.size(); I++) {
            auto Arg = Call->Arguments[I];
            auto Type_Passed = GuessType(Arg);
            auto Type_Expected = Decl->Signature->Parameters[I]->Type;
            
            if (Type_Passed->equals(Type_Expected)) {
                Score += 10;
            } else if (auto NumberLiteral = std::dynamic_pointer_cast<ast::NumberLiteral>(Arg)) {
                precondition(NumberLiteral->Type == ast::NumberLiteral::NumberType::Integer);
                if (ValueIsTriviallyConvertibleTo(NumberLiteral, Type_Expected)) {
                    Score += 5;
                }
            }
        }
        
        Matches.push_back({Score, Decl, Target.LLVMFunction, {}});
        
        if (Score == Call->Arguments.size() * 10) {
            // TODO does this mean we might miss other ambigious functions?
            HasPerfectMatch = true;
            break;
        }
    }
    
    
    if (!HasPerfectMatch) {
        for (auto &Target : TemplateFunctionTargets) {
            if (auto TemplateArgMapping = AttemptToResolveTemplateArgumentTypesForCall(Target, Call, ArgumentOffset)) {
                auto argc = Target->Signature->Parameters.size();
                uint32_t Score = argc * 10;
                Matches.push_back({Score, Target, nullptr, TemplateArgMapping.value()});
            } else {
                std::cout << "(skipped bc unable to resolve): " << Target->Signature << std::endl;
            }
        }
    }
    
    
    // TODO this seems like a bad idea
    std::sort(Matches.begin(), Matches.end(), [](auto &Arg0, auto &Arg1) { return Arg0.Score > Arg1.Score; });
    
#if 0
    std::cout << "Matching overloads:\n";
    for (auto &Match : Matches) {
        std::cout << "- " << Match.Score << ": " << Match.Decl->Signature << std::endl;
    }
#endif
    
    if (Matches.size() > 1 && Matches[0].Score == Matches[1].Score) {
        std::cout << "Error: ambiguous function call. unable to resolve. Potential candidates are:\n";
        for (auto &Match : Matches) {
            std::cout << "- " << Match.Score << ": " << Match.Decl->Signature << std::endl;
        }
        throw;
    }
    
    
    auto BestMatch = Matches.front();
    
    if (BestMatch.Decl->Signature->IsTemplateFunction && !BestMatch.LLVMFunction) {
        throw; // TODO use the new template resolver instead!!!
        return InstantiateTemplateFunctionForCall(BestMatch.Decl, Call, ArgumentOffset, BestMatch.TemplateArgumentMapping);
    }
    
    return ResolvedFunction(BestMatch.Decl, BestMatch.LLVMFunction);
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


// ArgumentOffset: indicates the number of skipped arguments at the start of the argumens list
llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::FunctionCall> Call, unsigned ArgumentOffset, std::shared_ptr<ast::FunctionSignature> *SelectedOverload) {
    llvm::Function *F;
    
    auto TargetName = Call->Target;
    auto ResolvedTarget = ResolveCall(Call, ArgumentOffset);
    
    if (SelectedOverload) {
        *SelectedOverload = ResolvedTarget.Decl->Signature;
    }
    
    // TODO:
    // - run argument type checks for intrinsics as well
    // - check that the number of supplied explicit template arguments does't exceed the total number of supplied template arguments
    
    if (!CallerCalleeSideEffectsCompatible(CurrentFunction.Decl->attributes->side_effects, ResolvedTarget.Decl->attributes->side_effects)) {
        LKFatalError("cannot call '%s' because side effects", TargetName.c_str());
    }
    
    if (ResolvedTarget.Decl->attributes->intrinsic) {
        return Codegen_HandleIntrinsic(ResolvedTarget.Decl->Signature, Call, ArgumentOffset);
    }
    
    F = ResolvedTarget.LLVMFunction;
    if (!F) {
        F = M->getFunction(TargetName);
    }
    if (!F) {
        LKFatalError("Unable to find function named '%s'", TargetName.c_str());
    }
    
    auto FT = F->getFunctionType();
    auto IsVariadic = FT->isVarArg();
    
    precondition(Call->Arguments.size() >= FT->getNumParams() - ArgumentOffset - IsVariadic);
    
    std::vector<llvm::Value *> Args(ArgumentOffset, nullptr);
    auto NumFixedArgs = FT->getNumParams() - ArgumentOffset;

    for (auto I = ArgumentOffset; I < FT->getNumParams(); I++) {
        auto ExpectedType = ResolvedTarget.Decl->Signature->Parameters[I]->Type;
        auto Expr = Call->Arguments[I - ArgumentOffset];
        TypeInfo *T;
        if (!TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(&Expr, ExpectedType, &T)) {
            LKFatalError("Type mismatch in call to '%s'. Arg #%i: expected '%s', got '%s'", Call->Target.c_str(), I, ExpectedType->str().c_str(), T->str().c_str());
        }
        Args.push_back(Codegen(Expr));
    }
    
    if (IsVariadic && GetResolvedFunctionWithName(TargetName)) { // TargetName is unmangled
        for (auto It = Call->Arguments.begin() + NumFixedArgs; It != Call->Arguments.end(); It++) {
            Args.push_back(Codegen(*It));
        }
    } else if (IsVariadic) {
        throw; // TODO implement
    }
    
    return Builder.CreateCall(F, Args);
}



llvm::Value *IRGenerator::Codegen_HandleIntrinsic(std::shared_ptr<ast::FunctionSignature> Signature, std::shared_ptr<ast::FunctionCall> Call, unsigned ArgumentOffset) {
    auto Name = mangling::MangleCanonicalNameForSignature(Signature);
    
    if (Name == "static_cast" || Name == "reinterpret_cast") {
        // TODO somehow use the SrcTy, if explicitly given?
        auto DstTy = Call->ExplicitTemplateArgumentTypes[0];
        auto Arg = Call->Arguments[0];
        auto CastKind = Name == "static_cast"
            ? ast::Typecast::CastKind::StaticCast
            : ast::Typecast::CastKind::Bitcast;
        return Codegen(std::make_shared<ast::Typecast>(Arg, DstTy, CastKind));
    }
    
    if (Name == "sizeof") {
        auto T = GetLLVMType(Call->ExplicitTemplateArgumentTypes[0]);
        return llvm::ConstantInt::get(i8, Module->getDataLayout().getTypeAllocSize(T));
    }
    
    std::cout << "Unhandled call to intrinsic: " << Name << std::endl;
    LKFatalError("");
}




llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::UnaryExpr> UnaryExpr) {
    auto Expr = UnaryExpr->Expr;
    
    switch (UnaryExpr->Op) {
        case ast::UnaryExpr::Operation::Negate:
            return Builder.CreateNeg(Codegen(Expr));
        
        case ast::UnaryExpr::Operation::BitwiseNot:
            return Builder.CreateNot(Codegen(Expr));
        
        case ast::UnaryExpr::Operation::LogicalNegation: {
            auto T = GuessType(Expr);
            precondition(T->equals(TypeInfo::Bool) || T->isPointer() || T->isIntegerType());
            return Builder.CreateIsNull(Codegen(Expr)); // TODO this seems like a cop-out answer?
        }
    }
}






#pragma mark - Control Flow

llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::IfStmt> If) {
    using BK = ast::IfStmt::Branch::BranchKind;
    
    auto F = Builder.GetInsertBlock()->getParent();
    auto InitialBB = Builder.GetInsertBlock();
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
        BranchConditionBlocks.push_back(MergeBB);
    }
    
    
    for (auto I = 0; I < If->Branches.size(); I++) {
        if (If->Branches[I]->Kind == BK::Else) break;
        if (I > 0) {
            auto BB = BranchConditionBlocks[I];
            F->getBasicBlockList().push_back(BB);
            Builder.SetInsertPoint(BB);
        }
        auto CondV = Codegen(If->Branches[I]->Condition);
        Builder.CreateCondBr(CondV, BranchBodyBlocks[I], BranchConditionBlocks[I + 1]);
    }
    
    
    for (auto I = 0; I < If->Branches.size(); I++) {
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



llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::ForLoop> forLoop) {
    auto T = GuessType(forLoop->expr);
    std::cout << T << std::endl;
    // TODO get iterator, turn that into a while loop or something like that
    
    auto iteratorCallTarget = mangling::MangleCanonicalName(T->getName(), "iterator", ast::FunctionSignature::FunctionKind::InstanceMethod);
    auto call = std::make_shared<ast::FunctionCall>(iteratorCallTarget, std::vector<std::shared_ptr<ast::Expr>>{forLoop->expr});
    
    throw;
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
    auto LhsTy = GuessType(Comparison->LHS);
    auto RhsTy = GuessType(Comparison->RHS);
    
    llvm::CmpInst::Predicate Pred;
    llvm::Value *LHS, *RHS;
    
    // Floats?
    if (LhsTy->equals(TypeInfo::Double) && RhsTy->equals(TypeInfo::Double)) {
        return Builder.CreateFCmp(GetMatchingLLVMCmpInstPredicateForComparisonOperator_Float(Comparison->Op),
                                  Codegen(Comparison->LHS), Codegen(Comparison->RHS));
    }
    
    // Are both integers?
    if (!LhsTy->isIntegerType() || !RhsTy->isIntegerType()) {
        LKFatalError("Cannot compare unrelated types '%s' and '%s'", LhsTy->str().c_str(), RhsTy->str().c_str());
    }
    
    if (LhsTy->equals(RhsTy)) {
        Pred = GetMatchingLLVMCmpInstPredicateForComparisonOperator_Int(Comparison->Op, LhsTy->isSigned());
        LHS = Codegen(Comparison->LHS);
        RHS = Codegen(Comparison->RHS);
    } else {
        // Both are integers, but different types
        
        TypeInfo *CastDestTy = TypeInfo::Unresolved;
        auto LargerSize = std::max(LhsTy->getSize(), RhsTy->getSize());
        
        if (LargerSize <= TypeInfo::kSizeof_i32) {
            CastDestTy = TypeInfo::i32;
        } else {
            precondition(LargerSize == TypeInfo::kSizeof_i64);
            CastDestTy = TypeInfo::i64;
        }
        
        LHS = Codegen(std::make_shared<ast::Typecast>(Comparison->LHS, CastDestTy, ast::Typecast::CastKind::StaticCast));
        RHS = Codegen(std::make_shared<ast::Typecast>(Comparison->RHS, CastDestTy, ast::Typecast::CastKind::StaticCast));
        
        Pred = GetMatchingLLVMCmpInstPredicateForComparisonOperator_Int(Comparison->Op, LhsTy->isSigned() || RhsTy->isSigned());
    }
    
    return Builder.CreateICmp(Pred, LHS, RHS);
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::LogicalOperation> LogOp) {
    // TODO rewrite
    auto LHS = Codegen(LogOp->LHS);
    auto RHS = Codegen(LogOp->RHS);
    
    precondition(LHS->getType() == Bool && RHS->getType() == Bool);
    
    auto Op = LogOp->Op == ast::LogicalOperation::Operation::And
    ? llvm::Instruction::BinaryOps::And
    : llvm::Instruction::BinaryOps::Or;
    
    return Builder.CreateBinOp(Op, LHS, RHS);
}





#pragma mark - Types


llvm::Type *IRGenerator::GetLLVMType(TypeInfo *TI) {
    if (auto T = TI->getLLVMType()) return T;

    switch (TI->getKind()) {
        case TypeInfo::Kind::Primitive: {
#define HANDLE(name, _llvmtype) if (TI->equals(TypeInfo::name)) { TI->setLLVMType(_llvmtype); return _llvmtype; }
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
            precondition(TypeCache.Contains(Name));
            
            auto LLVMStructType = llvm::StructType::create(C, Name);
            
            std::vector<llvm::Type *> Types;
            for (auto &Member : TypeCache.GetStruct(Name)->Members) {
                Types.push_back(GetLLVMType(Member->Type));
            }
            
            LLVMStructType->setBody(Types);
            auto T = LLVMStructType->getPointerTo();
            TI->setLLVMType(T);
            return T;
        }
        
        case TypeInfo::Kind::Function:
            throw;
        
        case TypeInfo::Kind::Typealias:
            return GetLLVMType(TI->getPointee());
        
        case TypeInfo::Kind::Unresolved:
            LKFatalError("unresolved type '%s'", TI->getName().c_str());
    }
    throw;
}



// TODO what does this do / why is it here / is it still needed?
bool IRGenerator::IsTriviallyConvertible(TypeInfo *SrcType, TypeInfo *DestType) {
    if (SrcType->equals(DestType)) return true;
    
    if (SrcType->isIntegerType() && DestType->isIntegerType()) {
        throw;
    }
    
    return false;
}


bool IRGenerator::ValueIsTriviallyConvertibleTo(std::shared_ptr<ast::NumberLiteral> Number, TypeInfo *TI) {
    using NT = ast::NumberLiteral::NumberType;
    
    // Allowed trivial conversions:
    // int literal to any int type (as long as the value fits)
    // int literal to double
    
    if (Number->Type == NT::Boolean) return TI->equals(TypeInfo::Bool);
    
    if (TI->equals(TypeInfo::Double)) {
        return Number->Type == NT::Double || Number->Type == NT::Integer;
    }
    
    precondition(Number->Type == NT::Integer && TI->isIntegerType());
    precondition(Number->Value > 0); // TODO whatthefuc? this will never be false since ast::NumberLitera::Value is unsigned!!!!!
    
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
    
    IF(MemberAccess, ast::MemberAccess) {
        return GuessType(MemberAccess);
    }
    
    IF(BinaryExpr, ast::BinaryOperation) {
        return GuessType(BinaryExpr->LHS); // todo should this check whether lhs and rhs have the same type?
    }
    
    IF(Ident, ast::Identifier) {
        return Scope.GetType(Ident->Value);
    }
    
    IF(Call, ast::FunctionCall) {
        return ResolveCall(Call, 0).Decl->Signature->ReturnType;
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



TypeInfo *IRGenerator::GuessType(std::shared_ptr<ast::MemberAccess> MemberAccess) {
    using MK = ast::MemberAccess::Member::MemberKind;
    TypeInfo *Type = TypeInfo::Unresolved;
    
    for (auto &Member : MemberAccess->Members) {
        switch (Member->Kind) {
            case MK::Initial_Identifier:
                Type = Scope.GetType(std::dynamic_pointer_cast<ast::Identifier>(Member->Value)->Value);
                break;
            
            // TODO these switch cases are identical?
            case MK::Initial_FunctionCall:
                Type = ResolveCall(std::dynamic_pointer_cast<ast::FunctionCall>(Member->Value), 0).Decl->Signature->ReturnType;
                break;
            
            case MK::Initial_StaticCall:
                Type = ResolveCall(std::dynamic_pointer_cast<ast::FunctionCall>(Member->Value), 0).Decl->Signature->ReturnType;
                break;
            
            case MK::OffsetRead:
                precondition(Type->isPointer());
                Type = Type->getPointee();
                break;
            
            case MK::MemberFunctionCall: {
                precondition(Type->isComplex() && TypeCache.Contains(Type->getName()));
                auto Call = std::dynamic_pointer_cast<ast::FunctionCall>(Member->Value);
                if (!mangling::IsCanonicalInstanceMethodName(Call->Target)) {
                    Call->Target = mangling::MangleCanonicalName(Type->getName(), Call->Target, ast::FunctionSignature::FunctionKind::InstanceMethod);
                }
                Type = ResolveCall(Call, kInstanceMethodCallArgumentOffset).Decl->Signature->ReturnType;
                break;
            }
            
            case MK::MemberAttributeRead:
                precondition(Type->isComplex());
                auto DidFind = false;
                auto MemberName = std::dynamic_pointer_cast<ast::Identifier>(Member->Value)->Value;
                for (auto &Member : TypeCache.GetStruct(Type->getName())->Members) {
                    if (Member->Name->Value == MemberName) {
                        Type = Member->Type;
                        DidFind = true;
                        break;
                    }
                }
                precondition(DidFind);
                break;
        }
    }
    
    return Type;
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
    
    std::shared_ptr<FunctionCall> Call(const std::string &Target, std::vector<std::shared_ptr<Expr>> Args) {
        return std::make_shared<FunctionCall>(Target, Args);
    }
    
    std::shared_ptr<Assignment> Assign(std::shared_ptr<Expr> Target, std::shared_ptr<Expr> Value) {
        return std::make_shared<Assignment>(Target, Value);
    }
    
    std::shared_ptr<Typecast> Cast(std::shared_ptr<Expr> Expr, TypeInfo *T) {
        return std::make_shared<Typecast>(Expr, T, Typecast::CastKind::StaticCast);
    }
}


llvm::Value *IRGenerator::GenerateStructInitializer(std::shared_ptr<ast::StructDecl> Struct) {
    auto Typename = Struct->Name->Value;
    
    auto T = TypeInfo::MakeComplex(Typename);
    auto F = std::make_shared<ast::FunctionDecl>();
    F->attributes = std::make_shared<yo::attributes::FunctionAttributes>();
    F->Signature = std::make_shared<ast::FunctionSignature>();
    F->Signature->Name = "init";
    F->Signature->Kind = ast::FunctionSignature::FunctionKind::StaticMethod;
    F->Signature->Parameters = Struct->Members;
    F->Signature->ReturnType = T;
    F->Signature->ImplType = Struct;
    F->Body = std::make_shared<ast::Composite>();
    
    auto self = astgen::Ident("self");
    
    {
        // Allocate Self
        F->Body->Statements.push_back(std::make_shared<ast::VariableDecl>(self, T));
        
        auto Malloc = astgen::Call("malloc", astgen::ExprVec({astgen::Number(M->getDataLayout().getTypeAllocSize(GetLLVMType(T)))}));
        
        auto X = std::make_shared<ast::Typecast>(Malloc, T, ast::Typecast::CastKind::Bitcast);
        F->Body->Statements.push_back(astgen::Assign(self, X));
        
    }
    
    // Set Attributes
    for (auto &Member : Struct->Members) {
        auto M1 = std::make_shared<ast::MemberAccess::Member>(ast::MemberAccess::Member::MemberKind::Initial_Identifier, self);
        auto M2 = std::make_shared<ast::MemberAccess::Member>(ast::MemberAccess::Member::MemberKind::MemberAttributeRead, Member->Name);
        
        auto M = std::make_shared<ast::MemberAccess>(std::vector<std::shared_ptr<ast::MemberAccess::Member>>({M1, M2}));
        F->Body->Statements.push_back(std::make_shared<ast::Assignment>(M, Member->Name));
        
    }
    
    // Return
    F->Body->Statements.push_back(std::make_shared<ast::ReturnStmt>(self));
    
    RegisterFunction(F);
    return Codegen(F);
}
