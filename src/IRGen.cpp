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
{ std::cout << __PRETTY_FUNCTION__ << ": Unhandled Node: " << util::typeinfo::getTypename(*(node)) << std::endl; \
throw; }


// IRGen

llvm::LLVMContext IRGenerator::C;

IRGenerator::IRGenerator(const std::string moduleName)
    : module(llvm::make_unique<llvm::Module>(moduleName, C)),
    M(module.get()),
    builder(C),
    debugInfo{llvm::DIBuilder(*module), nullptr, {}}
{
    i8  = llvm::Type::getInt8Ty(C);
    i16 = llvm::Type::getInt16Ty(C);
    i32 = llvm::Type::getInt32Ty(C);
    i64 = llvm::Type::getInt64Ty(C);
    
    i8_ptr = i8->getPointerTo();
    Void = llvm::Type::getVoidTy(C);
    i1 = Bool = llvm::Type::getInt1Ty(C);
    Double = llvm::Type::getDoubleTy(C);
    
    debugInfo.compileUnit = debugInfo.builder.createCompileUnit(llvm::dwarf::DW_LANG_C,
                                                        debugInfo.builder.createFile("main.yo", "."),
                                                        "yo", false, "", 0);
    debugInfo.lexicalBlocks.push_back(debugInfo.compileUnit);
    
    module->addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
}



void IRGenerator::emitDebugLocation(const std::shared_ptr<ast::Node> &node) {
    if (!node) {
        builder.SetCurrentDebugLocation(llvm::DebugLoc());
        return;
    }
    
    auto &SL = node->getSourceLocation();
    builder.SetCurrentDebugLocation(llvm::DebugLoc::get(SL.line, SL.column, debugInfo.lexicalBlocks.back()));
}




void IRGenerator::codegen(ast::AST &ast) {
    preflight(ast);
    
    verifyDeclarations();
    
    for (auto &node : ast) {
        codegen(node);
    }
    
    debugInfo.builder.finalize();
}



std::string mangleFullyResolved(const std::shared_ptr<ast::FunctionSignature> &signature) {
    if (signature->attributes->no_mangle) {
        return signature->name;
    } else if (!signature->attributes->mangledName.empty()) {
        return signature->attributes->mangledName;
    }
    return mangling::mangleFullyResolvedNameForSignature(signature);
}

void IRGenerator::preflight(ast::AST &ast) {
    // Q: Why collect the different kinds of top level decls first and then process them, instead of simply processing them all in a single for loop?
    // A: What if a function uses a type that is declared at some later point, or in another module? it's important all of these are processed in the correct order
    std::vector<std::shared_ptr<ast::TypealiasDecl>>        typealiases;
    std::vector<std::shared_ptr<ast::FunctionDecl>>         functionDecls;
    std::vector<std::shared_ptr<ast::StructDecl>>           structDecls;
    std::vector<std::shared_ptr<ast::ImplBlock>>            implBlocks;
    
#define HANDLE(T, dest) if (auto X = std::dynamic_pointer_cast<ast::T>(node)) { dest.push_back(X); continue; }
    
    for (auto &node : ast) {
        HANDLE(TypealiasDecl, typealiases)
        HANDLE(FunctionDecl, functionDecls)
        HANDLE(StructDecl, structDecls)
        HANDLE(ImplBlock, implBlocks)
        
        unhandled_node(node)
    }
    
#undef HANDLE
    
    for (auto &typealias : typealiases) {
        // TODO is this a good idea?
        // TODO prevent circular aliases!
        typeCache.insert(typealias->typename_, TypeInfo::makeTypealias(typealias->typename_, typealias->type));
    }
    
    for (auto &structDecl : structDecls) {
        registerStructDecl(structDecl);
    }
    
    for (auto &functionDecl : functionDecls) {
        if (functionDecl->signature->attributes->extern_) {
            functionDecl->signature->attributes->no_mangle = true;
        }
        registerFunction(functionDecl);
    }
    
    for (auto &implBlock : implBlocks) {
        registerImplBlock(implBlock);
    }
}


void IRGenerator::registerFunction(std::shared_ptr<ast::FunctionDecl> functionDecl) {
    auto signature = functionDecl->signature;
    
    if (signature->name == "main") {
        signature->attributes->no_mangle = true;
        // TODO run some checks to make sure main fulfills the requirements (correct return & parameter types, no other attributes, etc)
//        LKAssert(Signature->ReturnType->Equals(TypeInfo::i32));
//        LKAssert(Signature->parameters.size() == 0 || Signature->parameters.size() == 2);
//        if (Signature->parameters.size() == 2) {
//            LKAssert(Signature->parameters[0]->type->Equals(TypeInfo::))
//        }
    }
    
    
    if (signature->isTemplateFunction && !signature->isFullSpecialization()) {
        auto canonicalName = mangling::mangleCanonicalNameForSignature(signature);
        functions[canonicalName].push_back(ResolvedFunction(functionDecl, nullptr));
        return;
    }
    
    //std::vector<llvm::Type *> parameterTypes;
    //
    //for (auto &P : signature->parameters) {
    //    parameterTypes.push_back(getLLVMType(P->type));
    //}
    std::vector<llvm::Type *> parameterTypes = util::vector::map(signature->parameters, [this](auto &param) { return getLLVMType(param->type); });
    
    std::string canonicalName, resolvedName;
    
    if (signature->attributes->extern_) {
        canonicalName = resolvedName = mangling::mangleCanonicalNameForSignature(signature);
    } else {
        canonicalName = mangling::mangleCanonicalNameForSignature(signature);
        resolvedName = mangleFullyResolved(signature);
    }
    
    
    if (auto otherSignature = getResolvedFunctionWithName(resolvedName)) {
        LKAssert(signature->attributes->extern_ && "only extern functions are allowed to have multiple declarations");
        LKAssert(signature->returnType->equals(otherSignature->returnType));
        LKAssert(signature->parameters.size() == otherSignature->parameters.size());
        LKAssert(*signature->attributes == *otherSignature->attributes);
        return;
    }
    
    LKAssertMsg(M->getFunction(resolvedName) == nullptr, util::fmt_cstr("Redefinition of function '%s'", resolvedName.c_str())); // TODO print the signature instead!
    
    auto FT = llvm::FunctionType::get(getLLVMType(signature->returnType), parameterTypes, signature->attributes->variadic);
    auto F = llvm::Function::Create(FT, llvm::Function::LinkageTypes::ExternalLinkage, resolvedName, M);
    
    resolvedFunctions[resolvedName] = signature;
    functions[canonicalName].push_back(ResolvedFunction(functionDecl, F));
}


std::shared_ptr<ast::FunctionSignature> IRGenerator::getResolvedFunctionWithName(const std::string &name) {
    if (auto it = resolvedFunctions.find(name); it != resolvedFunctions.end()) {
        return it->second;
    }
    return nullptr;
}


void IRGenerator::registerStructDecl(std::shared_ptr<ast::StructDecl> structDecl) {
    auto name = structDecl->name->value;
    auto T = TypeInfo::makeComplex(name);
    typeCache.insert(name, T);
    typeCache.registerStruct(name, structDecl);
    
    auto LKYOObjectBase = typeCache.getStruct("LKMetadataAccessor");
    
    if (enableARC && structDecl->attributes->arc) {
        structDecl->members.insert(structDecl->members.begin(), LKYOObjectBase->members.begin(), LKYOObjectBase->members.end());
    }
    
    // TODO forward-declare the struct's default initializer!
    
    
    if (!structDecl->attributes->no_init) {
        auto initializer = std::make_shared<ast::FunctionDecl>();
        initializer->signature = std::make_shared<ast::FunctionSignature>();
        initializer->signature->attributes = std::make_shared<attributes::FunctionAttributes>();
        initializer->body = std::make_shared<ast::Composite>();
        initializer->signature->name = "init";
        initializer->signature->kind = ast::FunctionSignature::FunctionKind::StaticMethod;
        initializer->signature->returnType = TypeInfo::makePointer(T);
        initializer->signature->implType = structDecl;
        
        initializer->signature->parameters = structDecl->members;
        
        if (enableARC && structDecl->attributes->arc) {
            auto &params = initializer->signature->parameters;
            params.erase(params.begin(), params.begin() + LKYOObjectBase->members.size());
        }
        
//        if (!structDecl->attributes->arc) {
//            initializer->signature->parameters = structDecl->Members;
//        } else {
//            initializer->signature->parameters = std::vector<std::shared_ptr<ast::VariableDecl>>(structDecl->Members.begin() + LKYOObjectBase->Members.size(),
//                                                                                                 structDecl->Members.end());
//        }
        
        registerFunction(initializer);
    }
}



void IRGenerator::registerImplBlock(std::shared_ptr<ast::ImplBlock> implBlock) {
    using FK = ast::FunctionSignature::FunctionKind;
    
    auto typename_ = implBlock->typename_;
    LKAssert(typeCache.contains(typename_));
    auto ty = TypeInfo::makeComplex(typename_);
    
    for (auto &fn : implBlock->methods) {
        LKAssert(!fn->signature->attributes->no_mangle && "invalid attribute for function in impl block: no_mangle");
        auto kind = FK::StaticMethod;
        if (!fn->signature->parameters.empty()) {
            auto first = fn->signature->parameters[0];
            if (first->name->value == "self" && first->type->equals(ty->getPointerTo())) { // TODO remove the T check, just look for self?
                kind = FK::InstanceMethod;
            }
        }
        fn->signature->kind = kind;
        fn->signature->implType = typeCache.getStruct(typename_);
        registerFunction(fn);
    }
}






#pragma mark - Decl Verification


void IRGenerator::verifyDeclarations() {
    // TODO
}




# pragma mark - Codegen


#define HANDLE(node, T, ...) \
if (auto X = std::dynamic_pointer_cast<ast::T>(node)) return codegen(X, ## __VA_ARGS__);

#define IGNORE(node, T) \
if (std::dynamic_pointer_cast<ast::T>(node)) return nullptr;


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::TopLevelStmt> TLS) {
    HANDLE(TLS, FunctionDecl)
    HANDLE(TLS, StructDecl)
    HANDLE(TLS, ImplBlock)
    IGNORE(TLS, TypealiasDecl)
    
    unhandled_node(TLS)
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::LocalStmt> localStmt) {
    HANDLE(localStmt, Composite)
    HANDLE(localStmt, VariableDecl)
    HANDLE(localStmt, IfStmt)
    HANDLE(localStmt, Assignment)
    HANDLE(localStmt, WhileStmt)
    HANDLE(localStmt, ForLoop)
    HANDLE(localStmt, ExprStmt)
    
    unhandled_node(localStmt);
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::Expr> expr, CodegenReturnValueKind returnValueKind) {
    HANDLE(expr, NumberLiteral)
    HANDLE(expr, BinaryOperation)
    HANDLE(expr, Identifier, returnValueKind)
    HANDLE(expr, Comparison)
    HANDLE(expr, LogicalOperation)
    HANDLE(expr, Typecast)
    HANDLE(expr, StringLiteral)
    HANDLE(expr, UnaryExpr)
    HANDLE(expr, MatchExpr)
    HANDLE(expr, RawLLVMValueExpr)
    HANDLE(expr, MemberExpr, returnValueKind)
    HANDLE(expr, SubscriptExpr, returnValueKind)
    HANDLE(expr, CallExpr)
    
    unhandled_node(expr)
}

#undef HANDLE
#undef IGNORE


llvm::DIFile *DIFileForNode(llvm::DIBuilder &DIBuilder, const std::shared_ptr<ast::Node> &node) {
    auto &sourceLoc = node->getSourceLocation();
    auto [directory, filename] = util::string::extractPathAndFilename(sourceLoc.filepath);
    return DIBuilder.createFile(filename, directory);
}



llvm::DISubroutineType *IRGenerator::_toDISubroutineType(ast::FunctionSignature *signature) {
    // Looking at [godbolt](https://godbolt.org/z/EKfzqi ), it seems like the first element should be the function's return type?
    
    std::vector<llvm::Metadata *> types;
    
    types.push_back(getDIType(signature->returnType));
    for (auto &param : signature->parameters) {
        types.push_back(getDIType(param->type));
    }
    
    return debugInfo.builder.createSubroutineType(debugInfo.builder.getOrCreateTypeArray(types));
}


#pragma mark - Top Level Statements

llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::FunctionDecl> functionDecl) {
    auto &signature = functionDecl->signature;
    
    if (signature->attributes->extern_) {
        return nullptr;
    }
    
    if (signature->isTemplateFunction && !signature->isFullSpecialization()) {
        return nullptr;
    }
    
    LKAssert(scope.isEmpty());
    auto resolvedName = mangleFullyResolved(signature);
    
    auto F = M->getFunction(resolvedName);
    if (!F) {
        LKFatalError("Unable to find function '%s'", resolvedName.c_str());
    }
    
    if (signature->attributes->inline_) {
        F->addFnAttr(llvm::Attribute::InlineHint);
    }
    if (signature->attributes->always_inline) {
        F->addFnAttr(llvm::Attribute::AlwaysInline);
    }
    
    
    auto unit = DIFileForNode(debugInfo.builder, signature);
    auto SP = debugInfo.builder.createFunction(unit, signature->name, resolvedName, unit,
                                       signature->getSourceLocation().line,
                                       _toDISubroutineType(signature.get()),
                                       signature->getSourceLocation().line,
                                       llvm::DINode::FlagZero,
                                       llvm::DISubprogram::DISPFlags::SPFlagDefinition);
    
    auto entryBB = llvm::BasicBlock::Create(C, "entry", F);
    auto returnBB = llvm::BasicBlock::Create(C, "return");
    builder.SetInsertPoint(entryBB);
    
    emitDebugLocation(nullptr);
    
    F->setSubprogram(SP);
    debugInfo.builder.finalizeSubprogram(SP);
    debugInfo.lexicalBlocks.push_back(SP);
    
    
    std::vector<llvm::AllocaInst *> paramAllocas;
    
    for (auto &param : signature->parameters) {
        auto alloca = builder.CreateAlloca(getLLVMType(param->type));
        auto &name = param->name->value;
        alloca->setName(name);
        scope.insert(name, param->type, ValueBinding(alloca, [=]() {
            return builder.CreateLoad(alloca);
        }, [=](llvm::Value *V) {
            LKFatalError("Function arguments are read-only (%s in %s)", name.c_str(), resolvedName.c_str());
        }));
        paramAllocas.push_back(alloca);
    }
    
    
    for (size_t i = 0; i < signature->parameters.size(); i++) {
        auto alloca = paramAllocas.at(i);
        builder.CreateStore(&F->arg_begin()[i], alloca);
        
        auto &param = signature->parameters.at(i);
        auto varInfo = debugInfo.builder.createParameterVariable(SP, alloca->getName(), i + 1, unit,
                                                         param->getSourceLocation().line,
                                                         getDIType(param->type));
        debugInfo.builder.insertDeclare(alloca, varInfo, debugInfo.builder.createExpression(),
                                llvm::DILocation::get(C, param->getSourceLocation().line, param->getSourceLocation().column, SP),
                                entryBB);
    }
    
    
    
    
    llvm::Value *retvalAlloca = nullptr;
    auto returnType = signature->returnType;
    
    if (!returnType->isVoidType()) {
        retvalAlloca = builder.CreateAlloca(F->getFunctionType()->getReturnType());
        auto retvalBinding = ValueBinding(retvalAlloca, []() {
            LKFatalError("retval is write-only");
            return nullptr;
        }, [this, retvalAlloca](llvm::Value *V) {
            builder.CreateStore(V, retvalAlloca);
        });
        scope.insert(kRetvalAllocaIdentifier, returnType, retvalBinding);
        
        
        // Create Debug Metadata
        auto D = debugInfo.builder.createAutoVariable(SP, kRetvalAllocaIdentifier,
                                              unit,
                                              signature->getSourceLocation().line,
                                              getDIType(returnType));
        debugInfo.builder.insertDeclare(retvalAlloca, D,
                                debugInfo.builder.createExpression(),
                                llvm::DebugLoc::get(signature->getSourceLocation().line, 0, SP),
                                entryBB);
    }
    
    currentFunction = FunctionState(functionDecl, F, returnBB, retvalAlloca);
    
    codegen(functionDecl->body);
    
    F->getBasicBlockList().push_back(returnBB);
    builder.SetInsertPoint(returnBB);
    
    if (returnType->isVoidType()) {
        builder.CreateRetVoid();
    } else {
        builder.CreateRet(builder.CreateLoad(retvalAlloca));
    }
    
    LKAssert(scope.size() == signature->parameters.size() + static_cast<uint8_t>(!returnType->isVoidType()));
    
    for (auto &entry : scope.getEntriesSinceMarker(0)) {
        //std::cout << std::get<0>(Entry) << std::endl;
        // TODO: release
        scope.remove(entry.ident);
    }
    
    currentFunction = FunctionState();
    debugInfo.lexicalBlocks.pop_back(); // TODO maybe add a check that the lexical blocks weren't somehow modified?
    return F;
}




llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::StructDecl> structDecl) {
    if (!structDecl->attributes->no_init) {
        generateStructInitializer(structDecl);
    }
    return nullptr;
}



llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::ImplBlock> implBlock) {
    for (auto &method : implBlock->methods) {
        codegen(method);
    }
    return nullptr;
}




#pragma mark - Local Statements


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::VariableDecl> varDecl) {
    TypeInfo *type = nullptr /*TI::Unreoslved*/;
    bool hasInferredType = false;
    
    if ((type = varDecl->type) == nullptr /*TI::Unreoslved*/) {
        // If no type is specified, there _has_ to be an initial value
        LKAssert(varDecl->initialValue);
        type = guessType(varDecl->initialValue);
        hasInferredType = true;
    }
    
    LKAssert(type);
    auto alloca = builder.CreateAlloca(getLLVMType(type));
    alloca->setName(varDecl->name->value);
    
    // Create Debug Metadata
    auto D = debugInfo.builder.createAutoVariable(currentFunction.llvmFunction->getSubprogram(),
                                          varDecl->name->value,
                                          debugInfo.lexicalBlocks.back()->getFile(),
                                          varDecl->getSourceLocation().line,
                                          getDIType(type));
    debugInfo.builder.insertDeclare(alloca, D,
                            debugInfo.builder.createExpression(),
                            llvm::DebugLoc::get(varDecl->getSourceLocation().line, 0, currentFunction.llvmFunction->getSubprogram()),
                            builder.GetInsertBlock());
    
    auto binding = ValueBinding(alloca, [=] () {
        return builder.CreateLoad(alloca);
    }, [=] (llvm::Value *V) {
        LKAssert(V->getType() == alloca->getType()->getPointerElementType());
        builder.CreateStore(V, alloca);
    });
    
    scope.insert(varDecl->name->value, type, binding);
    
    if (auto expr = varDecl->initialValue) {
        // Q: Why create and handle an assignment to set the initial value, instead of just calling Binding.Write?
        // A: The Assignment codegen also includes the trivial type transformations, whish we'd otherwise have to implement again in here
        codegen(std::make_shared<ast::Assignment>(varDecl->name, varDecl->initialValue));
    }
    
    return alloca;
}







llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::Composite> composite) {
    emitDebugLocation(composite);
    
    auto marker = scope.getMarker();
    bool didReturn = false;
    
    for (auto it = composite->statements.begin(); !didReturn && it != composite->statements.end(); it++) {
        auto &stmt = *it;
        if (auto returnStmt = std::dynamic_pointer_cast<ast::ReturnStmt>(stmt)) {
            codegen(returnStmt);
            didReturn = true;
        } else {
            codegen(stmt);
        }
    }
    
    for (auto &entry : scope.getEntriesSinceMarker(marker)) {
        scope.remove(entry.ident);
    }
    
    return nullptr;
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::ReturnStmt> returnStmt) {
    emitDebugLocation(returnStmt);
    
    auto FName = builder.GetInsertBlock()->getParent()->getName().str();
    auto F = resolvedFunctions[FName];
    
    if (auto expr = returnStmt->expression) {
        TypeInfo *T;
        if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&expr, F->returnType, &T)) {
            LKFatalError("Error: Can't return value of type '%s' from function '%s' returning '%s'", T->str().c_str(), FName.c_str(), F->returnType->str().c_str());
        }
        
        codegen(std::make_shared<ast::Assignment>(std::make_shared<ast::Identifier>(kRetvalAllocaIdentifier), expr));
        return builder.CreateBr(currentFunction.returnBB);
    }
    
    LKAssert(F->returnType->equals(TypeInfo::Void));
    //return builder.CreateRetVoid();
    return builder.CreateBr(currentFunction.returnBB);
}


template <typename T>
bool value_fits_in_type(uint64_t value) {
    auto Min = std::numeric_limits<T>::min();
    auto Max = std::numeric_limits<T>::max();
    return static_cast<T>(value) >= Min && static_cast<T>(value) <= Max;
}


bool IntegerLiteralFitsInType(uint64_t value, TypeInfo *TI) {
    #define CASE(sizetype, signed_t, unsigned_t) case TypeInfo::sizetype: return isSigned ? value_fits_in_type<signed_t>(value) : value_fits_in_type<unsigned_t>(value);
    
    LKAssert(TI->isIntegerType());
    bool isSigned = TI->isSigned();
    
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


bool IRGenerator::typecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *expr, TypeInfo *expectedType, TypeInfo **initialTypeOfExpr) {
    auto type = guessType(*expr);
    if (initialTypeOfExpr) *initialTypeOfExpr = type;
    
    if (type->equals(expectedType)) return true;
    
    // at this point, both are integers
    if (auto numberLiteral = std::dynamic_pointer_cast<ast::NumberLiteral>(*expr)) {
        LKAssert(expectedType->isIntegerType());
        LKAssert(IntegerLiteralFitsInType(numberLiteral->value, expectedType));
        
        *expr = std::make_shared<ast::Typecast>(*expr, expectedType, ast::Typecast::CastKind::StaticCast);
        return true;
    }
    
    std::cout << "input: " << type->str() << ", expected: " << expectedType->str() << std::endl;
    throw;
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::Assignment> assignment) {
    emitDebugLocation(assignment);
    // TODO should assignments return something?
    // TODO rewrite this so that it doesn't rely on GuessType for function calls!
    
    auto expr = assignment->value;
    auto destTy = guessType(assignment->target);
    
    TypeInfo *T;
    if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&expr, destTy, &T)) {
        LKFatalError("type mismatch: cannot assign '%s' to '%s'", T->str().c_str(), destTy->str().c_str());
    }
    
    auto target = codegen(assignment->target, CodegenReturnValueKind::Address);
    builder.CreateStore(codegen(expr, CodegenReturnValueKind::Value), target);
    
    return nullptr;
}








#pragma mark - Expressions


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::RawLLVMValueExpr> rawExpr) {
    return rawExpr->value;
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::ExprStmt> exprStmt) {
    emitDebugLocation(exprStmt->expr);
    return codegen(exprStmt->expr);
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::NumberLiteral> numberLiteral) {
    emitDebugLocation(numberLiteral);
    
    using NT = ast::NumberLiteral::NumberType;
    
    switch (numberLiteral->type) {
        case NT::Boolean: {
            return llvm::ConstantInt::get(i1, numberLiteral->value);
        }
        case NT::Character: {
            LKAssert(IntegerLiteralFitsInType(numberLiteral->value, TypeInfo::i8));
            return llvm::ConstantInt::get(i8, numberLiteral->value);
        }
        case NT::Integer: {
            return llvm::ConstantInt::get(llvm::Type::getInt64Ty(C), numberLiteral->value);
        }
        case NT::Double: {
            LKFatalError("TODO: implement");
        }
    }
}



llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::StringLiteral> stringLiteral) {
    using SLK = ast::StringLiteral::StringLiteralKind;
    
    emitDebugLocation(stringLiteral);

    switch (stringLiteral->kind) {
        case SLK::ByteString:
            return builder.CreateGlobalStringPtr(stringLiteral->value);
        case SLK::NormalString: {
            LKAssert(typeCache.contains("String"));
            stringLiteral->kind = SLK::ByteString;
            auto target = std::make_shared<ast::Identifier>(mangling::mangleCanonicalName("String", "new", ast::FunctionSignature::FunctionKind::StaticMethod));
            auto call = std::make_shared<ast::CallExpr>(target, std::vector<std::shared_ptr<ast::Expr>>(1, stringLiteral));
            return codegen(call);
        }
    }
}



// If TakeAddress is true, this returns a pointer to the identifier, instead of the value stored
llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::Identifier> ident, CodegenReturnValueKind returnValueKind) {
    emitDebugLocation(ident);
    
    if (auto binding = scope.getBinding(ident->value)) {
        switch (returnValueKind) {
            case CodegenReturnValueKind::Value:
                return binding->read();
            case CodegenReturnValueKind::Address:
                return const_cast<llvm::Value *>(binding->value);
        }
    }
    
    std::cout << "Unable to find identifier " << ident->value << std::endl;
    throw;
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::Typecast> cast) {
    emitDebugLocation(cast);
    
    auto srcTy = guessType(cast->expression);
    auto destTy = cast->destType;
    
    if (srcTy->equals(destTy)) {
        return codegen(cast->expression);
    }
    
    llvm::Instruction::CastOps op;
    switch (cast->kind) {
        case ast::Typecast::CastKind::Bitcast: {
            LKAssert(M->getDataLayout().getTypeSizeInBits(getLLVMType(srcTy)) == M->getDataLayout().getTypeSizeInBits(getLLVMType(destTy)));
            if (srcTy->isPointer() && destTy->isIntegerType()) {
                op = llvm::Instruction::CastOps::PtrToInt;
            } else if (srcTy->isIntegerType() && destTy->isPointer()) {
                op = llvm::Instruction::CastOps::IntToPtr;
            } else {
                op = llvm::Instruction::CastOps::BitCast;
            }
            break;
        }
        case ast::Typecast::CastKind::StaticCast: {
            if (srcTy->isIntegerType() && destTy->isIntegerType()) {
                auto srcIntWidth  = getLLVMType(srcTy)->getIntegerBitWidth();
                auto destIntWidth = getLLVMType(destTy)->getIntegerBitWidth();
                
                if (srcIntWidth > destIntWidth) {
                    // casting to a smaller type
                    op = llvm::Instruction::CastOps::Trunc;
                } else {
                    // casting to a larger type
                    if (srcTy->isSigned()) {
                        op = llvm::Instruction::CastOps::SExt;
                    } else {
                        op = llvm::Instruction::CastOps::ZExt;
                    }
                }
                break;
            }
            throw;
        }
    }
    
    return builder.CreateCast(op, codegen(cast->expression), getLLVMType(destTy));
}







llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::MemberExpr> memberExpr, CodegenReturnValueKind returnValueKind) {
    emitDebugLocation(memberExpr);
    
    auto targetTy = guessType(memberExpr->target);
    LKAssert(targetTy->isPointer() && targetTy->getPointee()->isComplex());
    
    auto [memberIndex, memberType] = typeCache.getMember(targetTy->getPointee()->getName(), memberExpr->memberName);
    
    
    llvm::Value *offsets[] = {
        llvm::ConstantInt::get(i64, 0),
        llvm::ConstantInt::get(i32, memberIndex),
    };
    
    auto V = builder.CreateGEP(codegen(memberExpr->target), offsets);
    
    switch (returnValueKind) {
        case CodegenReturnValueKind::Address:
            return V;
        case CodegenReturnValueKind::Value:
            return builder.CreateLoad(V);
    }
}




llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::SubscriptExpr> subscript, CodegenReturnValueKind returnValueKind) {
    emitDebugLocation(subscript);
    
    auto target = codegen(subscript->target, CodegenReturnValueKind::Value);
    LKAssert(target->getType()->isPointerTy());
    auto offset = codegen(subscript->offset, CodegenReturnValueKind::Value);
    LKAssert(offset->getType()->isIntegerTy());
    
    auto GEP = builder.CreateGEP(target, codegen(subscript->offset));
    
    switch (returnValueKind) {
        case CodegenReturnValueKind::Address:
            return GEP;
        case CodegenReturnValueKind::Value:
            return builder.CreateLoad(GEP);
    }
}




bool isValidMatchPatternForMatchedExprType(std::shared_ptr<ast::Expr> patternExpr, TypeInfo *matchedExprType) {
    // Only patterns that are trivially and can be matched w/out side effects are allowed
    // TODO add the side effect checking
    
    if (dynamic_cast<ast::Identifier *>(patternExpr.get())) {
        throw;
        return true;
    }
    
    if (matchedExprType->isIntegerType()) {
        return std::dynamic_pointer_cast<ast::NumberLiteral>(patternExpr) != nullptr;
    } else if (matchedExprType->equals(TypeInfo::Bool)) {
        auto numberExpr = std::dynamic_pointer_cast<ast::NumberLiteral>(patternExpr);
        return numberExpr && numberExpr->type == ast::NumberLiteral::NumberType::Boolean;
    } else {
        return false;
    }
}


llvm::Value *IRGenerator::codegen_HandleMatchPatternExpr(MatchExprPatternCodegenInfo info) {
    emitDebugLocation(info.patternExpr);
    
    auto TT = info.targetType;
    auto PE = info.patternExpr;
    auto PT = guessType(PE);
    
    if (TT->isIntegerType()) {
        if (auto numberLiteral = std::dynamic_pointer_cast<ast::NumberLiteral>(PE)) {
            if (valueIsTriviallyConvertibleTo(numberLiteral, TT)) {
                return codegen(std::make_shared<ast::Comparison>(ast::Comparison::Operation::EQ,
                                                                 std::make_shared<ast::RawLLVMValueExpr>(info.targetLLVMValue, TT),
                                                                 numberLiteral));
            }
        } else {
            LKFatalError("Incompatible Match types: cannot match %s against %s", TT->str().c_str(), PT->str().c_str());
        }
    }
    throw;
}



bool lastBranchIsWildcard(const std::shared_ptr<ast::MatchExpr> &matchExpr) {
    auto lastBranch = matchExpr->branches.back();
    if (lastBranch->patterns.size() > 1) return false;
    if (auto ident = std::dynamic_pointer_cast<ast::Identifier>(lastBranch->patterns[0])) {
        return ident->value == "_";
    }
    return false;
}



// TODO should this go in the control flow section?
llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::MatchExpr> matchExpr) {
    emitDebugLocation(matchExpr);
    
    // TODO require that match patterns cannot contain side effects? (this should go in _IsValidMatchPatternForMatchedExprType!)
    auto F = currentFunction.llvmFunction;
    auto matchedExprType = guessType(matchExpr->target);
    auto resultType = guessType(matchExpr->branches.front()->expression);
    auto matchTargetValue = codegen(matchExpr->target);
    
    
    std::map<llvm::BasicBlock *, llvm::Value *> branchMappings;
    
    auto mergeBB = llvm::BasicBlock::Create(C);
    auto nextCondBB = llvm::BasicBlock::Create(C);
    auto nextValueBB = llvm::BasicBlock::Create(C);
    
    // TODO get rid of this and just have the first condition be part of the BB containing the match expression
    builder.CreateBr(nextCondBB);
    
    auto lastBranchIsWildcard = ::lastBranchIsWildcard(matchExpr);
    
    for (size_t i = 0; i < matchExpr->branches.size(); i++) {
        auto &branch = matchExpr->branches[i];
        auto valueBB = nextValueBB;
        nextValueBB = llvm::BasicBlock::Create(C);
        
        bool isLastBranchBeforeWildcard = lastBranchIsWildcard && i + 2 == matchExpr->branches.size();
        
        for (auto it = branch->patterns.begin(); it != branch->patterns.end(); it++) {
            auto &patternExpr = *it;
            if (auto ident = std::dynamic_pointer_cast<ast::Identifier>(patternExpr)) {
                LKAssert(it + 1 == branch->patterns.end() && branch->patterns.size() == 1);
                LKAssert(ident->value == "_");
                break;
            } else {
                // Not a wildcard
                F->getBasicBlockList().push_back(nextCondBB);
                builder.SetInsertPoint(nextCondBB);
                nextCondBB = llvm::BasicBlock::Create(C);
                
                auto cond = codegen_HandleMatchPatternExpr({matchedExprType, matchExpr->target, matchTargetValue, patternExpr});
                // If we reach here and the pattern didn't match and the next pattern is a wildcard, go directly to the value branch
                builder.CreateCondBr(cond, valueBB,
                                     isLastBranchBeforeWildcard && it + 1 == branch->patterns.end() ? nextValueBB : nextCondBB);
            }
        }
        
        TypeInfo *_initialTy = nullptr;
        if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&branch->expression, resultType, &_initialTy)) {
            LKFatalError("Invalid match branch result value: Type %s not compatible with expected type %s",
                         _initialTy->str().c_str(), resultType->str().c_str());
        }
        
        F->getBasicBlockList().push_back(valueBB);
        builder.SetInsertPoint(valueBB);
        branchMappings[valueBB] = codegen(branch->expression);
        builder.CreateBr(mergeBB);
    }
    
    
    
    F->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);
    
    auto PHI = builder.CreatePHI(getLLVMType(resultType), branchMappings.size());
    for (auto [BB, V] : branchMappings) {
        PHI->addIncoming(V, BB);
    }
    
    return PHI;
}





// MARK: Binops




llvm::Instruction::BinaryOps getLLVMBinaryOpInstruction_Int(ast::BinaryOperation::Operation op, bool isSigned) {
    using Operation = ast::BinaryOperation::Operation;
    using BinaryOps = llvm::Instruction::BinaryOps;
    
    switch (op) {
        case Operation::Add: return BinaryOps::Add;
        case Operation::Sub: return BinaryOps::Sub;
        case Operation::Mul: return BinaryOps::Mul;
        case Operation::Div: return isSigned ? BinaryOps::SDiv : BinaryOps::UDiv;
        case Operation::Mod: return isSigned ? BinaryOps::SRem : BinaryOps::URem;
        case Operation::And: return BinaryOps::And;
        case Operation::Or:  return BinaryOps::Or;
        case Operation::Xor: return BinaryOps::And;
        case Operation::Shl: return BinaryOps::Shl;
        case Operation::Shr: return BinaryOps::LShr; // TODO (important) arithmetic or logical right shift?
    }
    
    llvm_unreachable("should never reach here");
}




llvm::Instruction::BinaryOps getLLVMBinaryOpInstruction_Double(ast::BinaryOperation::Operation op) {
    using Operation = ast::BinaryOperation::Operation;
    using BinaryOps = llvm::Instruction::BinaryOps;
    
    switch (op) {
        case Operation::Add: return BinaryOps::FAdd;
        case Operation::Sub: return BinaryOps::FSub;
        case Operation::Mul: return BinaryOps::FMul;
        case Operation::Div: return BinaryOps::FDiv;
        case Operation::Mod: return BinaryOps::FRem;
        default: llvm_unreachable("nope");
    }
}


bool IRGenerator::typecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *lhs, std::shared_ptr<ast::Expr> *rhs, TypeInfo **lhsTy_out, TypeInfo **rhsTy_out) {
    LKAssert(lhsTy_out && rhsTy_out);
    
    auto lhsTy = guessType(*lhs);
    auto rhsTy = guessType(*rhs);
    
    *lhsTy_out = lhsTy;
    *rhsTy_out = rhsTy;
    
    if (lhsTy->equals(rhsTy)) {
        return true;
    }
    
    // TODO add some kind of "types are compatible for this kind of binary operation" check
    
    if (!lhsTy->isIntegerType() || !rhsTy->isIntegerType()) {
        LKFatalError("oh no");
    }
    
    if (std::dynamic_pointer_cast<ast::NumberLiteral>(*lhs)) {
        // lhs is literal, cast to type of ths
        *lhs = std::make_shared<ast::Typecast>(*lhs, rhsTy, ast::Typecast::CastKind::StaticCast);
        *lhsTy_out = rhsTy;
    } else if (std::dynamic_pointer_cast<ast::NumberLiteral>(*rhs)) {
        // rhs is literal, cast to type of lhs
        *rhs = std::make_shared<ast::Typecast>(*rhs, lhsTy, ast::Typecast::CastKind::StaticCast);
        *rhsTy_out = lhsTy;
    } else {
        return false;
    }
    
    return true;
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::BinaryOperation> binop) {
    emitDebugLocation(binop);
    
    auto lhs = binop->lhs;
    auto rhs = binop->rhs;
    TypeInfo *lhsTy, *rhsTy;
    
    if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&lhs, &rhs, &lhsTy, &rhsTy)) {
        LKFatalError("unable to create binop for supplied operand types '%s' and '%s'", lhsTy->str().c_str(), rhsTy->str().c_str());
    }
    
    LKAssert(lhsTy->equals(rhsTy));
    return builder.CreateBinOp(getLLVMBinaryOpInstruction_Int(binop->op, lhsTy->isSigned()), codegen(lhs), codegen(rhs));
}



std::optional<std::map<std::string, TypeInfo *>> IRGenerator::attemptToResolveTemplateArgumentTypesForCall(std::shared_ptr<ast::FunctionDecl> templateFunction, std::shared_ptr<ast::CallExpr> call, unsigned argumentOffset) {
    auto sig = templateFunction->signature;
    
    if (sig->parameters.size() != call->arguments.size()) {
        return std::nullopt;
    }
    
    std::map<std::string, TypeInfo *> templateArgumentMapping;
    
    for (size_t idx = 0; idx < sig->templateArgumentNames.size(); idx++) {
        auto name = sig->templateArgumentNames[idx];
        if (idx < call->explicitTemplateArgumentTypes.size()) {
            templateArgumentMapping[name] = call->explicitTemplateArgumentTypes[idx];
        } else {
            templateArgumentMapping[name] = TypeInfo::Unresolved;
        }
    }
    
    for (size_t idx = argumentOffset; idx < call->arguments.size(); idx++) {
        std::string paramTypename;
        auto paramType = sig->parameters[idx]->type;
        unsigned paramIndirectionCount = 0;
        
        if (paramType->isPointer()) {
            auto TI = paramType;
            while (TI->isPointer()) {
                paramIndirectionCount += 1;
                TI = TI->getPointee();
            }
            paramTypename = TI->getName();
        } else {
            paramTypename = paramType->getName();
        }
        
        if (auto mapping = templateArgumentMapping.find(paramTypename); mapping != templateArgumentMapping.end()) {
            auto guessedArgumentType = guessType(call->arguments[idx]);
            if (mapping->second == TypeInfo::Unresolved) {
                while (paramIndirectionCount-- > 0) {
                    LKAssert(guessedArgumentType->isPointer());
                    guessedArgumentType = guessedArgumentType->getPointee();
                }
                mapping->second = guessedArgumentType;
            } else {
                LKAssert(mapping->second->equals(guessedArgumentType));
            }
        }
    }
    
    return templateArgumentMapping;
}



uint8_t getArgumentOffset(TypeInfo *ty) {
    switch (ty->getFunctionTypeInfo().callingConvention) {
        case yo::TypeInfo::FunctionTypeInfo::CallingConvention::C:
            return 0;
        case yo::TypeInfo::FunctionTypeInfo::CallingConvention::Yo:
            return 1;
    }
}


std::shared_ptr<ast::FunctionSignature> makeFunctionSignatureFromFunctionTypeInfo(TypeInfo *ty) {
    LKAssert(ty->isFunction());
    auto functionTyInfo = ty->getFunctionTypeInfo();
    
    auto sig = std::make_shared<ast::FunctionSignature>();
    sig->returnType = functionTyInfo.returnType;
    sig->parameters = util::vector::map(functionTyInfo.parameterTypes, [] (TypeInfo *paramTy) {
        return std::make_shared<ast::VariableDecl>(ast::Identifier::emptyIdent(), paramTy);
    });
    
    return sig;
}



NEW_ResolvedFunction IRGenerator::resolveCall(std::shared_ptr<ast::CallExpr> callExpr, bool omitCodegen) {
    std::string targetName;
    uint8_t argumentOffset = 0;
    
    if (auto ident = std::dynamic_pointer_cast<ast::Identifier>(callExpr->target)) {
        targetName = ident->value;
        
        if (scope.contains(targetName)) {
            auto ty = scope.getType(targetName);
            LKAssert(ty->isFunction() && "cannot call a non-function variable");
            if (omitCodegen) {
                return NEW_ResolvedFunction(makeFunctionSignatureFromFunctionTypeInfo(ty), nullptr, getArgumentOffset(ty));
            } else {
                return NEW_ResolvedFunction(makeFunctionSignatureFromFunctionTypeInfo(ty), codegen(ident), getArgumentOffset(ty));
            }
        }
        
    } else if (auto staticDeclRefExpr = std::dynamic_pointer_cast<ast::StaticDeclRefExpr>(callExpr->target)) {
        targetName = mangling::mangleCanonicalName(staticDeclRefExpr->typeName, staticDeclRefExpr->memberName, ast::FunctionSignature::FunctionKind::StaticMethod);
        
    } else if (auto memberExpr = std::dynamic_pointer_cast<ast::MemberExpr>(callExpr->target)) {
        // <memberExpr>()
        // two options:
        // - calling a method
        // - calling a property that happens to be a function
        
        auto targetTy = guessType(memberExpr->target);
        LKAssert(targetTy->isPointer() && targetTy->getPointee()->isComplex());
        auto structName = targetTy->getPointee()->getName();
        
        if (typeCache.structHasMember(structName, memberExpr->memberName)) {
            auto memberTy = typeCache.getMember(structName, memberExpr->memberName).second;
            LKAssert(memberTy->isFunction() && "cannot call a non-function struct member");
            // struct properties cannot be overloaded, simply return what we found
            if (omitCodegen) {
                return NEW_ResolvedFunction(makeFunctionSignatureFromFunctionTypeInfo(memberTy), nullptr, getArgumentOffset(memberTy));
            } else {
                return NEW_ResolvedFunction(makeFunctionSignatureFromFunctionTypeInfo(memberTy), codegen(memberExpr), getArgumentOffset(memberTy));
            }
            
        } else {
            targetName = mangling::mangleCanonicalName(structName, memberExpr->memberName, ast::FunctionSignature::FunctionKind::InstanceMethod);
            argumentOffset = kInstanceMethodCallArgumentOffset;
        }
    } else {
        throw;
    }
    
    auto specializeTemplateFunctionForCall = [this, argumentOffset, omitCodegen] (std::shared_ptr<ast::FunctionDecl> functionDecl, std::map<std::string, TypeInfo *> templateArgumentMapping) -> NEW_ResolvedFunction {
        auto specializedDecl = TemplateResolver::specializeWithTemplateMapping(functionDecl, templateArgumentMapping);
        llvm::Function *llvmFunction = nullptr;
        if (!omitCodegen && !specializedDecl->signature->attributes->intrinsic) {
            registerFunction(specializedDecl);
            llvmFunction = withCleanSlate([&]() { return llvm::dyn_cast<llvm::Function>(codegen(specializedDecl)); });
        }
        return NEW_ResolvedFunction(specializedDecl->signature, llvmFunction, argumentOffset);
    };
    
    
    
    
    auto &possibleTargets = functions[targetName];
    LKAssertMsg(!possibleTargets.empty(), util::fmt_cstr("Unable to resolve call to %s", targetName.c_str()));
    
    
    if (possibleTargets.size() == 1) {
        auto &target = possibleTargets[0];
        if (!target.decl->signature->isTemplateFunction) {
            return NEW_ResolvedFunction(target.decl->signature, target.llvmFunction, argumentOffset);
        }
        
        // is a template function
        
        auto templateArgumentMapping = attemptToResolveTemplateArgumentTypesForCall(target.decl, callExpr, argumentOffset);
        LKAssert(templateArgumentMapping.has_value());
        return specializeTemplateFunctionForCall(target.decl, templateArgumentMapping.value());
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
        auto &decl = target.decl;
        auto signature = decl->signature;
        
        if (signature->parameters.size() != callExpr->arguments.size()) {
            continue;
        }
        
        if (signature->isTemplateFunction && !signature->isFullSpecialization()) {
            templateFunctions.push_back(decl);
            continue;
        }
        
        uint32_t score = 0;
        
        for (size_t i = 0; i < callExpr->arguments.size(); i++) {
            auto arg = callExpr->arguments[i];
            auto argTy = guessType(arg);
            auto expectedTy = signature->parameters[i]->type;
            
            if (argTy->equals(expectedTy)) {
                score += 10;
            } else if (auto numberLiteral = std::dynamic_pointer_cast<ast::NumberLiteral>(arg)) {
                LKAssert(numberLiteral->type == ast::NumberLiteral::NumberType::Integer);
                if (valueIsTriviallyConvertibleTo(numberLiteral, expectedTy)) {
                    score += 5;
                }
            }
        }
        
        matches.push_back({score, decl, target.llvmFunction, {}});
        
        if (score == callExpr->arguments.size() * 10) {
            // TODO does this mean we might miss other ambigious functions?
            hasPerfectMatch = true;
            break;
        }
    }
    
    if (!hasPerfectMatch) {
        for (auto &target : templateFunctions) {
            if (auto templateArgMapping = attemptToResolveTemplateArgumentTypesForCall(target, callExpr, argumentOffset)) {
                auto argc = target->signature->parameters.size();
                uint32_t score = argc * 10;
                matches.push_back({score, target, nullptr, templateArgMapping.value()});
            } else {
//                std::cout << "(skipped bc unable to resolve): " << Target->signature << std::endl;
            }
        }
    }
    
    // TODO this seems like a bad idea
    std::sort(matches.begin(), matches.end(), [](auto &arg0, auto &arg1) { return arg0.score > arg1.score; });
    
#if 0
    std::cout << "Matching overloads:\n";
    for (auto &match : matches) {
        std::cout << "- " << match.score << ": " << match.decl->signature << std::endl;
    }
#endif
    
    if (matches.size() > 1 && matches[0].score == matches[1].score) {
        std::cout << "Error: ambiguous function call. unable to resolve. Potential candidates are:\n";
        for (auto &match : matches) {
            std::cout << "- " << match.score << ": " << match.decl->signature << std::endl;
        }
        throw;
    }
    
    auto bestMatch = matches.front();
    
    if (bestMatch.decl->signature->isTemplateFunction && !bestMatch.llvmFunction) {
        return specializeTemplateFunctionForCall(bestMatch.decl, bestMatch.templateArgumentMapping);
    }
    return NEW_ResolvedFunction(bestMatch.decl->signature, bestMatch.llvmFunction, argumentOffset);
}








bool callerCalleeSideEffectsCompatible(const std::vector<yo::attributes::SideEffect> &callerSideEffects,
                                       const std::vector<yo::attributes::SideEffect> &calleeSideEffects) {
    if (callerSideEffects.size() == 1 && callerSideEffects[0] == yo::attributes::SideEffect::Unknown) {
        return true;
    }
    
    for (auto &sideEffect : calleeSideEffects) {
        if (!util::vector::contains(callerSideEffects, sideEffect)) return false;
    }
    
    return true;
}




llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::CallExpr> call) {
    emitDebugLocation(call);
    
    auto resolvedTarget = resolveCall(call, false);
    
    // TODO:
    // - run argument type checks for intrinsics as well
    // - check that the number of supplied explicit template arguments does't exceed the total number of supplied template arguments
    
    if (!callerCalleeSideEffectsCompatible(currentFunction.decl->signature->attributes->side_effects, resolvedTarget.signature->attributes->side_effects)) {
        auto targetName = mangling::mangleCanonicalNameForSignature(resolvedTarget.signature);
        LKFatalError("cannot call '%s' because side effects", targetName.c_str());
    }
    
    
    for (size_t i = resolvedTarget.argumentOffset; i < resolvedTarget.signature->parameters.size(); i++) {
        auto expectedType = resolvedTarget.signature->parameters[i]->type;
        auto expr = call->arguments[i - resolvedTarget.argumentOffset];
        TypeInfo *T;
        if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&expr, expectedType, &T)) {
            LKFatalError("Type mismatch in call to '%s'. Arg #%zu: expected '%s', got '%s'",
                         mangleFullyResolved(resolvedTarget.signature).c_str(),
                         i, expectedType->str().c_str(), T->str().c_str());
        }
        // TODO is modifying the arguments in-place necessarily a good idea?
        call->arguments[i] = expr;
    }
    
    if (resolvedTarget.signature->attributes->intrinsic) {
        return codegen_HandleIntrinsic(resolvedTarget.signature, call);
    }
    
    
    
    llvm::Value *llvmFunction = resolvedTarget.llvmValue;
    LKAssert(llvmFunction->getType()->isPointerTy() && llvmFunction->getType()->getContainedType(0)->isFunctionTy());
    auto llvmFunctionTy = llvm::dyn_cast<llvm::FunctionType>(llvmFunction->getType()->getContainedType(0));
    auto isVariadic = llvmFunctionTy->isVarArg();
    
    LKAssert(call->arguments.size() >= llvmFunctionTy->getNumParams() - resolvedTarget.argumentOffset - isVariadic);
    
    std::vector<llvm::Value *> args(resolvedTarget.argumentOffset, nullptr);
    auto numFixedArgs = llvmFunctionTy->getNumParams() - resolvedTarget.argumentOffset;
    
    for (size_t i = resolvedTarget.argumentOffset; i < llvmFunctionTy->getNumParams(); i++) {
        auto expectedType = resolvedTarget.signature->parameters[i]->type;
        auto expr = call->arguments[i - resolvedTarget.argumentOffset];
        TypeInfo *T;
        if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&expr, expectedType, &T)) {
            LKFatalError("Type mismatch in call to '%s'. Arg #%zu: expected '%s', got '%s'", llvmFunction->getName().str().c_str(), i, expectedType->str().c_str(), T->str().c_str());
        }
        args.push_back(codegen(expr));
    }
    
    if (auto memberExpr = std::dynamic_pointer_cast<ast::MemberExpr>(call->target); memberExpr != nullptr && resolvedTarget.argumentOffset == kInstanceMethodCallArgumentOffset) {
        // TODO this is a pretty bad assumption to make.
        // what if in the future there are more situations other than member function calls that require special argument offsets
        // maybe a better idea: get rid of the argumentOffset thing, introduce more granular calling conventions (yo.globalFunction, yo.staticmember, yo.instancemember, yo.lambda, etc) and make implicit argument insertion dependent on that!
        args[0] = codegen(memberExpr->target);
    }
    
    if (isVariadic && getResolvedFunctionWithName(llvmFunction->getName().str())) {
        for (auto it = call->arguments.begin() + numFixedArgs; it != call->arguments.end(); it++) {
            args.push_back(codegen(*it));
        }
    } else if (isVariadic) {
        throw; // TODO implement
    }
    
    return builder.CreateCall(llvmFunction, args);
}



llvm::Value *IRGenerator::codegen_HandleIntrinsic(std::shared_ptr<ast::FunctionSignature> signature, std::shared_ptr<ast::CallExpr> call) {
    auto name = mangling::mangleCanonicalNameForSignature(signature);
    
    if (name == "static_cast" || name == "reinterpret_cast") {
        // TODO somehow use the SrcTy, if explicitly given?
        auto dstTy = call->explicitTemplateArgumentTypes[0];
        auto arg = call->arguments[0];
        auto castKind = name == "static_cast"
            ? ast::Typecast::CastKind::StaticCast
            : ast::Typecast::CastKind::Bitcast;
        return codegen(std::make_shared<ast::Typecast>(arg, dstTy, castKind));
    }
    
    if (name == "sizeof") {
        auto T = getLLVMType(call->explicitTemplateArgumentTypes[0]);
        return llvm::ConstantInt::get(i64, module->getDataLayout().getTypeAllocSize(T));
    }
    
    std::cout << "Unhandled call to intrinsic: " << name << std::endl;
    LKFatalError("");
}




llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::UnaryExpr> unaryExpr) {
    emitDebugLocation(unaryExpr);
    
    auto expr = unaryExpr->expr;
    
    switch (unaryExpr->op) {
        case ast::UnaryExpr::Operation::Negate:
            return builder.CreateNeg(codegen(expr));
        
        case ast::UnaryExpr::Operation::BitwiseNot:
            return builder.CreateNot(codegen(expr));
        
        case ast::UnaryExpr::Operation::LogicalNegation: {
            auto T = guessType(expr);
            LKAssert(T->equals(TypeInfo::Bool) || T->isPointer() || T->isIntegerType());
            return builder.CreateIsNull(codegen(expr)); // TODO this seems like a cop-out answer?
        }
    }
}






#pragma mark - Control Flow

llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::IfStmt> ifStmt) {
    emitDebugLocation(ifStmt);
    
    using BK = ast::IfStmt::Branch::BranchKind;
    
    auto F = builder.GetInsertBlock()->getParent();
    auto mergeBB = llvm::BasicBlock::Create(C, "merge");
    bool needsMergeBB = false;
    
    // The entry points to each branch's condition
    // Note that if the last branch is a conditionless else branch, this points directly to the branch body
    std::vector<llvm::BasicBlock *> branchConditionBlocks(1, nullptr);
    std::vector<llvm::BasicBlock *> branchBodyBlocks;
    
    for (auto &branch : ifStmt->branches) {
        branchBodyBlocks.push_back(llvm::BasicBlock::Create(C, "if_body"));
        if (branch->kind != BK::Else) {
            branchConditionBlocks.push_back(llvm::BasicBlock::Create(C, "if_cond"));
        }
    }
    
    if (ifStmt->branches.back()->kind == BK::Else) {
        branchConditionBlocks.back() = branchBodyBlocks.back();
    } else {
        needsMergeBB = true;
        branchConditionBlocks.back() = mergeBB;
    }
    
    
    for (size_t i = 0; i < ifStmt->branches.size(); i++) {
        if (ifStmt->branches[i]->kind == BK::Else) break;
        if (i > 0) {
            auto BB = branchConditionBlocks[i];
            F->getBasicBlockList().push_back(BB);
            builder.SetInsertPoint(BB);
        }
        auto condV = codegen(ifStmt->branches[i]->condition);
        builder.CreateCondBr(condV, branchBodyBlocks[i], branchConditionBlocks[i + 1]);
    }
    
    
    for (size_t i = 0; i < ifStmt->branches.size(); i++) {
        auto BB = branchBodyBlocks[i];
        F->getBasicBlockList().push_back(BB);
        builder.SetInsertPoint(BB);
        
        codegen(ifStmt->branches[i]->body);
        if (!builder.GetInsertBlock()->back().isTerminator()) {
            needsMergeBB = true;
            builder.CreateBr(mergeBB);
        }
    }
    
    if (needsMergeBB) {
        F->getBasicBlockList().push_back(mergeBB);
        builder.SetInsertPoint(mergeBB);
    }
    
    return nullptr;
}



llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::WhileStmt> whileStmt) {
    emitDebugLocation(whileStmt);
    
    // TODO what if there;s a return statement in the body!
    auto F = builder.GetInsertBlock()->getParent();
    
    // TODO add unique ids to the branch names!, add the current function name?
    auto condBB = llvm::BasicBlock::Create(C, "while_cond");
    auto bodyBB = llvm::BasicBlock::Create(C, "while_body");
    auto mergeBB = llvm::BasicBlock::Create(C, "while_merge");
    
    F->getBasicBlockList().push_back(condBB);
    builder.CreateBr(condBB);
    builder.SetInsertPoint(condBB);
    
    builder.CreateCondBr(codegen(whileStmt->condition), bodyBB, mergeBB);
    
    F->getBasicBlockList().push_back(bodyBB);
    builder.SetInsertPoint(bodyBB);
    codegen(whileStmt->body);
    builder.CreateBr(condBB);
    
    F->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);
    
    return nullptr;
}


// TODO move to utils!
template <typename T>
void vec_append(std::vector<T> &dest, const std::vector<T> &src) {
    dest.insert(dest.end(), src.begin(), src.end());
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::ForLoop> forLoop) {
    emitDebugLocation(forLoop);
    
    // TODO this is disgusting
    
    auto T = guessType(forLoop->expr);
    LKAssert(T->isPointer() && T->getPointee()->isComplex());
    auto iteratorCallTarget = mangling::mangleCanonicalName(T->getPointee()->getName(), "iterator", ast::FunctionSignature::FunctionKind::InstanceMethod);
    
    auto call = std::make_shared<ast::CallExpr>(std::make_shared<ast::Identifier>(iteratorCallTarget),
                                                std::vector<std::shared_ptr<ast::Expr>>{ forLoop->expr });
    
    
    auto forStmtScope = std::make_shared<ast::Composite>();
    auto it_ident = std::make_shared<ast::Identifier>("$it");
    forStmtScope->statements.push_back(std::make_shared<ast::VariableDecl>(it_ident, TypeInfo::Unresolved, call));
    
    // while loop
    auto callInstanceMethod = [](const std::shared_ptr<ast::Identifier> &target, const std::string &methodName) {
        return std::make_shared<ast::CallExpr>(std::make_shared<ast::MemberExpr>(target, methodName));
    };
    
    auto whileBody = std::make_shared<ast::Composite>();
    whileBody->statements.push_back(std::make_shared<ast::VariableDecl>(forLoop->ident, TypeInfo::Unresolved, callInstanceMethod(it_ident, "next")));
    vec_append(whileBody->statements, forLoop->body->statements);
    forStmtScope->statements.push_back(std::make_shared<ast::WhileStmt>(callInstanceMethod(it_ident, "hasNext"), whileBody));
    return codegen(forStmtScope);
}



#pragma mark - Comparisons/Conditions

llvm::CmpInst::Predicate getMatchingLLVMCmpInstPredicateForComparisonOperator_Float(ast::Comparison::Operation op) {
    using Operation = ast::Comparison::Operation;
    using Predicate = llvm::CmpInst::Predicate;
    
    switch (op) {
        case Operation::EQ: return Predicate::FCMP_OEQ;
        case Operation::NE: return Predicate::FCMP_ONE;
        case Operation::LT: return Predicate::FCMP_OLT;
        case Operation::LE: return Predicate::FCMP_OLE;
        case Operation::GT: return Predicate::FCMP_OGT;
        case Operation::GE: return Predicate::FCMP_OGE;
    }
}

llvm::CmpInst::Predicate getMatchingLLVMCmpInstPredicateForComparisonOperator_Int(ast::Comparison::Operation op, bool isSigned) {
    using Operation = ast::Comparison::Operation;
    using Predicate = llvm::CmpInst::Predicate;
    
    switch (op) {
        case Operation::EQ: return Predicate::ICMP_EQ;
        case Operation::NE: return Predicate::ICMP_NE;
        case Operation::LT: return isSigned ? Predicate::ICMP_SLT : Predicate::ICMP_ULT;
        case Operation::LE: return isSigned ? Predicate::ICMP_SLE : Predicate::ICMP_ULE;
        case Operation::GT: return isSigned ? Predicate::ICMP_SGT : Predicate::ICMP_UGT;
        case Operation::GE: return isSigned ? Predicate::ICMP_SGE : Predicate::ICMP_UGE;
    }
}



llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::Comparison> comparison) {
    emitDebugLocation(comparison);
    
    auto lhsTy = guessType(comparison->lhs);
    auto rhsTy = guessType(comparison->rhs);
    
    llvm::CmpInst::Predicate pred;
    llvm::Value *lhs, *rhs;
    
    // Floats?
    if (lhsTy->equals(TypeInfo::Double) && rhsTy->equals(TypeInfo::Double)) {
        return builder.CreateFCmp(getMatchingLLVMCmpInstPredicateForComparisonOperator_Float(comparison->op),
                                  codegen(comparison->lhs), codegen(comparison->rhs));
    }
    
    // Are both integers?
    if (!lhsTy->isIntegerType() || !rhsTy->isIntegerType()) {
        LKFatalError("Cannot compare unrelated types '%s' and '%s'", rhsTy->str().c_str(), rhsTy->str().c_str());
    }
    
    if (lhsTy->equals(rhsTy)) {
        pred = getMatchingLLVMCmpInstPredicateForComparisonOperator_Int(comparison->op, lhsTy->isSigned());
        lhs = codegen(comparison->lhs);
        rhs = codegen(comparison->rhs);
    } else {
        // Both are integers, but different types
        
        TypeInfo *castDestTy = TypeInfo::Unresolved;
        auto largerSize = std::max(lhsTy->getSize(), rhsTy->getSize());
        
        if (largerSize <= TypeInfo::kSizeof_i32) {
            castDestTy = TypeInfo::i32;
        } else {
            LKAssert(largerSize == TypeInfo::kSizeof_i64);
            castDestTy = TypeInfo::i64;
        }
        
        lhs = codegen(std::make_shared<ast::Typecast>(comparison->lhs, castDestTy, ast::Typecast::CastKind::StaticCast));
        rhs = codegen(std::make_shared<ast::Typecast>(comparison->rhs, castDestTy, ast::Typecast::CastKind::StaticCast));
        
        pred = getMatchingLLVMCmpInstPredicateForComparisonOperator_Int(comparison->op, lhsTy->isSigned() || rhsTy->isSigned());
    }
    
    return builder.CreateICmp(pred, lhs, rhs);
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::LogicalOperation> logOp) {
    emitDebugLocation(logOp);
    
    
    // TODO rewrite
    auto lhs = codegen(logOp->lhs);
    auto rhs = codegen(logOp->rhs);
    
    LKAssert(lhs->getType() == Bool && rhs->getType() == Bool);
    
    auto op = logOp->op == ast::LogicalOperation::Operation::And
        ? llvm::Instruction::BinaryOps::And
        : llvm::Instruction::BinaryOps::Or;
    
    return builder.CreateBinOp(op, lhs, rhs);
}





#pragma mark - Types


llvm::Type *IRGenerator::getLLVMType(TypeInfo *TI) {
    if (TI->isTemplatedType()) {
        LKFatalError("TODO");
        return getLLVMType(instantiateTemplatedType(TI));
    }
    
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
            auto Type = getLLVMType(TI_temp);
            
            while (num_indirections--) {
                Type = Type->getPointerTo();
            }
            TI->setLLVMType(Type);
            return Type;
        }
        
        case TypeInfo::Kind::Complex: {
            auto name = TI->getName();
            LKAssert(typeCache.contains(name));
            
            auto llvmStructTy = llvm::StructType::create(C, name);
            
            std::vector<llvm::Type *> llvmStructFields = util::vector::map(typeCache.getStruct(name)->members,
                                                                           [this] (auto &member) { return getLLVMType(member->type); } );
            
//            std::vector<llvm::Type *> types;
//            for (auto &Member : typeCache.getStruct(Name)->Members) {
//                Types.push_back(getLLVMType(Member->type));
//            }
            
            llvmStructTy->setBody(llvmStructFields);
            TI->setLLVMType(llvmStructTy);
            return llvmStructTy;
        }
        
        case TypeInfo::Kind::Function: {
            auto functionTypeInfo = TI->getFunctionTypeInfo();
            LKAssert(functionTypeInfo.callingConvention == TypeInfo::FunctionTypeInfo::CallingConvention::C);
            auto paramTypes = util::vector::map(functionTypeInfo.parameterTypes, [this] (auto TI) { return getLLVMType(TI); });
            auto llvmFnTy = llvm::FunctionType::get(getLLVMType(functionTypeInfo.returnType), paramTypes, false); // TODO support variadic function types?
            auto T = llvmFnTy->getPointerTo();
            TI->setLLVMType(T);
            return T;
        }
        
        case TypeInfo::Kind::Typealias:
            return getLLVMType(TI->getPointee());
        
        case TypeInfo::Kind::Unresolved:
            LKFatalError("unresolved type '%s'", TI->getName().c_str());
        
        case TypeInfo::Kind::ComplexTemplated: {
            LKFatalError("should never reach here");
//            auto Name = mangling::mangleTemplatedComplexType(TI);
//            std::cout << Name << std::endl;
//
//            LKAssert(typeCache.Contains(Name));
            
//            if (auto ResolvedTy = typeCache.Get(Name)) {
//                return getLLVMType(ResolvedTy);
//            } else {
//
//            }
//            throw;
        }
    }
    throw;
}




llvm::DIType *IRGenerator::getDIType(TypeInfo *TI) {
    if (auto DIType = TI->getDIType()) return DIType;
    
    auto byteWidth = module->getDataLayout().getTypeSizeInBits(i8);
    auto pointerWidth = module->getDataLayout().getPointerSizeInBits();
    
    
    if (TI->isVoidType()) {
        return nullptr;
    }
    
    if (TI->isIntegerType()) {
        auto ty = debugInfo.builder.createBasicType(TI->getName(), TI->getSize() * byteWidth,
                                            TI->isSigned() ? llvm::dwarf::DW_ATE_signed : llvm::dwarf::DW_ATE_unsigned);
        TI->setDIType(ty);
        return ty;
    }
    
    if (TI->isPointer()) {
        auto ty = debugInfo.builder.createPointerType(getDIType(TI->getPointee()), pointerWidth);
        TI->setDIType(ty);
        return ty;
    }
    
    if (TI->isPrimitive()) {
        llvm::DIType *ty;
        if (TI->equals(TypeInfo::Bool)) {
            ty = debugInfo.builder.createBasicType("bool", byteWidth, llvm::dwarf::DW_ATE_boolean);
        } else {
            goto fail;
        }
        
        TI->setDIType(ty);
        return ty;
    }
    
    if (TI->isComplex()) {
        auto &name = TI->getName();
        auto structDecl = typeCache.getStruct(name);
        auto &SL = structDecl->getSourceLocation();
        
        auto declUnit = DIFileForNode(debugInfo.builder, structDecl);
        
        auto &dataLayout = module->getDataLayout();
        
        auto llvmTy = llvm::dyn_cast<llvm::StructType>(getLLVMType(TI));
        auto structLayout = dataLayout.getStructLayout(llvmTy);
        
        std::vector<llvm::Metadata *> elements;
        for (size_t i = 0; i < structDecl->members.size(); i++) {
            auto &member = structDecl->members[i];
            auto llvmMemberTy = getLLVMType(member->type);
            auto memberTy = debugInfo.builder.createMemberType(debugInfo.compileUnit, member->name->value, declUnit,
                                                       member->getSourceLocation().line,
                                                       dataLayout.getTypeSizeInBits(llvmMemberTy),
                                                       dataLayout.getPrefTypeAlignment(llvmMemberTy),
                                                       structLayout->getElementOffsetInBits(i),
                                                       llvm::DINode::DIFlags::FlagZero, getDIType(member->type));
            elements.push_back(memberTy);
        }
        
        auto ty = debugInfo.builder.createStructType(debugInfo.compileUnit, name,
                                             DIFileForNode(debugInfo.builder, structDecl), SL.line,
                                             dataLayout.getTypeSizeInBits(llvmTy),
                                             dataLayout.getPrefTypeAlignment(llvmTy),
                                             llvm::DINode::DIFlags::FlagZero, nullptr, debugInfo.builder.getOrCreateArray(elements));
        TI->setDIType(ty);
        return ty;
    }
    
    if (TI->isFunction()) {
        auto &FTI = TI->getFunctionTypeInfo();
        
        std::vector<llvm::Metadata *> parameterTypes;
        parameterTypes.push_back(getDIType(FTI.returnType));
        for (auto &paramTy : FTI.parameterTypes) {
            parameterTypes.push_back(getDIType(paramTy));
        }
        
        auto ty = debugInfo.builder.createPointerType(debugInfo.builder.createSubroutineType(debugInfo.builder.getOrCreateTypeArray(parameterTypes)), pointerWidth);
        TI->setDIType(ty);
        return ty;
    }
    
fail:
    LKFatalError("TODO: Create DIType for '%s'", TI->str().c_str());
    throw;
}


// TODO what does this do / why is it here / is it still needed?
bool IRGenerator::isTriviallyConvertible(TypeInfo *srcType, TypeInfo *destType) {
    if (srcType->equals(destType)) return true;
    
    if (srcType->isIntegerType() && destType->isIntegerType()) {
        throw;
    }
    
    return false;
}


bool IRGenerator::valueIsTriviallyConvertibleTo(std::shared_ptr<ast::NumberLiteral> number, TypeInfo *TI) {
    using NT = ast::NumberLiteral::NumberType;
    
    // Allowed trivial conversions:
    // int literal to any int type (as long as the value fits)
    // int literal to double
    
    if (number->type == NT::Boolean) return TI->equals(TypeInfo::Bool);
    
    if (TI->equals(TypeInfo::Double)) {
        return number->type == NT::Double || number->type == NT::Integer;
    }
    
    LKAssert(number->type == NT::Integer && TI->isIntegerType());
    LKAssert(number->value >= 0); // TODO whatthefuc? this will never be false since ast::NumberLitera::Value is unsigned!!!!!
    
    auto value = number->value;
    uint8_t bitCount = 0;
    while (value != 0) { ++bitCount; value >>= 1; }
    
    return bitCount <= TI->getSize();
}




TypeInfo *IRGenerator::guessType(std::shared_ptr<ast::Expr> expr) {
#define IF(name, T) if (auto name = std::dynamic_pointer_cast<T>(expr))
    
    IF(numberLiteral, ast::NumberLiteral) {
        return guessType(numberLiteral);
    }
    
    IF(binaryExpr, ast::BinaryOperation) {
        return guessType(binaryExpr->lhs); // todo should this check whether lhs and rhs have the same type?
    }
    
    IF(ident, ast::Identifier) {
        return scope.getType(ident->value);
    }
    
    IF(cast, ast::Typecast) {
        return cast->destType;
    }
    
    IF(stringLiteral, ast::StringLiteral) {
        switch (stringLiteral->kind) {
            case ast::StringLiteral::StringLiteralKind::NormalString:
                return TypeInfo::getWithName("String");
            case ast::StringLiteral::StringLiteralKind::ByteString:
                return TypeInfo::i8_ptr;
        }
    }
    
    IF(unaryExpr, ast::UnaryExpr) {
        switch (unaryExpr->op) {
            case ast::UnaryExpr::Operation::Negate:
            case ast::UnaryExpr::Operation::BitwiseNot:
                return guessType(unaryExpr->expr);
            case ast::UnaryExpr::Operation::LogicalNegation:
                return TypeInfo::Bool;
        }
    }
    
    IF(matchExpr, ast::MatchExpr) {
        return guessType(matchExpr->branches.front()->expression); // TODO somehow ensure all branches return the same type
    }
    
    IF(rawLLVMValueExpr, ast::RawLLVMValueExpr) {
        return rawLLVMValueExpr->type;
    }
    
    IF(memberExpr, ast::MemberExpr) {
        auto targetTy = guessType(memberExpr->target);
        LKAssert(targetTy->isPointer() && targetTy->getPointee()->isComplex());
        return typeCache.getMember(targetTy->getPointee()->getName(), memberExpr->memberName).second;
    }
    
    IF(callExpr, ast::CallExpr) {
        return resolveCall(callExpr, true).signature->returnType;
    }
    
    IF(subscript, ast::SubscriptExpr) {
        return guessType(subscript->target)->getPointee();
    }
    
    IF(comp, ast::Comparison) {
        return TypeInfo::Bool;
    }
    
    LKFatalError("Unhandled node %s", util::typeinfo::getTypename(*expr).c_str());
    
#undef IF
}


TypeInfo *IRGenerator::guessType(std::shared_ptr<ast::NumberLiteral> numberLiteral) {
    using NT = ast::NumberLiteral::NumberType;
    
    switch (numberLiteral->type) {
        case NT::Integer:   return TypeInfo::i64;
        case NT::Boolean:   return TypeInfo::Bool;
        case NT::Double:    return TypeInfo::Double;
        case NT::Character: return TypeInfo::i8;
    }
}







TypeInfo *IRGenerator::instantiateTemplatedType(TypeInfo *TI) {
    LKFatalError("TODO");
    
    if (!TI->isTemplatedType()) return TI;
    
    auto templateStructDecl = typeCache.getStruct(TI->getName());
    LKAssert(templateStructDecl->isTemplateStruct());
    std::map<std::string, TypeInfo *> mapping;
    
    for (size_t i = 0; i < templateStructDecl->templateArguments.size(); i++) {
        mapping[templateStructDecl->templateArguments[i]] = TI->getTemplateParameterTypes()[i];
    }
    
    auto mangledName = mangling::mangleTemplatedComplexType(TI);
    
    
    
    LKFatalError("TODO");
}










#pragma mark - Synthesized Functions


namespace astgen {
    using namespace ast;
    
    std::shared_ptr<Identifier> ident(std::string value) {
        return std::make_shared<Identifier>(value);
    }
    
    std::vector<std::shared_ptr<Expr>> exprVec(std::initializer_list<std::shared_ptr<Expr>> e) {
        return e;
    }
    
    std::shared_ptr<NumberLiteral> number(uint64_t value) {
        return std::make_shared<NumberLiteral>(value, NumberLiteral::NumberType::Integer);
    }
    
    std::shared_ptr<Assignment> assign(std::shared_ptr<Expr> target, std::shared_ptr<Expr> value) {
        return std::make_shared<Assignment>(target, value);
    }
    
    std::shared_ptr<Typecast> cast(std::shared_ptr<Expr> expr, TypeInfo *ty) {
        return std::make_shared<Typecast>(expr, ty, Typecast::CastKind::StaticCast);
    }
}


llvm::Value *IRGenerator::generateStructInitializer(std::shared_ptr<ast::StructDecl> structDecl) {
    auto structName = structDecl->name->value;
    
    auto T = TypeInfo::makePointer(TypeInfo::makeComplex(structName));
    auto F = functions[mangling::mangleCanonicalName(structName, "init", ast::FunctionSignature::FunctionKind::StaticMethod)][0].decl;
    
    auto self = std::make_shared<ast::Identifier>("self");
    
    // allocate object
    {
        auto allocCall = std::make_shared<ast::CallExpr>(astgen::ident("alloc"), astgen::exprVec({
            //astgen::Number(M->getDataLayout().getTypeAllocSize(getLLVMType(T)))
            astgen::number(1)
        }));
        allocCall->explicitTemplateArgumentTypes = { T->getPointee() };
        F->body->statements.push_back(std::make_shared<ast::VariableDecl>(self, T, allocCall));
    }
    
    // set runtime metadata
    if (enableARC && structDecl->attributes->arc) {
        auto set_retaincount = std::make_shared<ast::Assignment>(std::make_shared<ast::MemberExpr>(self, "retainCount"),
                                                                 astgen::number((uint64_t(1) << 60) | 1));
        
        auto sel = mangling::mangleCanonicalName(structName, "dealloc", ast::FunctionSignature::FunctionKind::InstanceMethod);
        auto dealloc_fn = functions[sel][0].llvmFunction;
        
        llvm::Type *t[] = { i8_ptr };
        auto dealloc_fn_ty = llvm::FunctionType::get(Void, t, false)->getPointerTo();
        auto dealloc_fn_cast = builder.CreateBitCast(dealloc_fn, dealloc_fn_ty);
        auto set_deallocFn = std::make_shared<ast::Assignment>(std::make_shared<ast::MemberExpr>(self, "deallocPtr"),
                                                               std::make_shared<ast::RawLLVMValueExpr>(dealloc_fn_cast, structDecl->members[1]->type));
        
        F->body->statements.push_back(set_retaincount);
        F->body->statements.push_back(set_deallocFn);
    }
    
    // set properties
    for (auto &param : F->signature->parameters) {
        F->body->statements.push_back(std::make_shared<ast::Assignment>(std::make_shared<ast::MemberExpr>(self, param->name->value), param->name));
    }
    F->body->statements.push_back(std::make_shared<ast::ReturnStmt>(self));
    
    return codegen(F);
}
