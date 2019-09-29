//
//  IRGen.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

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

using NK = ast::Node::NodeKind;


inline constexpr unsigned kInstanceMethodCallArgumentOffset = 1;
static const std::string kRetvalAllocaIdentifier = "%retval";

#define unhandled_node(node) \
{ std::cout << __PRETTY_FUNCTION__ << ": Unhandled Node: " << util::typeinfo::getTypename(*(node)) << std::endl; \
throw; }




std::shared_ptr<ast::Ident> makeIdent(const std::string& str) {
    return std::make_shared<ast::Ident>(str);
}



// IRGen

llvm::LLVMContext IRGenerator::C;

IRGenerator::IRGenerator(const std::string& moduleName, IRGenOptions options)
    : module(llvm::make_unique<llvm::Module>(moduleName, C)),
    builder(C),
    debugInfo{llvm::DIBuilder(*module), nullptr, {}},
    options(options)
{
    i8  = llvm::Type::getInt8Ty(C);
    i16 = llvm::Type::getInt16Ty(C);
    i32 = llvm::Type::getInt32Ty(C);
    i64 = llvm::Type::getInt64Ty(C);
    
    i8_ptr = i8->getPointerTo();
    Void = llvm::Type::getVoidTy(C);
    i1 = llvm::Type::getInt1Ty(C);
    Double = llvm::Type::getDoubleTy(C);
    
    debugInfo.compileUnit = debugInfo.builder.createCompileUnit(llvm::dwarf::DW_LANG_C,
                                                                debugInfo.builder.createFile("main.yo", "."),
                                                                "yo", options.isOptimized, "", 0);
    debugInfo.lexicalBlocks.push_back(debugInfo.compileUnit);
    module->addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
    
    
    
    // create all primitives' llvm::Type and llvm::DIType objects
    
    Type::initPrimitives();
    
    auto preflight_type = [&](Type *type) {
        type->setLLVMType(getLLVMType(type));
        type->setLLVMDIType(getDIType(type));
    };
    preflight_type(Type::getVoidType());
    preflight_type(Type::getBoolType());
    preflight_type(Type::getInt8Type());
    preflight_type(Type::getUInt8Type());
    preflight_type(Type::getInt16Type());
    preflight_type(Type::getUInt16Type());
    preflight_type(Type::getInt32Type());
    preflight_type(Type::getUInt32Type());
    preflight_type(Type::getInt64Type());
    preflight_type(Type::getUInt64Type());
    preflight_type(Type::getFloat64Type());
}



void IRGenerator::emitDebugLocation(const std::shared_ptr<ast::Node> &node) {
    if (!options.enableDebugMetadata) return;
    
    if (!node) {
        builder.SetCurrentDebugLocation(llvm::DebugLoc());
        return;
    }
    const auto& SL = node->getSourceLocation();
    builder.SetCurrentDebugLocation(llvm::DebugLoc::get(SL.line, SL.column, debugInfo.lexicalBlocks.back()));
}




void IRGenerator::codegen(const ast::AST& ast) {
    preflight(ast);
    
    for (const auto& node : ast) {
        codegen(node);
    }
    
    handleStartupAndShutdownFunctions();
    debugInfo.builder.finalize();
}



std::string mangleFullyResolved(const std::shared_ptr<ast::FunctionDecl>& functionDecl) {
    if (functionDecl->getAttributes().no_mangle) {
        return functionDecl->getName();
    } else if (!functionDecl->getAttributes().mangledName.empty()) {
        return functionDecl->getAttributes().mangledName;
    }
    return mangling::mangleFullyResolved(functionDecl);
}



void IRGenerator::preflight(const ast::AST& ast) {
    // Q: Why collect the different kinds of top level decls first and then process them, instead of simply processing them all in a single for loop?
    // A: What if a function uses a type that is declared at some later point, or in another module? it's important all of these are processed in the correct order
    std::vector<std::shared_ptr<ast::TypealiasDecl>> typealiases;
    std::vector<std::shared_ptr<ast::FunctionDecl>> functionDecls;
    std::vector<std::shared_ptr<ast::StructDecl>> structDecls;
    std::vector<std::shared_ptr<ast::ImplBlock>> implBlocks;
    
#define CASE(node, kind, dest) case NK::kind: { dest.push_back(std::static_pointer_cast<ast::kind>(node)); continue; }
    for (const auto& node : ast) {
        switch(node->getNodeKind()) {
            CASE(node, TypealiasDecl, typealiases)
            CASE(node, FunctionDecl, functionDecls)
            CASE(node, StructDecl, structDecls)
            CASE(node, ImplBlock, implBlocks)
            default: continue;
        }
    }
#undef CASE
    
    for (const auto& typealiasDecl : typealiases) {
        // TODO is this a good idea?
        // TODO prevent circular aliases!
        nominalTypes[typealiasDecl->typename_] = resolveTypeDesc(typealiasDecl->type);
    }
    
    for (const auto& structDecl : structDecls) {
        registerStructDecl(structDecl);
    }
    
    for (const auto& functionDecl : functionDecls) {
        if (functionDecl->getAttributes().extern_) {
            functionDecl->getAttributes().no_mangle = true;
        }
        registerFunction(functionDecl);
    }
    
    for (const auto& implBlock : implBlocks) {
        registerImplBlock(implBlock);
    }
}



void IRGenerator::registerFunction(std::shared_ptr<ast::FunctionDecl> functionDecl) {
    const auto& sig = functionDecl->getSignature();
    
    bool isMain = functionDecl->isOfKind(ast::FunctionKind::GlobalFunction) && functionDecl->getName() == "main";
    
    if (isMain) {
        functionDecl->getAttributes().no_mangle = true;
        // TODO run some checks to make sure main fulfills the requirements (correct return & parameter types, no other attributes, etc)
//        LKAssert(Signature->ReturnType->Equals(TypeInfo::i32));
//        LKAssert(Signature->parameters.size() == 0 || Signature->parameters.size() == 2);
//        if (Signature->parameters.size() == 2) {
//            LKAssert(Signature->parameters[0]->type->Equals(TypeInfo::))
//        }
    }
    
    
    if (sig.isTemplateFunction() || functionDecl->getAttributes().intrinsic) {
        auto canonicalName = mangling::mangleCanonicalName(functionDecl);
        functions[canonicalName].push_back(ResolvedCallable(sig, functionDecl, nullptr, 0));
        return;
    }
    
    
    auto returnType = resolveTypeDesc(sig.returnType);
    std::vector<llvm::Type *> parameterTypes = util::vector::map(sig.parameters, [this](const auto& P) {
        return resolveTypeDesc(P->type)->getLLVMType();
    });
    
    
    const std::string canonicalName = mangling::mangleCanonicalName(functionDecl);
    const std::string resolvedName = functionDecl->getAttributes().extern_ ? canonicalName : mangleFullyResolved(functionDecl);
    
    if (auto otherDecl = getResolvedFunctionWithName(resolvedName)) {
        LKAssert(otherDecl->funcDecl);
        const auto& otherSig = otherDecl->funcDecl->getSignature();
        LKAssert(functionDecl->getAttributes().extern_ && "only extern functions are allowed to have multiple declarations");
        LKAssert(resolveTypeDesc(sig.returnType) == resolveTypeDesc(otherSig.returnType));
        LKAssert(sig.parameters.size() == otherSig.parameters.size());
        return;
    }
    
    LKAssertMsg(module->getFunction(resolvedName) == nullptr, util::fmt_cstr("Redefinition of function '%s'", resolvedName.c_str())); // TODO print the signature instead!
    
    auto FT = llvm::FunctionType::get(returnType->getLLVMType(), parameterTypes, functionDecl->getAttributes().variadic);
    auto F = llvm::Function::Create(FT, llvm::Function::LinkageTypes::ExternalLinkage, resolvedName, *module);
    F->setDSOLocal(!functionDecl->getAttributes().extern_);
    
    ResolvedCallable RC(functionDecl, F, 0); // TODO set the correct argument offset here ?!
    LKAssert(!util::map::has_key(resolvedFunctions, resolvedName));
    resolvedFunctions.emplace(resolvedName, RC);
    functions[canonicalName].push_back(RC);
}



std::optional<ResolvedCallable> IRGenerator::getResolvedFunctionWithName(const std::string &name) {
    return util::map::get_opt(resolvedFunctions, name);
}



void IRGenerator::registerStructDecl(std::shared_ptr<ast::StructDecl> structDecl) {
    if (structDecl->isTemplateStruct()) {
        LKFatalError("TODO");
    }
    
    auto LKMetadataAccessor = llvm::dyn_cast_or_null<StructType>(nominalTypes["LKMetadataAccessor"]);
    
    const auto& structName = structDecl->name;
    // TODO add a check somewhere here to make sure there are no duplicate struct members
    
    uint64_t memberCount = structDecl->members.size();
    if (LKMetadataAccessor) memberCount += LKMetadataAccessor->getMembers().size();
    
    StructType::MembersT structMembers;
    structMembers.reserve(memberCount);
    //LKAssert(LKMetadataAccessor->isStructTy()); // Should always be true
    
    if (options.enableARC && structDecl->attributes.arc) {
        for (const auto& member : LKMetadataAccessor->getMembers()) {
            structMembers.push_back(member);
        }
    }
    
    
    for (const auto& varDecl : structDecl->members) {
        // Since the varDecls are pointers, the resolveTypeDecl call below also sets the structDecl's member's types,
        // meaning that we can simply use that as the initializer parameters
        auto type = resolveTypeDesc(varDecl->type);
        structMembers.push_back({varDecl->name, type});
    }
    
    auto structTy = StructType::create(structName, structMembers, structDecl->getSourceLocation());
    nominalTypes[structName] = structTy;
    
    if (!structDecl->attributes.no_init) {
        ast::FunctionSignature signature;
        signature.returnType = ast::TypeDesc::makeResolved(structTy->getPointerTo());
        signature.parameters = structDecl->members;
        
        auto initFnDecl = std::make_shared<ast::FunctionDecl>(ast::FunctionKind::StaticMethod, "init", signature, attributes::FunctionAttributes());
        initFnDecl->setImplType(structTy);
        
        registerFunction(initFnDecl);
    }
}



void IRGenerator::registerImplBlock(std::shared_ptr<ast::ImplBlock> implBlock) {
    using FK = ast::FunctionKind;
    
    const auto& typename_ = implBlock->typename_;
    auto type = nominalTypes.at(typename_);
    LKAssert(type->isStructTy());
    
    for (auto& fn : implBlock->methods) {
        LKAssert(!fn->getAttributes().no_mangle && "invalid attribute for function in impl block: no_mangle");
        auto kind = FK::StaticMethod;
        if (!fn->getSignature().parameters.empty()) {
            auto first = fn->getSignature().parameters[0];
            if (first->name == "self" && resolveTypeDesc(first->type) == type->getPointerTo()) {
                // TODO allow omitting the type of a self parameter, and set it here implicitly?
                kind = FK::InstanceMethod;
            }
        }
        fn->setFunctionKind(kind);
        fn->setImplType(llvm::dyn_cast<StructType>(type));
        registerFunction(fn);
    }
}





# pragma mark - Codegen



#define CASE(node, kind, ...) case NK::kind: return codegen(std::static_pointer_cast<ast::kind>(node), ## __VA_ARGS__);
#define CASE2(node, kind, ty, ...) case NK::kind: return codegen(std::static_pointer_cast<ast::ty>(node), ## __VA_ARGS__);
#define SKIP(node, kind) case NK::kind: return nullptr;

llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::TopLevelStmt> TLS) {
    switch (TLS->getNodeKind()) {
        CASE(TLS, FunctionDecl)
        CASE(TLS, StructDecl)
        CASE(TLS, ImplBlock)
        SKIP(TLS, TypealiasDecl)
        default: unhandled_node(TLS);
    }
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::LocalStmt> localStmt) {
    switch (localStmt->getNodeKind()) {
        CASE(localStmt, Composite)
        CASE(localStmt, VarDecl)
        CASE(localStmt, IfStmt)
        CASE(localStmt, Assignment)
        CASE(localStmt, WhileStmt)
        CASE(localStmt, ForLoop)
        CASE(localStmt, ExprStmt)
        default: unhandled_node(localStmt);
    }
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::Expr> expr, ValueKind returnValueKind) {
    switch (expr->getNodeKind()) {
    CASE(expr, NumberLiteral)
    CASE(expr, Ident, returnValueKind)
    CASE(expr, CastExpr)
    CASE(expr, StringLiteral)
    CASE(expr, UnaryExpr)
    CASE(expr, MatchExpr)
    CASE(expr, RawLLVMValueExpr)
    CASE(expr, MemberExpr, returnValueKind)
    CASE(expr, SubscriptExpr, returnValueKind)
    CASE(expr, CallExpr)
    CASE(expr, BinOp)
    default: unhandled_node(expr)
    }
}

#undef SKIP
#undef CASE
#undef CASE2


llvm::DIFile* DIFileForSourceLocation(llvm::DIBuilder& builder, const parser::TokenSourceLocation& loc) {
    const auto [directory, filename] = util::string::extractPathAndFilename(loc.filepath);
    return builder.createFile(filename, directory);
}


llvm::DISubroutineType *IRGenerator::_toDISubroutineType(const ast::FunctionSignature& signature) {
    // Looking at [godbolt]( https://godbolt.org/z/EKfzqi ), it seems like the first element should be the function's return type?
    
    std::vector<llvm::Metadata *> types(signature.parameters.size() + 1);
    
    types.push_back(resolveTypeDesc(signature.returnType)->getLLVMDIType());
    for (const auto& param : signature.parameters) {
        types.push_back(resolveTypeDesc(param->type)->getLLVMDIType());
    }    
    return debugInfo.builder.createSubroutineType(debugInfo.builder.getOrCreateTypeArray(types));
}








#pragma mark - Types



Type* resolvePrimitiveType(std::string_view name) {
#define HANDLE(_name, ty) if (name == _name) return ty;
    HANDLE("void", Type::getVoidType())
    HANDLE("bool", Type::getBoolType())
    HANDLE("i8",   Type::getInt8Type())
    HANDLE("i16",  Type::getInt16Type())
    HANDLE("i32",  Type::getInt32Type())
    HANDLE("i64",  Type::getInt64Type())
    HANDLE("u8",   Type::getUInt8Type())
    HANDLE("u16",  Type::getUInt16Type())
    HANDLE("u32",  Type::getUInt32Type())
    HANDLE("u64",  Type::getUInt64Type())
    HANDLE("f64",  Type::getFloat64Type())
#undef HANDLE
    return nullptr;
}



// Attempts to resolve an AST TypeDesc and returns a unique `yo::Type*` pointer.
// Also creates the `yo::Type`'s `llvm::Type` and `llvm::DIType` and sets the respective member fields
Type* IRGenerator::resolveTypeDesc(std::shared_ptr<ast::TypeDesc> typeDesc) {
    // HUGE FUCKING PROBLEM: typedescs should be resolved in the context which they were declared, not the one in which they might be used
    // (this isn't that big an issue rn, but might become in the future)
    
    using TDK = ast::TypeDesc::Kind;
    
    if (!typeDesc) LKFatalError("NULL TYPE DESC");
    
    auto handleResolvedTy = [this, typeDesc](Type *ty) {
        typeDesc->setResolvedType(ty);
        ty->setLLVMType(getLLVMType(ty));
        ty->setLLVMDIType(getDIType(ty));
        return ty;
    };
    
    //if (auto T = typeDesc->getResolvedType()) return T;
    if (auto ty = typeDesc->getResolvedType()) {
        return handleResolvedTy(ty);
    }
    
    switch (typeDesc->getKind()) {
        case TDK::Resolved:
            // Should actually never reach here since we already have the nonnull check above
            return typeDesc->getResolvedType();
        
        case TDK::Nominal: {
            const auto& name = typeDesc->getName();
            if (auto ty = resolvePrimitiveType(name)) {
                return handleResolvedTy(ty);
            } else {
                // a nominal, non-primitive type
                
                // If there is already an entry for that type, return that
                if (auto entry = util::map::get_opt(nominalTypes, name)) {
                    return handleResolvedTy(entry.value());
                }
                
                emitError(typeDesc->getSourceLocation(), util::fmt_cstr("Unable to resolve nominal type '%s'", name.c_str()));
            }
            break;
        }
        case TDK::Pointer:
            return handleResolvedTy(resolveTypeDesc(typeDesc->getPointee())->getPointerTo());
        
        case TDK::Reference:
            return handleResolvedTy(resolveTypeDesc(typeDesc->getPointee()));
            
        
        case TDK::Function: {
            const auto& FTI = typeDesc->getFunctionTypeInfo();
            const auto paramTypes = util::vector::map(FTI.parameterTypes, [this](const auto& typeDesc) { return resolveTypeDesc(typeDesc); });
            return handleResolvedTy(FunctionType::create(resolveTypeDesc(FTI.returnType), paramTypes, FTI.callingConvention));
        }
    }
    
    LKFatalError("unhandled type desc: %s", typeDesc->str().c_str());
}




bool IRGenerator::equal(const ast::FunctionSignature &lhs, const ast::FunctionSignature &rhs) {
    if (resolveTypeDesc(lhs.returnType) != resolveTypeDesc(rhs.returnType)) return false;
    
    if (lhs.parameters.size() != rhs.parameters.size()) return false;
    
    for (size_t i = 0; i < lhs.parameters.size(); i++) {
        if (resolveTypeDesc(lhs.parameters[i]->type) != resolveTypeDesc(rhs.parameters[i]->type)) return false;
    }
    
    if (lhs.templateArgumentNames != rhs.templateArgumentNames) return false;
    
    return true;
}



















#pragma mark - Top Level Statements

llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::FunctionDecl> functionDecl) {
    const auto& sig = functionDecl->getSignature();
    const auto& attr = functionDecl->getAttributes();
    
    if (attr.extern_ || attr.intrinsic || sig.isTemplateFunction()) {
        return nullptr;
    }
    
    LKAssert(scope.isEmpty());
    auto resolvedName = mangleFullyResolved(functionDecl);
    
    auto F = module->getFunction(resolvedName);
    if (!F) {
        LKFatalError("Unable to find function '%s'", resolvedName.c_str());
    }
    
    if (attr.inline_) {
        F->addFnAttr(llvm::Attribute::InlineHint);
    }
    if (attr.always_inline) {
        F->addFnAttr(llvm::Attribute::AlwaysInline);
    }
    
    
    auto entryBB = llvm::BasicBlock::Create(C, "entry", F);
    auto returnBB = llvm::BasicBlock::Create(C, "return");
    builder.SetInsertPoint(entryBB);
    
    if (options.enableDebugMetadata) {
        auto unit = DIFileForSourceLocation(debugInfo.builder, functionDecl->getSourceLocation());
        auto SP = debugInfo.builder.createFunction(unit, functionDecl->getName(), resolvedName, unit,
                                           sig.getSourceLocation().line,
                                           _toDISubroutineType(sig),
                                           sig.getSourceLocation().line,
                                           llvm::DINode::FlagZero,
                                           llvm::DISubprogram::DISPFlags::SPFlagDefinition);
        emitDebugLocation(nullptr);
        F->setSubprogram(SP);
        debugInfo.builder.finalizeSubprogram(SP);
        debugInfo.lexicalBlocks.push_back(SP);
    }
    
    
    std::vector<llvm::AllocaInst *> paramAllocas;
    
    for (const auto& param : sig.parameters) {
        auto type = resolveTypeDesc(param->type);
        auto alloca = builder.CreateAlloca(type->getLLVMType());
        const auto& name = param->name;
        alloca->setName(name);
        scope.insert(name, type, ValueBinding(alloca, [=]() {
            return builder.CreateLoad(alloca);
        }, [=](llvm::Value *V) {
            LKFatalError("Function arguments are read-only (%s in %s)", name.c_str(), resolvedName.c_str());
        }));
        paramAllocas.push_back(alloca);
    }
    
    
    for (size_t i = 0; i < sig.parameters.size(); i++) {
        auto alloca = paramAllocas.at(i);
        builder.CreateStore(&F->arg_begin()[i], alloca);
        
        const auto& param = sig.parameters.at(i);
        if (options.enableDebugMetadata) {
            auto SP = debugInfo.lexicalBlocks.back();
            auto varInfo = debugInfo.builder.createParameterVariable(SP, alloca->getName(), i + 1, SP->getFile(),
                                                                     param->getSourceLocation().line,
                                                                     resolveTypeDesc(param->type)->getLLVMDIType());
            debugInfo.builder.insertDeclare(alloca, varInfo, debugInfo.builder.createExpression(),
                                            llvm::DILocation::get(C, param->getSourceLocation().line, param->getSourceLocation().column, SP), entryBB);
        }
    }
    
    
    
    
    llvm::Value *retvalAlloca = nullptr;
    auto returnType = resolveTypeDesc(sig.returnType);
    
    if (!returnType->isVoidTy()) {
        retvalAlloca = builder.CreateAlloca(F->getFunctionType()->getReturnType());
        auto retvalBinding = ValueBinding(retvalAlloca, []() {
            LKFatalError("retval is write-only");
            return nullptr;
        }, [this, retvalAlloca](llvm::Value *V) {
            builder.CreateStore(V, retvalAlloca);
        });
        scope.insert(kRetvalAllocaIdentifier, returnType, retvalBinding);
        
        
        // Create Debug Metadata
        if (options.enableDebugMetadata) {
            auto SP = debugInfo.lexicalBlocks.back();
            auto D = debugInfo.builder.createAutoVariable(SP, kRetvalAllocaIdentifier, SP->getFile(),
                                                          sig.getSourceLocation().line, returnType->getLLVMDIType());
            debugInfo.builder.insertDeclare(retvalAlloca, D, debugInfo.builder.createExpression(),
                                            llvm::DebugLoc::get(sig.getSourceLocation().line, 0, SP), entryBB);
        }
    }
    
    currentFunction = FunctionState(functionDecl, F, returnBB, retvalAlloca);
    
    codegen(functionDecl->getBody());
    
    // TODO this is a bad idea!
    if (F->getReturnType()->isVoidTy() && !std::dynamic_pointer_cast<ast::ReturnStmt>(functionDecl->getBody().back())) {
        codegen(std::make_shared<ast::ReturnStmt>(nullptr));
    }
    
    F->getBasicBlockList().push_back(returnBB);
    builder.SetInsertPoint(returnBB);
    
    if (returnType->isVoidTy()) {
        builder.CreateRetVoid();
    } else {
        builder.CreateRet(builder.CreateLoad(retvalAlloca));
    }
    
    LKAssert(scope.size() == sig.parameters.size() + static_cast<uint8_t>(!returnType->isVoidTy()));
    
    for (const auto& entry : scope.getEntriesSinceMarker(0)) {
        //std::cout << std::get<0>(Entry) << std::endl;
        // TODO: release
        scope.remove(entry.ident);
    }
    
    currentFunction = FunctionState();
    if (options.enableDebugMetadata) {
        debugInfo.lexicalBlocks.pop_back(); // TODO maybe add a check that the lexical blocks weren't somehow modified?
    }
    return F;
}




llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::StructDecl> structDecl) {
    if (!structDecl->attributes.no_init) {
        generateStructInitializer(structDecl);
    }
    return nullptr;
}



llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::ImplBlock> implBlock) {
    for (const auto& method : implBlock->methods) {
        codegen(method);
    }
    return nullptr;
}




#pragma mark - Local Statements


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::VarDecl> varDecl) {
    Type *type = nullptr;
    bool hasInferredType = false;
    
    if (varDecl->type == nullptr) {
        // If no type is specified, there _has_ to be an initial value
        LKAssert(varDecl->initialValue);
        type = guessType(varDecl->initialValue);
        hasInferredType = true;
    } else {
        type = resolveTypeDesc(varDecl->type);
    }
    
    LKAssert(type);
    auto alloca = builder.CreateAlloca(type->getLLVMType());
    alloca->setName(varDecl->name);
    
    // Create Debug Metadata
    if (options.enableDebugMetadata) {
        auto D = debugInfo.builder.createAutoVariable(currentFunction.llvmFunction->getSubprogram(),
                                                      varDecl->name,
                                                      debugInfo.lexicalBlocks.back()->getFile(),
                                                      varDecl->getSourceLocation().line,
                                                      type->getLLVMDIType());
        debugInfo.builder.insertDeclare(alloca, D,
                                        debugInfo.builder.createExpression(),
                                        llvm::DebugLoc::get(varDecl->getSourceLocation().line, 0, currentFunction.llvmFunction->getSubprogram()),
                                        builder.GetInsertBlock());
    }
    
    auto binding = ValueBinding(alloca, [=] () {
        return builder.CreateLoad(alloca);
    }, [=] (llvm::Value *V) {
        LKAssert(V->getType() == alloca->getType()->getPointerElementType());
        builder.CreateStore(V, alloca);
    });
    
    scope.insert(varDecl->name, type, binding);
    
    if (auto expr = varDecl->initialValue) {
        // Q: Why create and handle an assignment to set the initial value, instead of just calling Binding.Write?
        // A: The Assignment codegen also includes the trivial type transformations, whish we'd otherwise have to implement again in here
        codegen(std::make_shared<ast::Assignment>(makeIdent(varDecl->name), varDecl->initialValue));
    }
    
    return alloca;
}









llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::Composite> composite) {
    emitDebugLocation(composite);
    return codegen(composite->statements);
}


llvm::Value *IRGenerator::codegen(const std::vector<std::shared_ptr<ast::LocalStmt>>& stmtList) {
    auto marker = scope.getMarker();
    bool didReturn = false;
    
    for (auto it = stmtList.begin(); !didReturn && it != stmtList.end(); it++) {
        const auto& stmt = *it;
        if (auto returnStmt = std::dynamic_pointer_cast<ast::ReturnStmt>(stmt)) {
            codegen(returnStmt);
            didReturn = true;
        } else {
            codegen(stmt);
        }
    }
    
    for (const auto& entry : scope.getEntriesSinceMarker(marker)) {
        scope.remove(entry.ident);
    }
    
    return nullptr;
}




llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::ReturnStmt> returnStmt) {
    emitDebugLocation(returnStmt);
    
    auto FName = builder.GetInsertBlock()->getParent()->getName().str();
    const auto returnType = resolveTypeDesc(currentFunction.decl->getSignature().returnType);
    
    if (auto expr = returnStmt->expression) {
        Type *T;
        if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&expr, returnType, &T)) {
            LKFatalError("Error: Can't return value of type '%s' from function '%s' returning '%s'", T->str().c_str(), FName.c_str(), returnType->str().c_str());
        }
        
        codegen(std::make_shared<ast::Assignment>(std::make_shared<ast::Ident>(kRetvalAllocaIdentifier), expr));
        return builder.CreateBr(currentFunction.returnBB);
    }
    
    LKAssert(returnType->isVoidTy());
    return builder.CreateBr(currentFunction.returnBB);
}


template <typename T>
bool value_fits_in_type(uint64_t value) {
    auto Min = std::numeric_limits<T>::min();
    auto Max = std::numeric_limits<T>::max();
    return static_cast<T>(value) >= Min && static_cast<T>(value) <= Max;
}


bool integerLiteralFitsInType(uint64_t value, Type *type) {
#define HANDLE(size_expr, signed_t, unsigned_t) if (size == (size_expr)) { return isSigned ? value_fits_in_type<signed_t>(value) : value_fits_in_type<unsigned_t>(value); }
    
    LKAssert(type->isNumericalTy());
    auto *numTy = llvm::dyn_cast<NumericalType>(type);
    auto size = numTy->getSize();
    bool isSigned = numTy->isSigned();
    
    HANDLE(Type::getInt8Type()->getSize(), int8_t, uint8_t)
    HANDLE(Type::getInt16Type()->getSize(), int16_t, uint16_t)
    HANDLE(Type::getInt32Type()->getSize(), int32_t, uint32_t)
    HANDLE(Type::getInt64Type()->getSize(), int64_t, uint64_t)
    
    LKFatalError("should not reach here?");
#undef HANDLE
}





bool IRGenerator::typecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *expr, Type *expectedType, Type **initialTypeOfExpr) {
    auto type = guessType(*expr);
    if (initialTypeOfExpr) *initialTypeOfExpr = type;
    
    if (type == expectedType) return true;
    
    // at this point, both are integers
    if (auto numberLiteral = std::dynamic_pointer_cast<ast::NumberLiteral>(*expr)) {
        LKAssert(expectedType->isNumericalTy());
        LKAssert(integerLiteralFitsInType(numberLiteral->value, expectedType));
        
        auto loc = (*expr)->getSourceLocation();
        *expr = std::make_shared<ast::CastExpr>(*expr, ast::TypeDesc::makeResolved(expectedType), ast::CastExpr::CastKind::StaticCast);
        (*expr)->setSourceLocation(loc);
        return true;
    }
    
    std::cout << "input: " << type->str() << ", expected: " << expectedType->str() << std::endl;
    std::cout << (*expr)->getSourceLocation() << std::endl;
    throw;
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::Assignment> assignment) {
    emitDebugLocation(assignment);
    // TODO should assignments return something?
    // TODO rewrite this so that it doesn't rely on GuessType for function calls!
    
    auto expr = assignment->value;
    auto destTy = guessType(assignment->target);
    
    Type *T;
    if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&expr, destTy, &T)) {
        LKFatalError("type mismatch: cannot assign '%s' to '%s'", T->str().c_str(), destTy->str().c_str());
    }
    
    auto target = codegen(assignment->target, LValue);
    builder.CreateStore(codegen(expr, RValue), target);
    
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
            LKAssert(integerLiteralFitsInType(numberLiteral->value, Type::getInt8Type()));
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
            LKAssert(util::map::has_key(nominalTypes, std::string("String")));
            stringLiteral->kind = SLK::ByteString;
            auto target = std::make_shared<ast::Ident>(mangling::mangleCanonicalName("String", "new", ast::FunctionKind::StaticMethod));
            auto call = std::make_shared<ast::CallExpr>(target, std::vector<std::shared_ptr<ast::Expr>>(1, stringLiteral));
            call->setSourceLocation(stringLiteral->getSourceLocation());
            return codegen(call);
        }
    }
}



// If TakeAddress is true, this returns a pointer to the identifier, instead of the value stored
llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::Ident> ident, ValueKind returnValueKind) {
    emitDebugLocation(ident);
    
    if (auto binding = scope.getBinding(ident->value)) {
        switch (returnValueKind) {
            case RValue:
                return binding->read();
            case LValue:
                return const_cast<llvm::Value *>(binding->value);
        }
    }
    
    std::cout << "Unable to find identifier " << ident->value << std::endl;
    throw;
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::CastExpr> cast) {
    emitDebugLocation(cast);
    
    auto srcTy = guessType(cast->expression);
    auto destTy = resolveTypeDesc(cast->destType);
    
    if (srcTy == destTy) {
        return codegen(cast->expression);
    }
    
    llvm::Instruction::CastOps op;
    switch (cast->kind) {
        case ast::CastExpr::CastKind::Bitcast: {
            LKAssert(module->getDataLayout().getTypeSizeInBits(getLLVMType(srcTy)) == module->getDataLayout().getTypeSizeInBits(getLLVMType(destTy)));
            if (srcTy->isPointerTy() && destTy->isNumericalTy()) {
                op = llvm::Instruction::CastOps::PtrToInt;
            } else if (srcTy->isNumericalTy() && destTy->isPointerTy()) {
                op = llvm::Instruction::CastOps::IntToPtr;
            } else {
                op = llvm::Instruction::CastOps::BitCast;
            }
            break;
        }
        case ast::CastExpr::CastKind::StaticCast: {
            if (srcTy->isNumericalTy() && destTy->isNumericalTy()) {
                auto srcIntWidth  = srcTy->getLLVMType()->getIntegerBitWidth();
                auto destIntWidth = destTy->getLLVMType()->getIntegerBitWidth();
                
                if (srcIntWidth > destIntWidth) {
                    // casting to a smaller type
                    op = llvm::Instruction::CastOps::Trunc;
                } else {
                    // casting to a larger type
                    if (llvm::dyn_cast<NumericalType>(srcTy)->isSigned()) {
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
    
    return builder.CreateCast(op, codegen(cast->expression), destTy->getLLVMType());
}







llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::MemberExpr> memberExpr, ValueKind returnValueKind) {
    emitDebugLocation(memberExpr);
    
    auto targetTy = guessType(memberExpr->target);
    LKAssert(targetTy->isPointerTy());
    auto pointerTy = llvm::dyn_cast<PointerType>(targetTy);
    LKAssert(pointerTy->getPointee()->isStructTy());
    auto structTy = llvm::dyn_cast<StructType>(pointerTy->getPointee());
    
    auto [memberIndex, memberType] = structTy->getMember(memberExpr->memberName);
    LKAssert(memberType != nullptr && "member does not exist");
    
    llvm::Value *offsets[] = {
        llvm::ConstantInt::get(i32, 0),
        llvm::ConstantInt::get(i32, memberIndex)
    };
    
    auto V = builder.CreateGEP(codegen(memberExpr->target), offsets);
    
    switch (returnValueKind) {
        case LValue:
            return V;
        case RValue:
            return builder.CreateLoad(V);
    }
}




llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::SubscriptExpr> subscript, ValueKind returnValueKind) {
    emitDebugLocation(subscript);
    
    auto target = codegen(subscript->target, RValue);
    LKAssert(target->getType()->isPointerTy());
    auto offset = codegen(subscript->offset, RValue);
    LKAssert(offset->getType()->isIntegerTy());
    
    auto GEP = builder.CreateGEP(target, codegen(subscript->offset));
    
    switch (returnValueKind) {
        case LValue:
            return GEP;
        case RValue:
            return builder.CreateLoad(GEP);
    }
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
            auto ty = guessType(expr);
            LKAssert(ty == Type::getBoolType() || ty->isPointerTy() || (ty->isNumericalTy() && llvm::dyn_cast<NumericalType>(ty)->isIntegerTy()));
            return builder.CreateIsNull(codegen(expr)); // TODO this seems like a cop-out answer?
        }
    }
}




bool isValidMatchPatternForMatchedExprType(std::shared_ptr<ast::Expr> patternExpr, Type *matchedExprType) {
    // Only patterns that are trivially and can be matched w/out side effects are allowed
    // TODO add the side effect checking
    
    if (patternExpr->getNodeKind() == NK::Ident) {
        LKFatalError("TODO");
        return true;
    }
    
    if (matchedExprType->isNumericalTy()) {
        return std::dynamic_pointer_cast<ast::NumberLiteral>(patternExpr) != nullptr;
    } else if (matchedExprType == Type::getBoolType()) {
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
    
    if (TT->isNumericalTy()) {
        if (auto numberLiteral = std::dynamic_pointer_cast<ast::NumberLiteral>(PE)) {
            if (valueIsTriviallyConvertibleTo(numberLiteral, TT)) {
                auto cmp = std::make_shared<ast::BinOp>(ast::Operator::EQ,
                                                        std::make_shared<ast::RawLLVMValueExpr>(info.targetLLVMValue, TT),
                                                        numberLiteral);
                cmp->setSourceLocation(numberLiteral->getSourceLocation());
                return codegen(cmp);
                
            }
        } else {
            LKFatalError("Incompatible Match types: cannot match %s against %s", TT->str().c_str(), PT->str().c_str());
        }
    }
    throw;
}



bool lastBranchIsWildcard(const std::shared_ptr<ast::MatchExpr> &matchExpr) {
    const auto& lastBranch = matchExpr->branches.back();
    if (lastBranch.patterns.size() > 1) return false;
    if (auto ident = std::dynamic_pointer_cast<ast::Ident>(lastBranch.patterns[0])) {
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
    auto resultType = guessType(matchExpr->branches.front().expression);
    auto matchTargetValue = codegen(matchExpr->target);
    
    
    std::map<llvm::BasicBlock *, llvm::Value *> branchMappings;
    
    auto mergeBB = llvm::BasicBlock::Create(C);
    auto nextCondBB = llvm::BasicBlock::Create(C);
    auto nextValueBB = llvm::BasicBlock::Create(C);
    
    // TODO get rid of this and just have the first condition be part of the BB containing the match expression
    builder.CreateBr(nextCondBB);
    
    auto lastBranchIsWildcard = ::lastBranchIsWildcard(matchExpr);
    
    for (size_t i = 0; i < matchExpr->branches.size(); i++) {
        auto& branch = matchExpr->branches[i]; // not const bc we might modify the expression if it's a literal
        auto valueBB = nextValueBB;
        nextValueBB = llvm::BasicBlock::Create(C);
        
        bool isLastBranchBeforeWildcard = lastBranchIsWildcard && i + 2 == matchExpr->branches.size();
        
        for (auto it = branch.patterns.begin(); it != branch.patterns.end(); it++) {
            const auto& patternExpr = *it;
            if (auto ident = std::dynamic_pointer_cast<ast::Ident>(patternExpr)) {
                LKAssert(it + 1 == branch.patterns.end() && branch.patterns.size() == 1);
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
                                     isLastBranchBeforeWildcard && it + 1 == branch.patterns.end() ? nextValueBB : nextCondBB);
            }
        }
        
        Type *_initialTy = nullptr;
        if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&branch.expression, resultType, &_initialTy)) {
            LKFatalError("Invalid match branch result value: Type %s not compatible with expected type %s",
                         _initialTy->str().c_str(), resultType->str().c_str());
        }
        
        F->getBasicBlockList().push_back(valueBB);
        builder.SetInsertPoint(valueBB);
        branchMappings[valueBB] = codegen(branch.expression);
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



bool isValidBinopOperator(ast::Operator op) {
    using Op = ast::Operator;
    
    return op == Op::Add
        || op == Op::Sub
        || op == Op::Mul
        || op == Op::Div
        || op == Op::Mod
        || op == Op::And
        || op == Op::Or
        || op == Op::Xor
        || op == Op::Shl
        || op == Op::Shr
        || op == Op::And
        || op == Op::LOr
        || op == Op::EQ
        || op == Op::NE
        || op == Op::LT
        || op == Op::LE
        || op == Op::GT
        || op == Op::GE
        || op == Op::LAnd
        || op == Op::LOr;
}



llvm::Value* IRGenerator::codegen(std::shared_ptr<ast::BinOp> binop) {
    LKAssert(isValidBinopOperator(binop->getOperator()));
    
    auto callExpr = std::make_shared<ast::CallExpr>(makeIdent(mangling::mangleCanonicalName(binop->getOperator())),
                                                    std::vector<std::shared_ptr<ast::Expr>> { binop->getLhs(), binop->getRhs() });
    callExpr->setSourceLocation(binop->getSourceLocation());
    return codegen(callExpr);
    
}



bool IRGenerator::typecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *lhs, std::shared_ptr<ast::Expr> *rhs, Type **lhsTy_out, Type **rhsTy_out) {
    LKAssert(lhsTy_out && rhsTy_out);
    
    auto lhsTy = guessType(*lhs);
    auto rhsTy = guessType(*rhs);
    
    *lhsTy_out = lhsTy;
    *rhsTy_out = rhsTy;
    
    if (lhsTy == rhsTy) {
        return true;
    }
    
    // TODO add some kind of "types are compatible for this kind of binary operation" check
    
    if (!lhsTy->isNumericalTy() || !rhsTy->isNumericalTy()) {
        LKFatalError("oh no");
    }
    
    if (std::dynamic_pointer_cast<ast::NumberLiteral>(*lhs)) {
        // lhs is literal, cast to type of ths
        auto loc = (*lhs)->getSourceLocation();
        *lhs = std::make_shared<ast::CastExpr>(*lhs, ast::TypeDesc::makeResolved(rhsTy), ast::CastExpr::CastKind::StaticCast);
        (*lhs)->setSourceLocation(loc);
        *lhsTy_out = rhsTy;
    } else if (std::dynamic_pointer_cast<ast::NumberLiteral>(*rhs)) {
        // rhs is literal, cast to type of lhs
        auto loc = (*rhs)->getSourceLocation();
        *rhs = std::make_shared<ast::CastExpr>(*rhs, ast::TypeDesc::makeResolved(lhsTy), ast::CastExpr::CastKind::StaticCast);
        (*rhs)->setSourceLocation(loc);
        *rhsTy_out = lhsTy;
    } else {
        return false;
    }
    
    return true;
}





#pragma mark - function calls


uint8_t IRGenerator::argumentOffsetForCallingConvention(CallingConvention cc) {
    switch (cc) {
        case CallingConvention::C: return 0;
    }
}



// Q: Why does this return `std::shared_ptr<ast::TypeDesc>`, instead of `Type *`?
// A: Because this map is then passed on to the template specialization instantiator, which simply creates a bunch of AST nodes, and therefore needs TypeDescs

// Note that this function returns fully resolved ast::TypeDesc objects.
std::optional<std::map<std::string, std::shared_ptr<ast::TypeDesc>>> IRGenerator::attemptToResolveTemplateArgumentTypesForCall(std::shared_ptr<ast::FunctionDecl> templateFunction, std::shared_ptr<ast::CallExpr> call, unsigned argumentOffset) {
    // TODO properly take the argument offset into account when handling calls to templated instance methods, or other functions w/ an offset > 0
    
    const auto& sig = templateFunction->getSignature();
    if (sig.parameters.size() != call->arguments.size() + argumentOffset) {
        return std::nullopt;
    }
    
    
    // There are two ways a template argument type is resolved here:
    // 1. It was explicitly specified at the call site. Example: `foo<i32>();`
    // 2. We deduce it by looking at the arguments that were passed to the function
    //    In this case, there are two distinctions to be made: is the call argument a literal or a non-literal expression?
    //    Literals are given less importance than non-literal expressions. Why is that the case? consider the following example:
    //    We're calling a function with the signature `(T, T) -> T`. The arguments are `x` (of type i32) and `12` (of type i64 since it is a literal).
    //    Since implicit conversions are allowed for literals, and 12 fits in an i32, there is no reason to reject this call and the compiler
    //    needs to resolve `T` as i32, thus allowing an implicit conversion to take place later in function call codegen.
    //    To take scenarios like this into account, non-literal function parameters are given more "weight" than literals, and they can override
    //    a template argument type deduced from a literal expression.
    
    
    enum class DeductionReason {
        Expr,       // deduced from argument expr
        Literal,    // deduced from argument expr that is a literal
        Explicit    // explicitly set (ie `foo<i32, i64>();`)
    };
    
    struct TemplateArgumentDeductionInfo {
        Type *type;
        DeductionReason reason;
    };
    
    
    std::map<std::string, std::optional<TemplateArgumentDeductionInfo>> templateArgumentMapping;
    
    // Fill the map, taking explicitly passed template argument types into account
    // Template aguments not explicitly passed as part of the call are set to null
    for (size_t i = 0; i < sig.templateArgumentNames.size(); i++) {
        auto name = sig.templateArgumentNames[i];
        if (i < call->explicitTemplateArgumentTypes.size()) {
            templateArgumentMapping[name] = { resolveTypeDesc(call->explicitTemplateArgumentTypes[i]), DeductionReason::Explicit };
        } else {
            templateArgumentMapping[name] = std::nullopt;
        }
    }
    
    // TODO this needs a fundamental rewrite to support more than just nominal types and pointers to (pointers to) nominal types!
    // What about a pointer to a function, or a function that takes another functuin, etc etc etc
    
    
    for (size_t i = argumentOffset; i < call->arguments.size(); i++) {
        // We have to keep working w/ the ast::TypeDesc object as long as possible since calling resolveTypeDesc might resolve a typename shadowed by a template w/ some other type declared in the parent scope
        std::string paramTypename;
        auto paramType = sig.parameters[i]->type;
        uint64_t paramIndirectionCount = 0;
        
        if (paramType->isPointer()) {
            auto ty = paramType;
            while (ty->isPointer()) {
                paramIndirectionCount += 1;
                ty = ty->getPointee();
            }
            paramTypename = ty->getName();
        } else {
            paramTypename = paramType->getName();
        }
        
        if (auto mapping = templateArgumentMapping.find(paramTypename); mapping != templateArgumentMapping.end()) {
            auto guessedArgumentType = guessType(call->arguments[i]);
            auto isLiteral = call->arguments[i]->getNodeKind() == NK::NumberLiteral;
            auto reason = isLiteral ? DeductionReason::Literal : DeductionReason::Expr;
            if (!mapping->second.has_value()) {
                while (paramIndirectionCount-- > 0) {
                    LKAssert(guessedArgumentType->isPointerTy());
                    guessedArgumentType = llvm::dyn_cast<PointerType>(guessedArgumentType)->getPointee();
                }
                mapping->second = { guessedArgumentType, reason };
            } else {
                if (mapping->second->reason == DeductionReason::Literal) {
                    mapping->second = { guessedArgumentType, reason };
                } else if (!isLiteral && mapping->second->type != guessedArgumentType) {
                    emitError(call->arguments[i]->getSourceLocation(),
                              util::fmt_cstr("type mismatch: expected '%s', but expression evaluates to '%s'",
                                             mapping->second->type->str().c_str(), guessedArgumentType->str().c_str()));
                }
            }
        }
    }
    
    
    std::map<std::string, std::shared_ptr<ast::TypeDesc>> retvalMap;
    for (const auto& [name, deductionInfo] : templateArgumentMapping) {
        if (deductionInfo.has_value()) {
            retvalMap[name] = ast::TypeDesc::makeResolved(deductionInfo->type);
        } else {
            // TODO also print the location of the call! this is pretty useless without proper context
            emitError(templateFunction->getSourceLocation(), util::fmt_cstr("unable to deduce template argument '%s'", name.c_str()));
        }
    }
    return retvalMap;
}



ast::FunctionSignature makeFunctionSignatureFromFunctionTypeInfo(FunctionType *fnType) {
    ast::FunctionSignature sig;
    sig.returnType = ast::TypeDesc::makeResolved(fnType->getReturnType());
    sig.parameters = util::vector::map(fnType->getParameterTypes(), [](Type *ty) {
        return std::make_shared<ast::VarDecl>("", ast::TypeDesc::makeResolved(ty));
    });
    return sig;
}



ResolvedCallable IRGenerator::resolveCall(std::shared_ptr<ast::CallExpr> callExpr, bool omitCodegen) {
    std::string targetName;
    uint8_t argumentOffset = 0;
    
    if (auto ident = std::dynamic_pointer_cast<ast::Ident>(callExpr->target)) {
        targetName = ident->value;
        
        if (scope.contains(targetName)) {
            auto ty = scope.getType(targetName);
            LKAssert(ty->isFunctionTy() && "cannot call a non-function variable");
            auto fnTy = llvm::dyn_cast<FunctionType>(ty);
            return ResolvedCallable(makeFunctionSignatureFromFunctionTypeInfo(fnTy), nullptr,
                                    omitCodegen ? nullptr : codegen(ident),
                                    argumentOffsetForCallingConvention(fnTy->getCallingConvention()));
        }
        
    } else if (auto staticDeclRefExpr = std::dynamic_pointer_cast<ast::StaticDeclRefExpr>(callExpr->target)) {
        // <typename>::<methodname>()
        targetName = mangling::mangleCanonicalName(staticDeclRefExpr->typeName, staticDeclRefExpr->memberName, ast::FunctionKind::StaticMethod);
        
    } else if (auto memberExpr = std::dynamic_pointer_cast<ast::MemberExpr>(callExpr->target)) {
        // TODO does this take overloaded instance methods into account?
        
        // <memberExpr>()
        // two options:
        // - calling a method
        // - calling a property that happens to be a function
        
        auto targetTy = guessType(memberExpr->target);
        LKAssert(targetTy->isPointerTy());
        auto ptrTy = llvm::dyn_cast<PointerType>(targetTy);
        LKAssert(ptrTy->getPointee()->isStructTy());
        auto structTy = llvm::dyn_cast<StructType>(ptrTy->getPointee());
        auto structName = structTy->getName();
        
        if (auto [memberIndex, memberTy] = structTy->getMember(memberExpr->memberName); memberTy != nullptr) {
            LKAssert(memberTy->isFunctionTy() && "cannot call a non-function struct member");
            // struct properties cannot be overloaded, simply return what we found
            auto fnTy = llvm::dyn_cast<FunctionType>(memberTy);
            return ResolvedCallable(makeFunctionSignatureFromFunctionTypeInfo(fnTy), nullptr,
                                    omitCodegen ? nullptr : codegen(memberExpr),
                                    argumentOffsetForCallingConvention(fnTy->getCallingConvention()));
            
        } else {
            targetName = mangling::mangleCanonicalName(structName, memberExpr->memberName, ast::FunctionKind::InstanceMethod);
            argumentOffset = kInstanceMethodCallArgumentOffset;
        }
    } else {
        throw;
    }
    
    
    

    auto specializeTemplateFunctionForCall = [this, argumentOffset, omitCodegen] (std::shared_ptr<ast::FunctionDecl> functionDecl, std::map<std::string, std::shared_ptr<ast::TypeDesc>> templateArgumentMapping) -> ResolvedCallable {
        auto specializedDecl = TemplateResolver::specializeWithTemplateMapping(functionDecl, templateArgumentMapping);

        // Avoid generating the same specialization multiple times
        // In theory, we should never end up here since the call target resolution code should prefer the already-specialized version
        // over re-instantiating the template. However, the code is not very good and cannot (yet?) do that
        
        // We need the function's types fully resolved for the `mangleFullyResolved` call below
        resolveTypeDesc(specializedDecl->getSignature().returnType);
        for (auto param : specializedDecl->getSignature().parameters) {
            resolveTypeDesc(param->type);
        }
        
        auto mangled = mangleFullyResolved(specializedDecl);
        if (auto decl = getResolvedFunctionWithName(mangled)) {
            if (this->equal(specializedDecl->getSignature(), decl->funcDecl->getSignature())) {
                return decl.value();
            }
        }
        
        llvm::Function *llvmFunction = nullptr;
        if (!omitCodegen && !specializedDecl->getAttributes().intrinsic) {
            registerFunction(specializedDecl);
            llvmFunction = withCleanSlate([&]() { return llvm::dyn_cast<llvm::Function>(codegen(specializedDecl)); });
        }
        return ResolvedCallable(specializedDecl, llvmFunction, argumentOffset);
    };
    
    
    
    const auto& possibleTargets = functions[targetName];

    if (possibleTargets.empty()) {
        emitError(callExpr->getSourceLocation(), util::fmt_cstr("unable to resolve call to '%s'", targetName.c_str()));
    }
        
    if (possibleTargets.size() == 1) {
        const auto& target = possibleTargets[0];
        if (!target.funcDecl->getSignature().isTemplateFunction()) {
            return ResolvedCallable(target.funcDecl, target.llvmValue, argumentOffset);
        }
        
        // is a template functions
        
        auto templateArgumentMapping = attemptToResolveTemplateArgumentTypesForCall(target.funcDecl, callExpr, argumentOffset);
        LKAssert(templateArgumentMapping.has_value());
        return specializeTemplateFunctionForCall(target.funcDecl, templateArgumentMapping.value());
    }
    
    
    // more than one potential target
    
    
    struct FunctionResolutionMatchInfo {
        uint32_t score;
        std::shared_ptr<ast::FunctionDecl> decl;
        llvm::Value *llvmValue; // nullptr if this is a yet to be instantiated template function
        std::map<std::string, std::shared_ptr<ast::TypeDesc>> templateArgumentMapping; // fully resolved ast::TypeDescs!
    };
    
    
    // List of uninstantiated function templates that might be potential targets
    std::vector<std::shared_ptr<ast::FunctionDecl>> templateFunctions;
    // list of potential targets, with a score indicating how close they match the call
    std::vector<FunctionResolutionMatchInfo> matches;
    bool hasPerfectMatch = false;
    
    
    for (const auto& target : possibleTargets) {
        const auto& decl = target.funcDecl;
        const auto& sig = decl->getSignature();
        
        if (sig.parameters.size() != callExpr->arguments.size()) {
            continue;
        }
        
        if (sig.isTemplateFunction()) {
            templateFunctions.push_back(decl);
            continue;
        }
        
        uint32_t score = 0;
        
        for (size_t i = 0; i < callExpr->arguments.size(); i++) {
            auto arg = callExpr->arguments[i];
            auto argTy = guessType(arg);
            auto expectedTy = resolveTypeDesc(sig.parameters[i]->type);
            
            if (argTy == expectedTy) {
                score += 10;
            } else if (auto numberLiteral = std::dynamic_pointer_cast<ast::NumberLiteral>(arg)) {
                LKAssert(numberLiteral->type == ast::NumberLiteral::NumberType::Integer);
                if (valueIsTriviallyConvertibleTo(numberLiteral, expectedTy)) {
                    score += 5;
                }
            }
        }
        
        matches.push_back({score, decl, target.llvmValue, {}});
        
        if (score == callExpr->arguments.size() * 10) {
            // TODO does this mean we might miss other ambigious functions?
            hasPerfectMatch = true;
            break;
        }
    }
    
    if (!hasPerfectMatch) {
        for (const auto& target : templateFunctions) {
            if (auto templateArgMapping = attemptToResolveTemplateArgumentTypesForCall(target, callExpr, argumentOffset)) {
                auto argc = target->getSignature().parameters.size();
                uint32_t score = argc * 10;
                matches.push_back({score, target, nullptr, templateArgMapping.value()});
            } else {
//                std::cout << "(skipped bc unable to resolve): " << Target->signature << std::endl;
            }
        }
    }
    
    // TODO this seems like a bad idea
    std::sort(matches.begin(), matches.end(), [](auto& arg0, auto& arg1) { return arg0.score > arg1.score; });
    
#if 0
    std::cout << "Matching overloads for call to '" << targetName << "':\n";
    for (auto& match : matches) {
        std::cout << "- " << match.score << ": " << match.decl->getSignature() << std::endl;
    }
#endif
    
    if (matches.size() > 1 && matches[0].score == matches[1].score) {
        std::cout << "Error: ambiguous function call. unable to resolve. Potential candidates are:\n";
        for (auto& match : matches) {
            std::cout << "- " << match.score << ": " << match.decl->getSignature() << std::endl;
        }
        throw;
    }
    

    
    auto bestMatch = matches.front();
    
    if (bestMatch.decl->getSignature().isTemplateFunction() && !bestMatch.llvmValue) {
        return specializeTemplateFunctionForCall(bestMatch.decl, bestMatch.templateArgumentMapping);
    }
    return ResolvedCallable(bestMatch.decl->getSignature(), nullptr, bestMatch.llvmValue, argumentOffset);
}








bool callerCalleeSideEffectsCompatible(const std::vector<yo::attributes::SideEffect> &callerSideEffects,
                                       const std::vector<yo::attributes::SideEffect> &calleeSideEffects) {
    if (callerSideEffects.size() == 1 && callerSideEffects[0] == yo::attributes::SideEffect::Unknown) {
        return true;
    }
    
    for (const auto& sideEffect : calleeSideEffects) {
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
    
    
    if (auto calledFuncDecl = resolvedTarget.funcDecl) {
        if (!callerCalleeSideEffectsCompatible(currentFunction.decl->getAttributes().side_effects, calledFuncDecl->getAttributes().side_effects)) {
            auto targetName = mangling::mangleCanonicalName(calledFuncDecl);
            LKFatalError("cannot call '%s' because side effects", targetName.c_str());
        }
    }
    
    
    for (size_t i = resolvedTarget.argumentOffset; i < resolvedTarget.signature.parameters.size(); i++) {
        auto expectedType = resolveTypeDesc(resolvedTarget.signature.parameters[i]->type);
        auto expr = call->arguments[i - resolvedTarget.argumentOffset];
        Type *T;
        if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&expr, expectedType, &T)) {
            LKFatalError("Type mismatch in call. Arg #%zu: expected '%s', got '%s'",
                         i, expectedType->str().c_str(), T->str().c_str());
        }
        // TODO is modifying the arguments in-place necessarily a good idea?
        call->arguments[i - resolvedTarget.argumentOffset] = expr;
    }
    
    if (resolvedTarget.funcDecl && resolvedTarget.funcDecl->getAttributes().intrinsic) {
        return codegen_HandleIntrinsic(resolvedTarget.funcDecl, call);
    }
    
    
    
    llvm::Value *llvmFunction = resolvedTarget.llvmValue;
    LKAssert(llvmFunction->getType()->isPointerTy() && llvmFunction->getType()->getContainedType(0)->isFunctionTy());
    auto llvmFunctionTy = llvm::dyn_cast<llvm::FunctionType>(llvmFunction->getType()->getContainedType(0));
    auto isVariadic = llvmFunctionTy->isVarArg();
    
    LKAssert(call->arguments.size() >= llvmFunctionTy->getNumParams() - resolvedTarget.argumentOffset - isVariadic);
    
    std::vector<llvm::Value *> args(resolvedTarget.argumentOffset, nullptr);
    auto numFixedArgs = llvmFunctionTy->getNumParams() - resolvedTarget.argumentOffset;
    
    for (size_t i = resolvedTarget.argumentOffset; i < llvmFunctionTy->getNumParams(); i++) {
        auto expectedType = resolveTypeDesc(resolvedTarget.signature.parameters[i]->type);
        auto expr = call->arguments[i - resolvedTarget.argumentOffset];
        Type *T;
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
    
    if (isVariadic && getResolvedFunctionWithName(llvmFunction->getName().str())->funcDecl->getAttributes().extern_) {
        for (auto it = call->arguments.begin() + numFixedArgs; it != call->arguments.end(); it++) {
            args.push_back(codegen(*it));
        }
    } else if (isVariadic) {
        throw; // TODO implement
    }
    
    return builder.CreateCall(llvmFunction, args);
}





#pragma mark - Intrinsics

static const std::map<std::string_view, ast::Operator> intrinsicsArithmeticOperationMapping = {
    { "__add", ast::Operator::Add },
    { "__sub", ast::Operator::Sub },
    { "__mul", ast::Operator::Mul },
    { "__div", ast::Operator::Div },
    { "__mod", ast::Operator::Mod },
    { "__and", ast::Operator::And },
    { "__or",  ast::Operator::Or  },
    { "__xor", ast::Operator::Xor },
    { "__shl", ast::Operator::Shl },
    { "__shr", ast::Operator::Shr },
};

static const std::map<std::string_view, ast::Operator> intrinsicsComparisonOperationMapping = {
    { "__eq", ast::Operator::EQ },
    { "__lt", ast::Operator::LT },
    { "__gt", ast::Operator::GT }
};


llvm::Value *IRGenerator::codegen_HandleIntrinsic(std::shared_ptr<ast::FunctionDecl> funcDecl, std::shared_ptr<ast::CallExpr> call) {
    auto name = mangling::mangleCanonicalName(funcDecl);
    
    if (name == "static_cast" || name == "reinterpret_cast") {
        // TODO somehow use the SrcTy, if explicitly given?
        auto dstTy = call->explicitTemplateArgumentTypes[0];
        auto arg = call->arguments[0];
        auto castKind = name == "static_cast"
            ? ast::CastExpr::CastKind::StaticCast
            : ast::CastExpr::CastKind::Bitcast;
        auto castExpr = std::make_shared<ast::CastExpr>(arg, dstTy, castKind);
        castExpr->setSourceLocation(funcDecl->getSourceLocation());
        return codegen(castExpr);
    }
    
    if (name == "sizeof") {
        auto ty = resolveTypeDesc(call->explicitTemplateArgumentTypes[0])->getLLVMType();
        return llvm::ConstantInt::get(i64, module->getDataLayout().getTypeAllocSize(ty));
    }
    
    if (auto op = util::map::get_opt(intrinsicsArithmeticOperationMapping, std::string_view(name))) {
        LKAssert(call->arguments.size() == 2);
        return codegen_HandleArithmeticIntrinsic(op.value(), call->arguments[0], call->arguments[1]);
    }
    
    if (auto op = util::map::get_opt(intrinsicsComparisonOperationMapping, std::string_view(name))) {
        LKAssert(call->arguments.size() == 2);
        return codegen_HandleComparisonIntrinsic(op.value(), call->arguments[0], call->arguments[1]);
    }
    
    
    if (auto op = mangling::demangleCanonicalOperatorEncoding(name); op == ast::Operator::LAnd || op == ast::Operator::LOr) {
        LKAssert(call->arguments.size() == 2);
        return codegen_HandleLogOpIntrinsic(op, call->arguments[0], call->arguments[1]);
    }
    
    std::cout << "Unhandled call to intrinsic: " << name << std::endl;
    LKFatalError("");
}



llvm::Instruction::BinaryOps getLLVMBinaryOpInstruction_Int(ast::Operator op, bool isSigned) {
    using Op = ast::Operator;
    using LLVMBinOp = llvm::Instruction::BinaryOps;
    
    switch (op) {
        case Op::Add: return LLVMBinOp::Add;
        case Op::Sub: return LLVMBinOp::Sub;
        case Op::Mul: return LLVMBinOp::Mul;
        case Op::Div: return isSigned ? LLVMBinOp::SDiv : LLVMBinOp::UDiv;
        case Op::Mod: return isSigned ? LLVMBinOp::SRem : LLVMBinOp::URem;
        case Op::And: return LLVMBinOp::And;
        case Op::Or:  return LLVMBinOp::Or;
        case Op::Xor: return LLVMBinOp::And;
        case Op::Shl: return LLVMBinOp::Shl;
        case Op::Shr: return LLVMBinOp::LShr; // TODO (important) arithmetic or logical right shift?
        default: LKFatalError("");
    }
}

llvm::Instruction::BinaryOps getLLVMBinaryOpInstruction_Float(ast::Operator op) {
    using Op = ast::Operator;
    using LLVMBinOp = llvm::Instruction::BinaryOps;
    
    switch (op) {
        case Op::Add: return LLVMBinOp::FAdd;
        case Op::Sub: return LLVMBinOp::FSub;
        case Op::Mul: return LLVMBinOp::FMul;
        case Op::Div: return LLVMBinOp::FDiv;
        case Op::Mod: return LLVMBinOp::FRem;
        default: LKFatalError("");
    }
}



bool isValidIntArithBinop(ast::Operator op) {
    using Op = ast::Operator;
    return op == Op::Add
        || op == Op::Sub
        || op == Op::Mul
        || op == Op::Div
        || op == Op::Mod
        || op == Op::And
        || op == Op::Or
        || op == Op::Xor
        || op == Op::Shl
        || op == Op::Shr;
}

bool isValidFloatArithBinop(ast::Operator op) {
    using Op = ast::Operator;
    return op == Op::Add
        || op == Op::Sub
        || op == Op::Mul
        || op == Op::Div;
}


llvm::Value* IRGenerator::codegen_HandleArithmeticIntrinsic(ast::Operator op, std::shared_ptr<ast::Expr> lhs, std::shared_ptr<ast::Expr> rhs) {
    emitDebugLocation(lhs);
    
    Type *lhsTy, *rhsTy;
    
    if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&lhs, &rhs, &lhsTy, &rhsTy)) {
        LKFatalError("unable to create binop for supplied operand types '%s' and '%s'", lhsTy->str().c_str(), rhsTy->str().c_str());
    }
    
    LKAssert(lhsTy->isNumericalTy() && rhsTy->isNumericalTy());
    LKAssert(lhsTy == rhsTy);
    auto numTy = llvm::dyn_cast<NumericalType>(lhsTy);
    
    if (numTy->isIntegerTy() || numTy->isBoolTy()) {
        LKAssert(isValidIntArithBinop(op));
    } else if (numTy->isFloatTy()) {
        LKAssert(isValidFloatArithBinop(op));
    } else {
        LKAssert("TODO: invalid operand type?");
    }
    
    auto llvmOp = numTy->isFloatTy() ? getLLVMBinaryOpInstruction_Float(op) : getLLVMBinaryOpInstruction_Int(op, numTy->isSigned());
    return builder.CreateBinOp(llvmOp, codegen(lhs), codegen(rhs));
}



llvm::CmpInst::Predicate getMatchingLLVMCmpInstPredicateForComparisonOperator_Int(ast::Operator op, bool isSigned) {
    using Op = ast::Operator;
    using Pred = llvm::CmpInst::Predicate;
    
    switch (op) {
        case Op::EQ: return Pred::ICMP_EQ;
        case Op::NE: return Pred::ICMP_NE;
        case Op::LT: return isSigned ? Pred::ICMP_SLT : Pred::ICMP_ULT;
        case Op::LE: return isSigned ? Pred::ICMP_SLE : Pred::ICMP_ULE;
        case Op::GT: return isSigned ? Pred::ICMP_SGT : Pred::ICMP_UGT;
        case Op::GE: return isSigned ? Pred::ICMP_SGE : Pred::ICMP_UGE;
        default: LKFatalError("");
    }
}


llvm::CmpInst::Predicate getMatchingLLVMCmpInstPredicateForComparisonOperator_Float(ast::Operator op) {
    using Op = ast::Operator;
    using Pred = llvm::CmpInst::Predicate;
    
    switch (op) {
        case Op::EQ: return Pred::FCMP_OEQ;
        case Op::NE: return Pred::FCMP_ONE;
        case Op::LT: return Pred::FCMP_OLT;
        case Op::LE: return Pred::FCMP_OLE;
        case Op::GT: return Pred::FCMP_OGT;
        case Op::GE: return Pred::FCMP_OGE;
        default: LKFatalError("");
    }
}



llvm::Value* IRGenerator::codegen_HandleComparisonIntrinsic(ast::Operator op, std::shared_ptr<ast::Expr> lhs, std::shared_ptr<ast::Expr> rhs) {
    emitDebugLocation(lhs);
    
    auto lhsTy = guessType(lhs);
    auto rhsTy = guessType(rhs);
    
    llvm::CmpInst::Predicate pred;
    llvm::Value *lhsVal, *rhsVal;
    
    // Floats?
    if (lhsTy == rhsTy && lhsTy == Type::getFloat64Type()) {
        return builder.CreateFCmp(getMatchingLLVMCmpInstPredicateForComparisonOperator_Float(op),
                                  codegen(lhs), codegen(rhs));
    }
    
    // Are both integers?
    if (!(lhsTy->isNumericalTy() && rhsTy->isNumericalTy())) {
        LKFatalError("Cannot compare unrelated types '%s' and '%s'", rhsTy->str().c_str(), rhsTy->str().c_str());
    }
    
    auto numTyLhs = llvm::dyn_cast<NumericalType>(lhsTy);
    auto numTyRhs = llvm::dyn_cast<NumericalType>(rhsTy);
    
    if (numTyLhs == numTyRhs) {
        pred = getMatchingLLVMCmpInstPredicateForComparisonOperator_Int(op, numTyLhs->isSigned());
        lhsVal = codegen(lhs);
        rhsVal = codegen(rhs);
    } else {
        // Both are integers, but different types
        
        Type *castDestTy;
        auto largerSize = std::max(numTyLhs->getSize(), numTyRhs->getSize());
        
        if (largerSize <= Type::getInt32Type()->getSize()) {
            castDestTy = Type::getInt32Type();
        } else {
            LKAssert(largerSize == Type::getInt64Type()->getSize());
            castDestTy = Type::getInt64Type();
        }
        
        lhsVal = codegen(std::make_shared<ast::CastExpr>(lhs, ast::TypeDesc::makeResolved(castDestTy), ast::CastExpr::CastKind::StaticCast));
        rhsVal = codegen(std::make_shared<ast::CastExpr>(rhs, ast::TypeDesc::makeResolved(castDestTy), ast::CastExpr::CastKind::StaticCast));
        pred = getMatchingLLVMCmpInstPredicateForComparisonOperator_Int(op, numTyLhs->isSigned() || numTyRhs->isSigned());
        
    }
    
    return builder.CreateICmp(pred, lhsVal, rhsVal);
}





llvm::Value* IRGenerator::codegen_HandleLogOpIntrinsic(ast::Operator op, std::shared_ptr<ast::Expr> lhs, std::shared_ptr<ast::Expr> rhs) {
    LKAssert(op == ast::Operator::LAnd || op == ast::Operator::LOr);
    LKAssert(guessType(lhs) == Type::getBoolType() && guessType(rhs) == Type::getBoolType());
    
    const auto isAnd = op == ast::Operator::LAnd;
    
    auto llvmTrueVal = llvm::ConstantInt::getTrue(i1);
    auto llvmFalseVal = llvm::ConstantInt::getFalse(i1);
    auto F = currentFunction.llvmFunction;
    
    auto lhsBB = builder.GetInsertBlock();
    auto rhsBB = llvm::BasicBlock::Create(C, "rhs");
    auto mergeBB = llvm::BasicBlock::Create(C, "merge");
    
    builder.CreateCondBr(builder.CreateICmpEQ(codegen(lhs), llvmTrueVal),
                         isAnd ? rhsBB : mergeBB,
                         isAnd ? mergeBB : rhsBB);
    
    
    F->getBasicBlockList().push_back(rhsBB);
    builder.SetInsertPoint(rhsBB);
    auto rhsVal = builder.CreateICmpEQ(codegen(rhs), llvmTrueVal);
    builder.CreateBr(mergeBB);
    
    F->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);
    
    auto phi = builder.CreatePHI(i1, 2);
    phi->addIncoming(isAnd ? llvmFalseVal : llvmTrueVal, lhsBB);
    phi->addIncoming(rhsVal, rhsBB);
    
    return phi;
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
    
    for (const auto& branch : ifStmt->branches) {
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
    
    // TODO the code below is pretty bad
    
    LKFatalError("TODO");
    
//    auto T = guessType(forLoop->expr);
//    LKAssert(T->isPointer() && T->getPointee()->isComplex());
//    auto iteratorCallTarget = mangling::mangleCanonicalName(T->getPointee()->getName(), "iterator", ast::FunctionSignature::FunctionKind::InstanceMethod);
//
//    auto call = std::make_shared<ast::CallExpr>(std::make_shared<ast::Ident>(iteratorCallTarget),
//                                                std::vector<std::shared_ptr<ast::Expr>>{ forLoop->expr });
//
//
//    auto forStmtScope = std::make_shared<ast::Composite>();
//    auto it_ident = std::make_shared<ast::Ident>("$it");
//    forStmtScope->statements.push_back(std::make_shared<ast::VarDecl>(it_ident, TypeInfo::Unresolved, call));
//
//    // while loop
//    auto callInstanceMethod = [](const std::shared_ptr<ast::Ident> &target, const std::string &methodName) {
//        return std::make_shared<ast::CallExpr>(std::make_shared<ast::MemberExpr>(target, methodName));
//    };
//
//    auto whileBody = std::make_shared<ast::Composite>();
//    whileBody->statements.push_back(std::make_shared<ast::VarDecl>(forLoop->ident, TypeInfo::Unresolved, callInstanceMethod(it_ident, "next")));
//    vec_append(whileBody->statements, forLoop->body->statements);
//    forStmtScope->statements.push_back(std::make_shared<ast::WhileStmt>(callInstanceMethod(it_ident, "hasNext"), whileBody));
//    return codegen(forStmtScope);
}








#pragma mark - Globals


void IRGenerator::handleStartupAndShutdownFunctions() {
    std::vector<llvm::Type *> structTys = {
        i32, llvm::FunctionType::get(Void, false)->getPointerTo(), i8_ptr
    };
    auto structTy = llvm::StructType::get(C, structTys);
    
    
    
    auto imp = [&](llvm::StringRef dest, bool attributes::FunctionAttributes::* attr) {
        std::vector<ResolvedCallable> functions;
        
        for (const auto &[name, callable] : resolvedFunctions) {
            if (callable.funcDecl && callable.funcDecl->getAttributes().*attr) {
                functions.push_back(callable);
            }
        }
        
        auto arrayTy = llvm::ArrayType::get(structTy, functions.size());
        module->getOrInsertGlobal(dest, arrayTy);
        
        std::vector<llvm::Constant *> arrayElements;
        for (const auto &fn : functions) {
            std::vector<llvm::Constant *> values = {
                llvm::ConstantInt::get(i32, 65535), // TODO how should this be ordered?
                llvm::dyn_cast<llvm::Function>(fn.llvmValue),
                llvm::ConstantPointerNull::get(llvm::dyn_cast<llvm::PointerType>(i8_ptr))
            };
            arrayElements.push_back(llvm::ConstantStruct::get(structTy, values));
        }
        
        auto array = llvm::ConstantArray::get(arrayTy, arrayElements);
        auto global = module->getGlobalVariable(dest);
        global->setInitializer(array);
        global->setLinkage(llvm::GlobalVariable::LinkageTypes::AppendingLinkage);
        
    };
    
    
    imp("llvm.global_ctors", &attributes::FunctionAttributes::startup);
    imp("llvm.global_dtors", &attributes::FunctionAttributes::shutdown);
}





#pragma mark - Types


llvm::Type *IRGenerator::getLLVMType(Type *type) {
    if (auto T = type->getLLVMType()) return T;
    
    auto handle_llvm_type = [type](llvm::Type *llvmTy) -> llvm::Type* {
        type->setLLVMType(llvmTy);
        return llvmTy;
    };
    
    switch (type->getTypeId()) {
        case Type::TypeID::Void:
            return handle_llvm_type(Void);
        
        case Type::TypeID::Numerical: {
            auto numTy = llvm::dyn_cast<NumericalType>(type);
            switch (numTy->getNumericalTypeID()) {
                case NumericalType::NumericalTypeID::Bool:
                    return handle_llvm_type(i1);
                
                case NumericalType::NumericalTypeID::Int8:
                case NumericalType::NumericalTypeID::UInt8:
                    return handle_llvm_type(i8);
                
                case NumericalType::NumericalTypeID::Int16:
                case NumericalType::NumericalTypeID::UInt16:
                    return handle_llvm_type(i16);
                
                case NumericalType::NumericalTypeID::Int32:
                case NumericalType::NumericalTypeID::UInt32:
                    return handle_llvm_type(i32);
                
                case NumericalType::NumericalTypeID::Int64:
                case NumericalType::NumericalTypeID::UInt64:
                    return handle_llvm_type(i64);
                
                case NumericalType::NumericalTypeID::Float64:
                    return handle_llvm_type(Double);
            }
        }
        
        case Type::TypeID::Pointer: {
            //auto ptrTy = static_cast<PointerType *>(type);
            uint64_t numIndirections = 0;
            
            Type *ty = type;
            while (ty->isPointerTy()) {
                numIndirections += 1;
                ty = llvm::dyn_cast<PointerType>(ty)->getPointee();
            }
            
            auto llvmType = getLLVMType(ty);
            while (numIndirections--) {
                llvmType = llvmType->getPointerTo();
            }
            
            return handle_llvm_type(llvmType);
        }
        
        case Type::TypeID::Struct: {
            auto structTy = llvm::dyn_cast<StructType>(type);
            auto llvmStructTy = llvm::StructType::create(C, structTy->getName());
            llvmStructTy->setBody(util::vector::map(structTy->getMembers(), [this](const auto& member) -> llvm::Type* {
                return getLLVMType(member.second);
            }));
            return handle_llvm_type(llvmStructTy);
        }
        
        case Type::TypeID::Function: {
            auto fnTy = llvm::dyn_cast<FunctionType>(type);
            auto paramTypes = util::vector::map(fnTy->getParameterTypes(), [this](auto ty) { return getLLVMType(ty); });
            auto llvmFnTy = llvm::FunctionType::get(getLLVMType(fnTy->getReturnType()), paramTypes, false); // TODO support variadic function types?
            return handle_llvm_type(llvmFnTy->getPointerTo());
        }
    }
    
    LKFatalError("should never reach here");
}



// IDEA:
// - give every Type* a reference to the irgen object
// - calling getLLVM{DI}Type will - if necessary - simply forward to the irgen's getLLVMType function, which then creates the type
// pro: not hwving to worry about whether a Type has its llvm types set
// con: this would make using primitive types objects in the parser a bit more difficult?



llvm::DIType* IRGenerator::getDIType(Type *type) {
    if (auto ty = type->getLLVMDIType()) return ty;
    
    auto handle_di_type = [type](llvm::DIType *diType) -> llvm::DIType* {
        type->setLLVMDIType(diType);
        return diType;
    };
    
    auto& DL = module->getDataLayout();
    auto& builder = debugInfo.builder;
    
    //auto byteWidth = DL.getTypeSizeInBits(i8);
    auto pointerWidth = DL.getPointerSizeInBits();
    
    switch (type->getTypeId()) {
        case Type::TypeID::Void:
            return nullptr;
        
        case Type::TypeID::Pointer: {
            auto pointee = llvm::dyn_cast<PointerType>(type)->getPointee();
            return handle_di_type(builder.createPointerType(getDIType(pointee), pointerWidth));
        }
        
        case Type::TypeID::Numerical: {
            auto numTy = llvm::dyn_cast<NumericalType>(type);
            auto ty = builder.createBasicType(numTy->getName(), numTy->getPrimitiveSizeInBits(),
                                              numTy->isSigned() ? llvm::dwarf::DW_ATE_signed : llvm::dwarf::DW_ATE_unsigned);
            return handle_di_type(ty);
        }
        
        case Type::TypeID::Function: {
            auto fnTy = llvm::dyn_cast<FunctionType>(type);
            
            std::vector<llvm::Metadata *> paramTypes;
            paramTypes.reserve(fnTy->getNumberOfParameters());
            
            paramTypes.push_back(getDIType(fnTy->getReturnType()));
            for (auto paramTy : fnTy->getParameterTypes()) {
                paramTypes.push_back(getDIType(paramTy));
            }
            
            auto diFnTy = builder.createSubroutineType(builder.getOrCreateTypeArray(paramTypes));
            auto ty = builder.createPointerType(diFnTy, pointerWidth);
            return handle_di_type(ty);
        }
        
        case Type::TypeID::Struct: {
            auto structTy = llvm::dyn_cast<StructType>(type);
            auto llvmStructTy = llvm::dyn_cast<llvm::StructType>(getLLVMType(structTy));
            auto llvmStructTyLayout = DL.getStructLayout(llvmStructTy);
            auto unit = DIFileForSourceLocation(builder, structTy->getSourceLocation());
            
            std::vector<llvm::Metadata *> llvmMembers = util::vector::mapi(structTy->getMembers(), [&](auto idx, auto& member) -> llvm::Metadata* {
                auto llvmMemberTy = getLLVMType(member.second);
                return  builder.createMemberType(unit, member.first, unit, 0, // TODO struct member line number?
                                                 DL.getTypeSizeInBits(llvmMemberTy), DL.getPrefTypeAlignment(llvmMemberTy),
                                                 llvmStructTyLayout->getElementOffsetInBits(idx),
                                                 llvm::DINode::DIFlags::FlagZero, getDIType(member.second));
            });
            
            auto ty = builder.createStructType(unit, structTy->getName(), unit, structTy->getSourceLocation().line,
                                               DL.getTypeSizeInBits(llvmStructTy), DL.getPrefTypeAlignment(llvmStructTy),
                                               llvm::DINode::DIFlags::FlagZero, nullptr, builder.getOrCreateArray(llvmMembers));
            return handle_di_type(ty);
        }
    }
    
    LKFatalError("should never reach here");
}








bool IRGenerator::valueIsTriviallyConvertibleTo(std::shared_ptr<ast::NumberLiteral> number, Type *type) {
    using NT = ast::NumberLiteral::NumberType;
    
    if (!type->isNumericalTy()) return false; // TODO is this too strict?
    
    // Allowed trivial conversions:
    // int literal to any int type (as long as the value fits)
    // int literal to double
    
    if (number->type == NT::Boolean) return type == Type::getBoolType();
    
    if (type == Type::getFloat64Type()) {
        return number->type == NT::Double || number->type == NT::Integer;
    }
    
    if (!type->isNumericalTy()) return false;
    
    LKAssert(number->type == NT::Integer && type->isNumericalTy());
    auto numTy = llvm::dyn_cast<NumericalType>(type);
    LKAssert(numTy->isIntegerTy());
    LKAssert(number->value >= 0); // TODO whatthefuc? this will never be false since ast::NumberLitera::Value is unsigned!!!!!
    
    auto value = number->value;
    uint8_t bitCount = 0;
    while (value != 0) { ++bitCount; value >>= 1; }
    
    return bitCount <= numTy->getPrimitiveSizeInBits();
}




Type* IRGenerator::guessType(std::shared_ptr<ast::Expr> expr) {
    switch (expr->getNodeKind()) {
        case NK::NumberLiteral: {
            using NT = ast::NumberLiteral::NumberType;
            auto numberLiteral = std::static_pointer_cast<ast::NumberLiteral>(expr);
            switch (numberLiteral->type) {
                case NT::Boolean:   return Type::getBoolType();
                case NT::Integer:   return Type::getInt64Type();
                case NT::Character: return Type::getUInt8Type(); // TODO introduce a dedicated char type?
                case NT::Double:    return Type::getFloat64Type();
            }
        }
        
        case NK::StringLiteral: {
            using SLK = ast::StringLiteral::StringLiteralKind;
            switch (static_cast<ast::StringLiteral *>(expr.get())->kind) {
                case SLK::ByteString: return Type::getInt8Type()->getPointerTo();
                case SLK::NormalString: {
                    auto StringTy = util::map::get_opt(nominalTypes, std::string("String"));
                    LKAssert(StringTy.has_value());
                    return StringTy.value();
                }
            }
        }
        
        case NK::Ident:
            return scope.getType(std::static_pointer_cast<ast::Ident>(expr)->value);
        
        case NK::CastExpr:
            return resolveTypeDesc(static_cast<ast::CastExpr *>(expr.get())->destType);
        
        case NK::CallExpr:
            return resolveTypeDesc(resolveCall(std::static_pointer_cast<ast::CallExpr>(expr), true).signature.returnType);
        
        case NK::MatchExpr:
            return guessType(static_cast<ast::MatchExpr *>(expr.get())->branches.front().expression); // TODO add a check somewhere to make sure all branches return the same type
        
        case NK::RawLLVMValueExpr:
            return static_cast<ast::RawLLVMValueExpr *>(expr.get())->type;
        
        case NK::SubscriptExpr: {
            // TODO allow non-pointer subscripting
            auto targetTy = guessType(static_cast<ast::SubscriptExpr *>(expr.get())->target);
            LKAssert(targetTy->isPointerTy());
            return llvm::dyn_cast<PointerType>(targetTy)->getPointee();
        }
        
        case NK::MemberExpr: {
            auto memberExpr = static_cast<ast::MemberExpr *>(expr.get());
            auto targetTy = guessType(memberExpr->target);
            LKAssert(targetTy->isPointerTy());
            auto ptrTy = llvm::dyn_cast<PointerType>(targetTy);
            LKAssert(ptrTy->getPointee()->isStructTy());
            auto structTy = llvm::dyn_cast<StructType>(ptrTy->getPointee());
            auto memberTy = structTy->getMember(memberExpr->memberName).second;
            if (!memberTy) {
                emitError(memberExpr->getSourceLocation(),
                          util::fmt_cstr("Struct '%s' does not have a member named '%s'",
                                         structTy->getName().c_str(), memberExpr->memberName.c_str()));
            }
            return memberTy;
        }
        
        case NK::UnaryExpr: {
            return guessType(static_cast<ast::UnaryExpr *>(expr.get())->expr);
        }
        
        case NK::CompOp:
            return Type::getBoolType();
        
        case NK::BinOp: {
            auto binopExpr = static_cast<ast::BinOp *>(expr.get());
            auto mangledCanonicalName = mangling::mangleCanonicalName(binopExpr->getOperator());
            // TODO don't allocate an object for every check!
            auto tempCallExpr = std::make_shared<ast::CallExpr>(makeIdent(mangledCanonicalName),
                                                                std::vector<std::shared_ptr<ast::Expr>>{ binopExpr->getLhs(), binopExpr->getRhs() });
            tempCallExpr->setSourceLocation(binopExpr->getSourceLocation());
            return resolveTypeDesc(resolveCall(tempCallExpr, true).signature.returnType);
        }
        
        default:
            unhandled_node(expr);
            LKFatalError("TODO");
    }
}





Type *IRGenerator::instantiateTemplatedType(std::shared_ptr<ast::TypeDesc> typeDesc) {
    LKFatalError("TODO");
    
//    if (!TI->isTemplatedType()) return TI;
//
//    auto templateStructDecl = typeCache.getStruct(TI->getName());
//    LKAssert(templateStructDecl->isTemplateStruct());
//    std::map<std::string, TypeInfo *> mapping;
//
//    for (size_t i = 0; i < templateStructDecl->templateArguments.size(); i++) {
//        mapping[templateStructDecl->templateArguments[i]] = TI->getTemplateParameterTypes()[i];
//    }
//
//    auto mangledName = mangling::mangleTemplatedComplexType(TI);
//
//
//
//    LKFatalError("TODO");
}








#pragma mark - Errors


void IRGenerator::emitError(const parser::TokenSourceLocation& loc, std::string message) {
    if (loc.empty()) {
        std::cout << message << std::endl;
    } else {
        std::cout << loc.filepath << ":" << loc.line << ":" << loc.column << ": " << message << std::endl;
        std::cout << util::fs::read_specific_line(loc.filepath, loc.line - 1) << std::endl;
        
        for (auto i = 0; i < loc.column - 1; i++) {
            std::cout << ' ';
        }
        std::cout << "^" << std::endl;
    }
    
    LKFatalError("");
}






#pragma mark - Synthesized Functions


namespace astgen {
    using namespace ast;
    
    std::shared_ptr<Ident> ident(std::string value) {
        return std::make_shared<Ident>(value);
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
    
    std::shared_ptr<CastExpr> cast(std::shared_ptr<Expr> expr, Type *ty) {
        return std::make_shared<CastExpr>(expr, ast::TypeDesc::makeResolved(ty), CastExpr::CastKind::StaticCast);
    }
}


llvm::Value *IRGenerator::generateStructInitializer(std::shared_ptr<ast::StructDecl> structDecl) {
    const auto& structName = structDecl->name;
    auto structType = llvm::dyn_cast<StructType>(nominalTypes[structName]);

    auto F = functions[mangling::mangleCanonicalName(structName, "init", ast::FunctionKind::StaticMethod)][0].funcDecl;
    
    std::vector<std::shared_ptr<ast::LocalStmt>> fnBody;

    auto self = std::make_shared<ast::Ident>("self");

    // allocate object
    {
        auto allocCall = std::make_shared<ast::CallExpr>(astgen::ident("alloc"), astgen::exprVec({
            //astgen::Number(M->getDataLayout().getTypeAllocSize(getLLVMType(T)))
            astgen::number(1)
        }));
        allocCall->explicitTemplateArgumentTypes = { ast::TypeDesc::makeResolved(structType) };
        fnBody.push_back(std::make_shared<ast::VarDecl>(self->value, ast::TypeDesc::makeResolved(structType->getPointerTo()), allocCall));
    }

    // set runtime metadata
    if (options.enableARC && structDecl->attributes.arc) {
        auto set_retaincount = std::make_shared<ast::Assignment>(std::make_shared<ast::MemberExpr>(self, "retainCount"),
                                                                 astgen::number((uint64_t(1) << 60) | 1));

        auto sel = mangling::mangleCanonicalName(structName, "dealloc", ast::FunctionKind::InstanceMethod);
        auto dealloc_fn = functions[sel][0].llvmValue;

        llvm::Type *t[] = { i8_ptr };
        auto dealloc_fn_ty = llvm::FunctionType::get(Void, t, false)->getPointerTo();
        auto dealloc_fn_cast = builder.CreateBitCast(dealloc_fn, dealloc_fn_ty);
        auto set_deallocFn = std::make_shared<ast::Assignment>(std::make_shared<ast::MemberExpr>(self, "deallocPtr"),
                                                               std::make_shared<ast::RawLLVMValueExpr>(dealloc_fn_cast,
                                                                                                       structType->getMembers()[1].second));

        fnBody.push_back(set_retaincount);
        fnBody.push_back(set_deallocFn);
    }

    // set properties
    for (auto& param : F->getSignature().parameters) {
        fnBody.push_back(std::make_shared<ast::Assignment>(std::make_shared<ast::MemberExpr>(self, param->name), makeIdent(param->name)));
    }
    fnBody.push_back(std::make_shared<ast::ReturnStmt>(self));
    
    F->setBody(fnBody);

    return codegen(F);
}
