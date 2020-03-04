//
//  IRGen.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "IRGen.h"

#include "Mangling.h"
#include "util_llvm.h"
#include "TemplateSpecialization.h"
#include "Attributes.h"
#include "Diagnostics.h"

#include <optional>
#include <limits>
#include <set>


using namespace yo;
using namespace yo::irgen;


using NK = ast::Node::Kind;


inline constexpr unsigned kInstanceMethodCallArgumentOffset = 1;

static const std::string kInitializerMethodName = "init";
static const std::string kSynthesizedDeallocMethodName = "__dealloc";
static const std::string kRetvalAllocaIdentifier = "__retval";
static const std::string kIteratorMethodName = "iterator";
static const std::string kIteratorHasNextMethodName = "hasNext";
static const std::string kIteratorNextMethodName = "next";

#define unhandled_node(node) \
{ std::cout << __PRETTY_FUNCTION__ << ": Unhandled Node: " << util::typeinfo::getTypename(*(node)) << std::endl; \
throw; }




std::shared_ptr<ast::Ident> makeIdent(const std::string& str, ast::TokenSourceLocation SL = ast::TokenSourceLocation()) {
    auto ident = std::make_shared<ast::Ident>(str);
    ident->setSourceLocation(SL);
    return ident;
}


std::shared_ptr<ast::Ident> formatTupleMemberAtIndex(size_t index) {
    return makeIdent(util::fmt::format("__{}", index));
}


// TODO is this a good idea?
// Note: this assumes that the type has already been fully initialized (important for tuples and lambdas)
StructType* getUnderlyingStruct(Type *ty) {
    auto handle = [](Type *type) -> StructType* {
        if (!type) {
            return nullptr;
        }
        if (auto tupleTy = llvm::dyn_cast<TupleType>(type)) {
            return tupleTy->getUnderlyingStructType();
        } else {
            return llvm::dyn_cast<StructType>(type);
        }
    };
    
    switch (ty->getTypeId()) {
        case Type::TypeID::Struct:
            return llvm::cast<StructType>(ty);
        
        case Type::TypeID::Pointer:
            return handle(llvm::cast<PointerType>(ty)->getPointee());
        
        case Type::TypeID::Reference:
            return handle(llvm::cast<ReferenceType>(ty)->getReferencedType());
        
        case Type::TypeID::Tuple:
            return llvm::cast<TupleType>(ty)->getUnderlyingStructType();
        
        default:
            return nullptr;
    }
}




// IRGen

llvm::LLVMContext IRGenerator::C;

IRGenerator::IRGenerator(const std::string &translationUnitPath, const driver::Options &options)
    : module(llvm::make_unique<llvm::Module>(util::fs::path_utils::getFilename(translationUnitPath), C)),
    builder(C),
    debugInfo{llvm::DIBuilder(*module), nullptr, {}},
    driverOptions(options)
{
    builtinTypes.llvm = {
        .i8     = llvm::Type::getInt8Ty(C),
        .i16    = llvm::Type::getInt16Ty(C),
        .i32    = llvm::Type::getInt32Ty(C),
        .i64    = llvm::Type::getInt64Ty(C),
        .i1     = llvm::Type::getInt1Ty(C),
        .Void   = llvm::Type::getVoidTy(C),
        .Float  = llvm::Type::getFloatTy(C),
        .Double = llvm::Type::getDoubleTy(C),
        .i8Ptr  = llvm::Type::getInt8PtrTy(C)
    };
    
    // create all primitives' llvm::Type and llvm::DIType objects
    Type::initPrimitives();
    
    auto preflight_type = [&](auto *type) -> auto* {
        // getLLVM{DI}Type() will also set the respective member fields in the type object
        getLLVMType(type);
        getDIType(type);
        return type;
    };
    builtinTypes.yo = {
        .u8    = preflight_type(Type::getUInt8Type()),
        .u16   = preflight_type(Type::getUInt16Type()),
        .u32   = preflight_type(Type::getUInt32Type()),
        .u64   = preflight_type(Type::getUInt64Type()),
        .i8    = preflight_type(Type::getInt8Type()),
        .i16   = preflight_type(Type::getInt16Type()),
        .i32   = preflight_type(Type::getInt32Type()),
        .i64   = preflight_type(Type::getInt64Type()),
        .Bool  = preflight_type(Type::getBoolType()),
        .f32   = preflight_type(Type::getFloat32Type()),
        .f64   = preflight_type(Type::getFloat64Type()),
        .Void  = preflight_type(Type::getVoidType()),
        .i8Ptr = preflight_type(Type::getInt8Type()->getPointerTo())
    };
    
    const auto [path, filename] = util::string::extractPathAndFilename(translationUnitPath);
    module->setSourceFileName(filename);
    
    debugInfo.compileUnit = debugInfo.builder.createCompileUnit(llvm::dwarf::DW_LANG_C,
                                                                debugInfo.builder.createFile(filename, path),
                                                                "yo", driverOptions.optimize, "", 0);
    debugInfo.lexicalBlocks.push_back(debugInfo.compileUnit);
    module->addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
}



void IRGenerator::emitDebugLocation(const std::shared_ptr<ast::Node> &node) {
    if (!shouldEmitDebugInfo()) return;
    
    if (!node || node->getSourceLocation().empty())  {
        builder.SetCurrentDebugLocation(llvm::DebugLoc());
        return;
    }
    const auto &SL = node->getSourceLocation();
    builder.SetCurrentDebugLocation(llvm::DebugLoc::get(SL.line, SL.column, debugInfo.lexicalBlocks.back()));
}




void IRGenerator::codegen(const ast::AST& ast) {
    preflight(ast);
    
    for (const auto &node : ast) {
        codegen(node);
    }
    
    for (const auto &[name, decl] : structDecls) {
        if (decl->attributes.no_init) continue;
        
        synthesizeDefaultMemberwiseInitializer(decl, kRunCodegen);
        synthesizeDefaultCopyConstructor(decl, kRunCodegen);
        synthesizeDefaultDeallocMethod(decl, kRunCodegen);
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
    
#define CASE(node, kind, dest) case NK::kind: { dest.push_back(llvm::dyn_cast<ast::kind>(node)); continue; }
    for (const auto& node : ast) {
        switch(node->getKind()) {
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
        nominalTypes.insert(typealiasDecl->typename_, resolveTypeDesc(typealiasDecl->type));
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
        
        auto insert = [&](std::map<std::string, std::shared_ptr<ast::StructDecl>> &decls, bool shouldMangle) {
            auto name = shouldMangle ? mangling::mangleAsStruct(implBlock->typeDesc->getName()) : implBlock->typeDesc->getName();
            if (!util::map::has_key(decls, name)) {
                diagnostics::emitError(implBlock->getSourceLocation(), "unable to find corresponding type");
            }
            decls[name]->implBlocks.push_back(implBlock);
        };
        
        if (implBlock->isNominalTemplateType) {
            insert(templateStructs, false);
        } else {
            insert(this->structDecls, true);
        }
    }
}


uint8_t argumentOffsetForFunctionKind(ast::FunctionKind kind) {
    switch (kind) {
        case yo::ast::FunctionKind::GlobalFunction:
        case yo::ast::FunctionKind::StaticMethod:
        case yo::ast::FunctionKind::OperatorOverload: // TODO what about instance methods that are operator overloads?
            return 0;
        case yo::ast::FunctionKind::InstanceMethod:
            return kInstanceMethodCallArgumentOffset;
    }
}

void IRGenerator::registerFunction(std::shared_ptr<ast::FunctionDecl> functionDecl) {
    LKAssert(functionDecl->getParamNames().size() == functionDecl->getSignature().paramTypes.size());
    
    const auto &sig = functionDecl->getSignature();
    const bool isMain = functionDecl->isOfFunctionKind(ast::FunctionKind::GlobalFunction) && functionDecl->getName() == "main";
    
    const auto argumentOffset = argumentOffsetForFunctionKind(functionDecl->getFunctionKind());
    
    if (isMain) {
        functionDecl->getAttributes().no_mangle = true;
        
        // Check signature
        if (sig.paramTypes.empty() && resolveTypeDesc(sig.returnType) != builtinTypes.yo.i32) {
            diagnostics::emitError(functionDecl->getSourceLocation(), "invalid signature: 'main' must return 'i32'");
        } else if (!sig.paramTypes.empty()) {
            ast::FunctionSignature expectedSig;
            expectedSig.returnType = ast::TypeDesc::makeResolved(builtinTypes.yo.i32);
            expectedSig.paramTypes = {
                expectedSig.returnType, ast::TypeDesc::makeResolved(builtinTypes.yo.i8Ptr->getPointerTo())
            };
            if (!equal(sig, expectedSig)) {
                diagnostics::emitError(functionDecl->getSourceLocation(), util::fmt::format("invalid signature for function 'main'. Expected {}, got {}", expectedSig, sig));
            }
        }
    }
    
    
    // TODO insert more signature checks:
    // - initializers have to return void, as do dealloc functions
    // - subscript operators can only have 1 parameter
    // - ...
    
    
    if (sig.isTemplateDecl() || functionDecl->getAttributes().intrinsic || functionDecl->getAttributes().int_isCtor) {
        auto canonicalName = mangling::mangleCanonicalName(functionDecl);
        
        if (functionDecl->getAttributes().int_isCtor && !functions[canonicalName].empty()) {
            // Prevent multiple ambiguous (and unnecessary) overloads for template instantiations
            return;
        }
        
        functions[canonicalName].push_back(ResolvedCallable(sig, functionDecl, nullptr, argumentOffset));
        return;
    }
    
    
    // Resolve parameter types and return type
    // We temporarily need to put all parameters into the local scope, since they might be used in a decltype statement
    
    LKAssert(localScope.isEmpty());
    
    std::vector<llvm::Type *> paramTypes(sig.paramTypes.size(), nullptr);
    
    for (size_t idx = 0; idx < sig.paramTypes.size(); idx++) {
        auto type = resolveTypeDesc(sig.paramTypes[idx]);
        auto name = functionDecl->getParamNames()[idx]->value;
        paramTypes[idx] = type->getLLVMType();
        localScope.insert(name, ValueBinding(type, nullptr, []() -> llvm::Value* {
            LKFatalError("invalid operation");
        }, [](llvm::Value *) {
            LKFatalError("invalid operation");
        }, ValueBinding::Flags::None));
    }
    
    auto returnType = resolveTypeDesc(sig.returnType);
    localScope.removeAll();
    
    
    const std::string canonicalName = mangling::mangleCanonicalName(functionDecl);
    const std::string resolvedName = functionDecl->getAttributes().extern_ ? canonicalName : mangleFullyResolved(functionDecl);
    
    if (auto otherDecl = getResolvedFunctionWithName(resolvedName); otherDecl && (otherDecl->funcDecl->getAttributes().extern_ || functionDecl->getAttributes().extern_)) {
        if (!(otherDecl->funcDecl->getAttributes().extern_ && functionDecl->getAttributes().extern_)) {
            diagnostics::emitNote(otherDecl->funcDecl->getSourceLocation(), "identical signature already declared here");
            diagnostics::emitError(functionDecl->getSourceLocation(), "only external functions can have multiple identical declarations");
        }
        const auto &otherSig = otherDecl->funcDecl->getSignature();
        if (!equal(sig, otherSig)) {
            diagnostics::emitNote(otherDecl->funcDecl->getSourceLocation(), "other declaration here");
            diagnostics::emitError(functionDecl->getSourceLocation(), "multiple forward declarations w/ non-matching signatures");
        }
        return;
    }
    
    if (auto RC = util::map::get_opt(resolvedFunctions, resolvedName)) {
        if (RC->funcDecl->getAttributes().int_isFwdDecl && equal(RC->signature, sig)) {
            RC->funcDecl->getAttributes().int_isFwdDecl = false; // TODO this shouldn't be in here!!!!!
            return;
        }
        diagnostics::emitNote(RC->funcDecl->getSourceLocation(), "already declared here");
        diagnostics::emitError(functionDecl->getSourceLocation(), "multiple declarations w/ same signature");
    }
    
    auto FT = llvm::FunctionType::get(returnType->getLLVMType(), paramTypes, functionDecl->getSignature().isVariadic);
    auto F = llvm::Function::Create(FT, llvm::Function::LinkageTypes::ExternalLinkage, resolvedName, *module);
    F->setDSOLocal(!functionDecl->getAttributes().extern_);
    
    ResolvedCallable RC(functionDecl, F, argumentOffset); // TODO set the correct argument offset here ?!
    LKAssert(!util::map::has_key(resolvedFunctions, resolvedName));
    resolvedFunctions.emplace(resolvedName, RC);
    functions[canonicalName].push_back(RC);
}



// TODO why does this function exist?
std::optional<ResolvedCallable> IRGenerator::getResolvedFunctionWithName(const std::string &name) {
    return util::map::get_opt(resolvedFunctions, name);
}


// TODO come up w/ a better implementation than this!
StructType* UselessStructTypeForTemplateStructCtor(std::string name, const parser::TokenSourceLocation &SL) {
    return StructType::create(name, {}, SL);
}





void ensureTemplateParametersAreDistinct(const ast::TemplateParamDeclList &paramDeclList) {
    std::vector<std::string> paramNames;
    
    for (auto &param : paramDeclList.getParams()) {
        if (util::vector::contains(paramNames, param.name->value)) {
            diagnostics::emitError(paramDeclList.getSourceLocation(), util::fmt::format("duplicate template parameter name '{}'", param.name->value));
        }
        paramNames.push_back(param.name->value);
    }
}


// the initializer_list should contain template param decls in decreasing order of precedence
// (ie, a struct decl's param list should come before the decl list of one of the struct's member functions)
// Note: this function assumes that the individual lists are duplicate-free
// (otherwise, it will still catch these duplicates, but the error message won't really make sense)
void ensureTemplateParametersDontShadow(std::initializer_list<std::shared_ptr<ast::TemplateParamDeclList>> lists) {
    std::vector<ast::TemplateParamDeclList::Param> params;
    
    for (auto &list : lists) {
        for (auto &param : list->getParams()) {
            if (auto prev = util::vector::first_where(params, [&param](auto &P) { return P.name->value == param.name->value; })) {
                diagnostics::emitNote(prev.value().name->getSourceLocation(), "previously declared here");
                diagnostics::emitError(param.name->getSourceLocation(),
                                       util::fmt::format("declaration of '{}' shadows template parameter from outer scope", param.name->value));
            } else {
                params.push_back(param);
            }
        }
    }
}


StructType* IRGenerator::registerStructDecl(std::shared_ptr<ast::StructDecl> structDecl) {
    auto structName = structDecl->name;
    
    if (structDecl->isTemplateDecl()) {
        LKAssert(!structDecl->attributes.no_init && "template struct cannot have no_init attribute");
        ensureTemplateParametersAreDistinct(*structDecl->templateParamsDecl);
        
        templateStructs[structDecl->name] = structDecl;
        
        ast::FunctionSignature ctorSig;
        std::vector<std::shared_ptr<ast::Ident>> ctorParamNames;
        ctorSig.returnType = ast::TypeDesc::makeNominalTemplated(structName, util::vector::map(structDecl->templateParamsDecl->getParams(), [](auto &param) {
            return ast::TypeDesc::makeNominal(param.name->value);
        }));
        ctorSig.templateParamsDecl = std::make_shared<ast::TemplateParamDeclList>(*structDecl->templateParamsDecl);
        ctorSig.setSourceLocation(structDecl->getSourceLocation());

        auto ctorFnDecl = std::make_shared<ast::FunctionDecl>(ast::FunctionKind::StaticMethod,
                                                              structName, ctorSig,
                                                              attributes::FunctionAttributes());
        ctorFnDecl->setParamNames(ctorParamNames);
        ctorFnDecl->setImplType(UselessStructTypeForTemplateStructCtor(structName, structDecl->getSourceLocation())); // TODO?
        ctorFnDecl->setSourceLocation(structDecl->getSourceLocation());
        ctorFnDecl->getAttributes().int_isCtor = true;
        ctorFnDecl->getAttributes().int_isSynthesized = true;

        registerFunction(ctorFnDecl);
        return nullptr;
    }
    
    structName = mangling::mangleFullyResolved(structDecl);
    
    // TODO add a check somewhere here to make sure there are no duplicate struct members
    StructType::MembersT structMembers;
    structMembers.reserve(structDecl->members.size());

    for (const auto &varDecl : structDecl->members) {
        // Since the varDecls are pointers, the resolveTypeDecl call below also sets the structDecl's member's types,
        // meaning that we can simply use that as the initializer parameters
        auto type = resolveTypeDesc(varDecl->type);
        structMembers.push_back({ varDecl->getName(), type });
    }
    
    auto structTy = StructType::create(structName, structMembers, structDecl->resolvedTemplateArgTypes, structDecl->getSourceLocation());
    nominalTypes.insert(structName, structTy);
    
    LKAssert(!util::map::has_key(structDecls, structName));
    structDecls[structName] = structDecl;
    
    if (!structDecl->attributes.no_init) {
        ast::FunctionSignature signature;
        signature.returnType = ast::TypeDesc::makeResolved(structTy);
        signature.setSourceLocation(structDecl->getSourceLocation());
        
        attributes::FunctionAttributes attributes;
        attributes.no_debug_info = structDecl->attributes.no_debug_info;
        
        auto ctorFnDecl = std::make_shared<ast::FunctionDecl>(ast::FunctionKind::StaticMethod,
                                                              structName, signature, attributes);
        ctorFnDecl->setImplType(structTy);
        ctorFnDecl->setSourceLocation(structDecl->getSourceLocation());
        ctorFnDecl->getAttributes().int_isCtor = true;
        ctorFnDecl->getAttributes().int_isSynthesized = true;
        
        registerFunction(ctorFnDecl);
        
        // TODO instead of this, find custom implementations, if provided, and codegen them instead
        synthesizeDefaultMemberwiseInitializer(structDecl, kSkipCodegen);
        synthesizeDefaultCopyConstructor(structDecl, kSkipCodegen);
        synthesizeDefaultDeallocMethod(structDecl, kSkipCodegen);
    }
    
    return structTy;
}



void IRGenerator::registerImplBlock(std::shared_ptr<ast::ImplBlock> implBlock) {
    using ast::FunctionKind;
    
    auto getFunctionKind = [](const ast::FunctionDecl &FD) -> FunctionKind {
        auto &sig = FD.getSignature();
        if (sig.paramTypes.empty()) {
            LKAssert(!FD.isOperatorOverload()); // TODO would be cool to support this
            return FunctionKind::StaticMethod;
        }
        if (FD.getParamNames()[0]->value != "self") {
            return FunctionKind::StaticMethod;
        }
        auto T = sig.paramTypes[0];
        if (T->isReference() && T->getPointee()->isNominal() && T->getPointee()->getName() == "Self") {
            return FunctionKind::InstanceMethod;
        }
        return FunctionKind::StaticMethod;
    };
    
    
    auto assertIsValidMemberFunction = [](const ast::FunctionDecl &FD, std::shared_ptr<ast::TemplateParamDeclList> implTypeTemplateParams = nullptr) {
        auto &sig = FD.getSignature();
        auto &attr = FD.getAttributes();
        
        if (attr.no_mangle) {
            diagnostics::emitError(FD.getSourceLocation(), "type member function cannot have 'no_mangle' attribute");
        }
        
        if (!attr.mangledName.empty()) {
            diagnostics::emitError(FD.getSourceLocation(), "type member function cannot have explicitly set mangled name");
        }
        
        if (sig.isTemplateDecl()) {
            ensureTemplateParametersAreDistinct(*sig.templateParamsDecl);
            if (implTypeTemplateParams) {
                ensureTemplateParametersDontShadow({implTypeTemplateParams, sig.templateParamsDecl});
            }
        }
    };
    
    
    
    auto typeDesc = implBlock->typeDesc;
    if (auto templateDecl = util::map::get_opt(templateStructs, typeDesc->getName())) {
        implBlock->isNominalTemplateType = true;
        // impl blocks for templated types are registered when their respective type is instantiated
        // TODO static methods of struct templates should be registered anyways!!!!
        
        for (auto &FD : implBlock->methods) {
            assertIsValidMemberFunction(*FD, templateDecl.value()->templateParamsDecl);
            
            // TODO what about registering static method in here, so that we don't have to instantiate the type just to resolve them?
        }
        
        return;
    }
    
    
    auto implTy = resolveTypeDesc(implBlock->typeDesc);
    auto structTy = llvm::cast<StructType>(implTy);
    
    const auto &structAttrs = structDecls.at(structTy->getName())->attributes;
    
    for (auto &fn : implBlock->methods) {
        assertIsValidMemberFunction(*fn);
        auto functionKind = getFunctionKind(*fn);
        if (functionKind == FunctionKind::InstanceMethod) {
            fn->getSignature().paramTypes[0] = ast::TypeDesc::makeResolved(structTy->getReferenceTo());
        }
        fn->getAttributes().no_debug_info = structAttrs.no_debug_info;
        fn->setFunctionKind(functionKind);
        fn->setImplType(structTy);
        registerFunction(fn);
    }
}





# pragma mark - Codegen



#define CASE(node, kind, ...) case NK::kind: return codegen(llvm::dyn_cast<ast::kind>(node), ## __VA_ARGS__);

llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::TopLevelStmt> TLS) {
    switch (TLS->getKind()) {
    CASE(TLS, FunctionDecl)
    CASE(TLS, StructDecl)
    CASE(TLS, ImplBlock)
    case NK::TypealiasDecl:
        return nullptr;
    default: unhandled_node(TLS);
    }
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::LocalStmt> localStmt) {
    switch (localStmt->getKind()) {
    CASE(localStmt, CompoundStmt)
    CASE(localStmt, VarDecl)
    CASE(localStmt, IfStmt)
    CASE(localStmt, Assignment)
    CASE(localStmt, WhileStmt)
    CASE(localStmt, ForLoop)
    CASE(localStmt, ExprStmt)
    CASE(localStmt, ReturnStmt)
    CASE(localStmt, BreakContStmt)
    default: unhandled_node(localStmt);
    }
}

#undef CASE


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::Expr> expr, ValueKind VK, bool insertImplicitLoadInst) {
#define CASE(T, ...) case NK::T: V = codegen(llvm::dyn_cast<ast::T>(expr), ## __VA_ARGS__); break;
    
    llvm::Value *V = nullptr;
    
    switch (expr->getKind()) {
    CASE(NumberLiteral)
    CASE(Ident, VK)
    CASE(CastExpr)
    CASE(StringLiteral, VK)
    CASE(UnaryExpr)
    CASE(MatchExpr)
    CASE(RawLLVMValueExpr)
    CASE(MemberExpr, VK)
    CASE(SubscriptExpr, VK)
    CASE(CallExpr, VK)
    CASE(BinOp)
    CASE(LambdaExpr, VK)
    CASE(ArrayLiteralExpr, VK)
    CASE(TupleExpr, VK)
    default: unhandled_node(expr)
    }
#undef CASE
    
    if (
        insertImplicitLoadInst && V && VK == LValue && getType(expr)->isReferenceTy()
        && (llvm::isa<llvm::AllocaInst>(V) || llvm::isa<llvm::GetElementPtrInst>(V)) // TODO this line isn't really needed, right?
        && V->getType()->isPointerTy() && V->getType()->getPointerElementType() == getType(expr)->getLLVMType()
    ) {
        V = builder.CreateLoad(V);
    }
    return V;
}


llvm::DIFile* DIFileForSourceLocation(llvm::DIBuilder& builder, const parser::TokenSourceLocation& loc) {
    const auto [directory, filename] = util::string::extractPathAndFilename(loc.filepath);
    return builder.createFile(filename, directory);
}


#pragma mark - Top Level Statements

llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::FunctionDecl> functionDecl) {
    const auto &sig = functionDecl->getSignature();
    const auto &attr = functionDecl->getAttributes();
    
    LKAssert(!attr.int_isDelayed);
    
    if (attr.extern_ || attr.intrinsic || attr.int_isCtor || sig.isTemplateDecl()) {
        return nullptr;
    }
    
    LKAssert(localScope.isEmpty());
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
    // TODO
//    if (attr.no_inline) {
//        F->addFnAttr(llvm::Attribute::NoInline);
//    }
    
    LKAssert(F->empty());
    
    
    // for now, assign just the function decl, so that shouldEmitDebugInfo can access the current function's attributes
    // the proper `currentFunction` is assigned before function body codegen
    currentFunction = FunctionState(functionDecl, /* llvmFunction */ nullptr, /* returnBB */ nullptr, /* retvalAlloca */ nullptr, /* stackTopMarker */ 0);
    
    auto entryBB = llvm::BasicBlock::Create(C, "entry", F);
    auto returnBB = llvm::BasicBlock::Create(C, "return");
    builder.SetInsertPoint(entryBB);
    
    if (shouldEmitDebugInfo()) {
        auto unit = DIFileForSourceLocation(debugInfo.builder, functionDecl->getSourceLocation());
        auto SP = debugInfo.builder.createFunction(unit, functionDecl->getName(), resolvedName, unit,
                                           sig.getSourceLocation().line,
                                           toDISubroutineType(sig),
                                           sig.getSourceLocation().line,
                                           llvm::DINode::FlagZero,
                                           llvm::DISubprogram::DISPFlags::SPFlagDefinition);
        emitDebugLocation(nullptr);
        F->setSubprogram(SP);
        debugInfo.builder.finalizeSubprogram(SP);
        debugInfo.lexicalBlocks.push_back(SP);
    }
    
    
    std::vector<llvm::AllocaInst *> paramAllocas;
    
    for (size_t i = 0; i < sig.paramTypes.size(); i++) {
        auto type = resolveTypeDesc(sig.paramTypes[i]);
        auto alloca = builder.CreateAlloca(type->getLLVMType());
        const auto &name = functionDecl->getParamNames()[i]->value;
        alloca->setName(name);
        
        localScope.insert(name, ValueBinding{
            type, alloca, [=]() -> llvm::Value* {
                return builder.CreateLoad(alloca);
            }, [=](llvm::Value *V) {
                // TODO turn this into an assignment-side error
                LKFatalError("Function arguments are read-only (%s in %s)", name.c_str(), resolvedName.c_str());
            },
            ValueBinding::Flags::CanRead
        });
        
        paramAllocas.push_back(alloca);
    }
    
    
    for (size_t i = 0; i < sig.paramTypes.size(); i++) {
        auto alloca = paramAllocas.at(i);
        builder.CreateStore(&F->arg_begin()[i], alloca);
        
        const auto &paramTy = sig.paramTypes.at(i);
        const auto &paramNameDecl = functionDecl->getParamNames().at(i);
        if (shouldEmitDebugInfo()) {
            auto SP = debugInfo.lexicalBlocks.back();
            auto varInfo = debugInfo.builder.createParameterVariable(SP, alloca->getName(), i + 1, SP->getFile(),
                                                                     paramNameDecl->getSourceLocation().line,
                                                                     resolveTypeDesc(paramTy)->getLLVMDIType());
            debugInfo.builder.insertDeclare(alloca, varInfo, debugInfo.builder.createExpression(),
                                            llvm::DILocation::get(C, paramNameDecl->getSourceLocation().line, paramNameDecl->getSourceLocation().column, SP), entryBB);
        }
    }
    
    
    llvm::Value *retvalAlloca = nullptr;
    auto returnType = resolveTypeDesc(sig.returnType);
    
    if (!returnType->isVoidTy()) {
        retvalAlloca = builder.CreateAlloca(F->getFunctionType()->getReturnType());
        retvalAlloca->setName(kRetvalAllocaIdentifier);
        
        localScope.insert(kRetvalAllocaIdentifier, ValueBinding(
            returnType, retvalAlloca, []() -> llvm::Value* {
                LKFatalError("retval is write-only");
            }, [this, retvalAlloca](llvm::Value *V) {
                builder.CreateStore(V, retvalAlloca);
            },
            { ValueBinding::Flags::CanWrite, ValueBinding::Flags::DontDestroy }
        ));
        
        // Create Debug Metadata
        if (shouldEmitDebugInfo()) {
            auto SP = debugInfo.lexicalBlocks.back();
            auto D = debugInfo.builder.createAutoVariable(SP, kRetvalAllocaIdentifier, SP->getFile(),
                                                          sig.getSourceLocation().line, returnType->getLLVMDIType());
            debugInfo.builder.insertDeclare(retvalAlloca, D, debugInfo.builder.createExpression(),
                                            llvm::DebugLoc::get(sig.getSourceLocation().line, 0, SP), entryBB);
        }
    }
    
    currentFunction = FunctionState(functionDecl, F, returnBB, retvalAlloca, localScope.getMarker());
    
    
    // TODO is this a good idea?
    if (functionDecl->getBody()->isEmpty() || !functionDecl->getBody()->statements.back()->isOfKind(NK::ReturnStmt)) {
        if (returnType->isVoidTy()) {
            functionDecl->getBody()->statements.push_back(std::make_shared<ast::ReturnStmt>(nullptr));
        } else {
            diagnostics::emitError(functionDecl->getSourceLocation(), "missing return statement at end of function body");
        }
    }
    
    
    codegen(functionDecl->getBody());
    
    F->getBasicBlockList().push_back(returnBB);
    builder.SetInsertPoint(returnBB);
    
    // These were alreadu\y destructed (either by returning from the local scope, or by reaching the end of the function body)
    localScope.removeAllSinceMarker(currentFunction.stackTopMarker);
    LKAssert(localScope.size() == currentFunction.stackTopMarker);
    
    // Cleanup the rest of the local scope (parameters and kRetvalAllocaIdentifier, although the return value won't be destructed)
    destructLocalScopeUntilMarker(0, true);
    LKAssert(localScope.isEmpty());
    
    if (returnType->isVoidTy()) {
        builder.CreateRetVoid();
    } else {
        builder.CreateRet(builder.CreateLoad(retvalAlloca));
    }
    
    
    if (shouldEmitDebugInfo()) {
        debugInfo.lexicalBlocks.pop_back(); // TODO maybe add a check that the lexical blocks weren't somehow modified?
    }
    currentFunction = FunctionState();
    
    return F;
}




llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::StructDecl> structDecl) {
    if (structDecl->isTemplateDecl()) return nullptr;
    
    for (auto &implBlock : structDecl->implBlocks) {
        for (auto &FD : implBlock->methods) {
            codegen(FD);
        }
    }
    
    return nullptr;
}




llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::ImplBlock> implBlock) {
    return nullptr;
}





#pragma mark - Local Statements




llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::CompoundStmt> compoundStmt) {
    const auto &stmts = compoundStmt->statements;
    
    auto marker = localScope.getMarker();
    bool didReturn = false;
        
    for (auto it = stmts.begin(); !didReturn && it != stmts.end(); it++) {
        const auto stmt = *it;
        codegen(stmt);
        if (stmt->isOfKind(NK::ReturnStmt)) {
            didReturn = true;
        }
    }
    
    if (!didReturn) {
        destructLocalScopeUntilMarker(marker, /*removeFromLocalScope*/ true);
    } else {
        localScope.removeAllSinceMarker(marker);
    }
    
    return nullptr;
}





llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::VarDecl> varDecl) {
    if (localScope.contains(varDecl->getName())) {
        // TODO is there a good reason why this shouldn't be allowed?
        auto msg = util::fmt::format("redeclaration of '{}'", varDecl->getName());
        diagnostics::emitError(varDecl->ident->getSourceLocation(), msg);
    }
    
    
    Type *type = nullptr;
    bool hasInferredType = false;
    
    if (varDecl->type == nullptr) {
        // If no type is specified, there _has_ to be an initial value
        if (!varDecl->initialValue) {
            diagnostics::emitError(varDecl->getSourceLocation(), "must specify initial value");
        }
        type = getType(varDecl->initialValue);
        hasInferredType = true;
    } else {
        type = resolveTypeDesc(varDecl->type);
    }
    if (!type) {
        diagnostics::emitError(varDecl->getSourceLocation(), "unable to infer type of variable");
    }
    
    if (varDecl->declaresUntypedReference && !type->isReferenceTy()) {
        type = type->getReferenceTo();
    } else if (type->isReferenceTy() && hasInferredType && !varDecl->declaresUntypedReference) {
        type = llvm::dyn_cast<ReferenceType>(type)->getReferencedType();
    }
    
    if (type->isReferenceTy()) {
        if (!varDecl->initialValue) {
            diagnostics::emitError(varDecl->getSourceLocation(), "lvalue reference declaration requires initial value");
        }
        
        if (isTemporary(varDecl->initialValue)) {
            auto msg = util::fmt::format("lvalue reference of type '{}' cannot bind to temporary of type '{}'", type, getType(varDecl->initialValue));
            diagnostics::emitError(varDecl->initialValue->getSourceLocation(), msg);
        }
    } else {
        // TODO necessary?
    }
    
    emitDebugLocation(varDecl);
    auto alloca = builder.CreateAlloca(getLLVMType(type));
    alloca->setName(varDecl->getName());
    
    // Create Debug Metadata
    if (shouldEmitDebugInfo()) {
        auto D = debugInfo.builder.createAutoVariable(currentFunction.llvmFunction->getSubprogram(),
                                                      varDecl->getName(),
                                                      debugInfo.lexicalBlocks.back()->getFile(),
                                                      varDecl->getSourceLocation().line,
                                                      getDIType(type));
        debugInfo.builder.insertDeclare(alloca, D,
                                        debugInfo.builder.createExpression(),
                                        llvm::DebugLoc::get(varDecl->getSourceLocation().line, 0, currentFunction.llvmFunction->getSubprogram()),
                                        builder.GetInsertBlock());
    }
    
    localScope.insert(varDecl->getName(), ValueBinding(
        type, alloca, [=]() -> llvm::Value* {
            return builder.CreateLoad(alloca);
        }, [=](llvm::Value *V) {
            LKAssert(V->getType() == alloca->getType()->getPointerElementType());
            builder.CreateStore(V, alloca);
        },
        ValueBinding::Flags::ReadWrite
    ));
    
    if (auto initialValueExpr = varDecl->initialValue) {
        // Q: Why create and handle an assignment to set the initial value, instead of just calling Binding.Write?
        // A: The Assignment codegen also includes the trivial type transformations, whish we'd otherwise have to implement again in here
        if (!type->isReferenceTy()) {
            auto assignment = std::make_shared<ast::Assignment>(varDecl->ident, initialValueExpr);
            assignment->setSourceLocation(varDecl->getSourceLocation());
            assignment->shouldDestructOldValue = false;
            codegen(assignment);
        } else {
            auto V = codegen(initialValueExpr, LValue);
            emitDebugLocation(varDecl);
            builder.CreateStore(V, alloca);
        }
    } else {
        if (!driverOptions.fzeroInitialize) {
            diagnostics::emitError(varDecl->getSourceLocation(), "no initial value specified");
        } else {
            // zero initialize
            if (!(type->isPointerTy() || type->isNumericalTy())) {
                // TODO:
                // 1) should function types be considered pointers? (probably, right?)
                // 2) there are other types that can also be zero-initialized? (basically everything!)
                // -> this is a stupid requirement
                diagnostics::emitError(varDecl->getSourceLocation(), "only pointer or numerical types can be zero-initialized");
            } else {
                auto null = llvm::Constant::getNullValue(type->getLLVMType());
                emitDebugLocation(varDecl);
                builder.CreateStore(null, alloca);
            }
        }
    }
    
    return alloca;
}





llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::ReturnStmt> returnStmt) {
    const auto returnType = resolveTypeDesc(currentFunction.decl->getSignature().returnType);

    if (auto expr = returnStmt->expr) {
        auto retvalTy = getType(expr);
        
        auto typeMismatch = [&]() {
            auto msg = util::fmt::format("expression evaluates to type '{}', which is incompatible with the expected return type '{}'",
                                        retvalTy, returnType);
            diagnostics::emitError(returnStmt->getSourceLocation(), msg);
        };
        
        if (retvalTy == returnType) {
            goto handle;
        }
        
        if (returnType->isReferenceTy()) {
            if (isTemporary(expr)) {
                diagnostics::emitError(returnStmt->getSourceLocation(), "cannot return reference to temporary");
            } else if (!retvalTy->isReferenceTy() && retvalTy->getReferenceTo() == returnType) {
                goto handle;
            } else {
                typeMismatch();
            }
        } else {
            if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&expr, retvalTy)) {
                typeMismatch();
            }
        }
        
    handle:
        if (returnType->isReferenceTy() && retvalTy->isReferenceTy()) {
            auto V = codegen(expr, LValue);
            emitDebugLocation(returnStmt);
            builder.CreateStore(V, currentFunction.retvalAlloca);
        } else {
            auto assignment = std::make_shared<ast::Assignment>(makeIdent(kRetvalAllocaIdentifier), expr);
            assignment->setSourceLocation(returnStmt->getSourceLocation());
            assignment->shouldDestructOldValue = false;
            codegen(assignment);
        }
    } else {
        LKAssert(returnType->isVoidTy());
    }
    
    destructLocalScopeUntilMarker(currentFunction.stackTopMarker, /*removeFromLocalScope*/ false);
    
    emitDebugLocation(returnStmt);
    return builder.CreateBr(currentFunction.returnBB);
}


template <typename Dst, typename Src>
bool value_fits_in_type(Src value) {
    // TODO this seems way too easy?
    return static_cast<Src>(static_cast<Dst>(value)) == value;
}


bool integerLiteralFitsInIntegralType(uint64_t value, Type *type) {
#define HANDLE(size_expr, signed_t, unsigned_t) if (size == (size_expr)) { return isSigned ? value_fits_in_type<signed_t>(value) : value_fits_in_type<unsigned_t>(value); }
    
    LKAssert(type->isNumericalTy());
    auto *numTy = llvm::dyn_cast<NumericalType>(type);
    LKAssert(numTy->isIntegerTy());
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
    auto type = getType(*expr);
    if (initialTypeOfExpr) *initialTypeOfExpr = type;
    
    if (type == expectedType) return true;
    
    // at this point, both are numeric? TODO how do we know this?
    if (auto numberLiteral = llvm::dyn_cast<ast::NumberLiteral>(*expr)) {
        if (expectedType->isReferenceTy()) {
            expectedType = llvm::dyn_cast<ReferenceType>(expectedType)->getReferencedType();
        }
        
        if (!valueIsTriviallyConvertible(numberLiteral, expectedType)) return false;
        
        auto loc = (*expr)->getSourceLocation();
        *expr = std::make_shared<ast::CastExpr>(*expr, ast::TypeDesc::makeResolved(expectedType), ast::CastExpr::CastKind::StaticCast);
        (*expr)->setSourceLocation(loc);
        return true;
    }
    
    return false;
}


// TODO should assignments return something?
llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::Assignment> assignment) {
    llvm::Value *llvmTargetLValue = nullptr;
    llvm::Value *llvmRhsVal = nullptr;
    auto rhsExpr = assignment->value;
    auto lhsTy = getType(assignment->target);
    auto rhsTy = getType(assignment->value);
    
    if (rhsExpr->isOfKind(NK::BinOp) && static_cast<ast::BinOp *>(rhsExpr.get())->isInPlaceBinop()) {
        // <lhs> <op>= <rhs>
        // The issue here is that we have to make sure not to evaluate lhs twice
        auto binop = llvm::dyn_cast<ast::BinOp>(rhsExpr);
        LKAssert(assignment->target == binop->getLhs());
        
        auto lhsLValue = codegen(binop->getLhs(), LValue, /*insertImplicitLoadInst*/ false);
        llvmTargetLValue = lhsLValue;
        auto lhsRValue = builder.CreateLoad(lhsLValue);
        
        auto newLhs = std::make_shared<ast::RawLLVMValueExpr>(lhsRValue, lhsTy);
        newLhs->setSourceLocation(assignment->target->getSourceLocation());
        
        auto newBinop = std::make_shared<ast::BinOp>(binop->getOperator(), newLhs, binop->getRhs());
        newBinop->setSourceLocation(binop->getSourceLocation());
        rhsExpr = newBinop;
        
    } else {
        if (isTemporary(assignment->target)) {
            diagnostics::emitError(assignment->target->getSourceLocation(), "cannot assign to temporary value");
        }
        llvmTargetLValue = codegen(assignment->target, LValue, /*insertImplicitLoadInst*/ false);
        
        if (lhsTy == rhsTy) goto ok;
        
        auto check_one_is_matching_ref = [](Type *t1, Type *t2) -> bool {
            return !t1->isReferenceTy() && t2->isReferenceTy() && static_cast<ReferenceType *>(t2)->getReferencedType() == t1;
        };
        if (check_one_is_matching_ref(lhsTy, rhsTy) || check_one_is_matching_ref(rhsTy, lhsTy)) {
            // either of the following:
            // - `&T` <- `T`
            // - `T` <- `&T`
            goto ok;
        }
        
        Type *T;
        if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&rhsExpr, lhsTy, &T)) {
            auto msg = util::fmt::format("cannot assign to '{}' from incompatible type '{}'", lhsTy, T);
            diagnostics::emitError(assignment->getSourceLocation(), msg);
        }
    }
    
ok:
    
    if (lhsTy->isReferenceTy() && !assignment->overwriteReferences
        && (llvm::isa<llvm::AllocaInst>(llvmTargetLValue)
            || (llvm::isa<llvm::GetElementPtrInst>(llvmTargetLValue) && assignment->target->isOfKind(NK::MemberExpr))
            ))
    {
        emitDebugLocation(assignment);
        llvmTargetLValue = builder.CreateLoad(llvmTargetLValue);
    }
    
    if (assignment->shouldDestructOldValue) {
        auto destructStmt = createDestructStmtIfDefined(lhsTy, llvmTargetLValue, /*includeReferences*/ true);
        if (destructStmt) {
            codegen(destructStmt);
        }
    }
    
    if (lhsTy->isReferenceTy() && assignment->overwriteReferences) {
        llvmRhsVal = codegen(rhsExpr, LValue, /*insertImplicitLoadInst*/ false);
        llvmRhsVal = builder.CreateLoad(llvmRhsVal);
    } else {
        bool didConstructCopy;
        llvmRhsVal = constructCopyIfNecessary(rhsTy, rhsExpr, &didConstructCopy);
        if (!didConstructCopy && rhsTy->isReferenceTy()) {
            emitDebugLocation(assignment);
            llvmRhsVal = builder.CreateLoad(llvmRhsVal);
        }
    }
    
    emitDebugLocation(assignment);
    builder.CreateStore(llvmRhsVal, llvmTargetLValue);
    
    return nullptr;
}








#pragma mark - Expressions


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::RawLLVMValueExpr> rawExpr) {
    return rawExpr->value;
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::ExprStmt> exprStmt) {
    auto expr = exprStmt->expr;
    auto type = getType(expr);
    auto ident = currentFunction.getTmpIdent();
    llvm::AllocaInst *alloca = nullptr;
    
    if (!type->isVoidTy()) {
        alloca = builder.CreateAlloca(type->getLLVMType());
        alloca->setName(ident);
        includeInStackDestruction(type, alloca);
    }
    
    auto value = codegen(expr);
    
    if (!type->isVoidTy()) {
        builder.CreateStore(value, alloca);
    }
    return value;
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::NumberLiteral> numberLiteral) {
    emitDebugLocation(numberLiteral);
    
    using NT = ast::NumberLiteral::NumberType;
    
    switch (numberLiteral->type) {
        case NT::Boolean: {
            return llvm::ConstantInt::get(builtinTypes.llvm.i1, numberLiteral->value);
        }
        case NT::Character: {
            LKAssert(integerLiteralFitsInIntegralType(numberLiteral->value, builtinTypes.yo.i8));
            return llvm::ConstantInt::get(builtinTypes.llvm.i8, numberLiteral->value);
        }
        case NT::Integer: {
            return llvm::ConstantInt::get(builtinTypes.llvm.i64, numberLiteral->value);
        }
        case NT::Double: {
            auto value = util::bitcast<double>(numberLiteral->value);
            return llvm::ConstantFP::get(builtinTypes.llvm.Double, value);
        }
    }
}



llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::StringLiteral> stringLiteral, ValueKind VK) {
    using SLK = ast::StringLiteral::StringLiteralKind;

    switch (stringLiteral->kind) {
        case SLK::ByteString:
            emitDebugLocation(stringLiteral);
            // TODO insert VK == RValue check here?
            LKAssert(VK == RValue);
            return builder.CreateGlobalStringPtr(stringLiteral->value);
        
        case SLK::NormalString: {
            if (!nominalTypes.contains(mangling::mangleAsStruct("String"))) {
                diagnostics::emitError(stringLiteral->getSourceLocation(), "unable to find 'String' type");
            }
            auto target = makeIdent("String"); //std::make_shared<ast::Ident>(mangling::mangleCanonicalName("String", "String", ast::FunctionKind::StaticMethod));
            target->setSourceLocation(stringLiteral->getSourceLocation());
            auto callExpr = std::make_shared<ast::CallExpr>(target);
            callExpr->arguments = {
                std::make_shared<ast::RawLLVMValueExpr>(builder.CreateGlobalStringPtr(stringLiteral->value), builtinTypes.yo.i8Ptr)
            };
            callExpr->setSourceLocation(stringLiteral->getSourceLocation());
            return codegen(callExpr, VK);
        }
    }
}



llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::Ident> ident, ValueKind returnValueKind) {
    emitDebugLocation(ident);
    
    auto binding = localScope.get(ident->value);
    if (!binding) {
        diagnostics::emitError(ident->getSourceLocation(), util::fmt::format("use of undeclared identifier '{}'", ident->value));
    }
    
    switch (returnValueKind) {
        case LValue:
            return binding->value;
        
        case RValue:
            if (!binding->hasFlag(ValueBinding::Flags::CanRead)) {
                auto msg = util::fmt::format("value binding for ident '{}' in local scope does not allow reading", ident->value);
                diagnostics::emitError(ident->getSourceLocation(), msg);
            }
            return binding->read();
    }
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::CastExpr> castExpr) {
    using LLVMCastOp = llvm::Instruction::CastOps;
    constexpr auto invalidCastOp = static_cast<LLVMCastOp>(-1); // TODO is -1 a good invalid value?
    
    LLVMCastOp op = invalidCastOp;
    auto srcTy = getType(castExpr->expr);
    auto dstTy = resolveTypeDesc(castExpr->destType);
    auto &DL = module->getDataLayout();
    
    if (srcTy == dstTy) {
        return codegen(castExpr->expr);
    }
    
    switch (castExpr->kind) {
        case ast::CastExpr::CastKind::Bitcast: {
            LKAssert(DL.getTypeSizeInBits(getLLVMType(srcTy)) == DL.getTypeSizeInBits(getLLVMType(dstTy)));
            if (srcTy->isPointerTy() && dstTy->isNumericalTy()) {
                op = LLVMCastOp::PtrToInt;
            } else if (srcTy->isNumericalTy() && dstTy->isPointerTy()) { // TODO restrict this to `i/u64 -> ptr`?
                op = LLVMCastOp::IntToPtr;
            } else {
                op = LLVMCastOp::BitCast;
            }
            break;
        }
        
        // TODO support static casts between integers and bools
        case ast::CastExpr::CastKind::StaticCast: {
            if (srcTy->isNumericalTy() && dstTy->isNumericalTy()) {
                auto srcTyNT = static_cast<NumericalType *>(srcTy);
                auto dstTyNT = static_cast<NumericalType *>(dstTy);
                
                if (srcTyNT->isIntegerTy() && dstTyNT->isIntegerTy()) {
                    auto srcIntWidth = srcTyNT->getLLVMType()->getIntegerBitWidth();
                    auto dstIntWidth = dstTyNT->getLLVMType()->getIntegerBitWidth();
                    
                    if (srcIntWidth > dstIntWidth) {
                        // casting to a smaller type
                        op = LLVMCastOp::Trunc;
                    } else {
                        // casting to a larger type
                        if (srcTyNT->isSigned()) {
                            op = LLVMCastOp::SExt;
                        } else {
                            op = LLVMCastOp::ZExt;
                        }
                    }
                } else if (srcTyNT->isFloatTy() && dstTyNT->isIntegerTy()) {
                    if (dstTyNT->isSigned()) {
                        op = LLVMCastOp::FPToSI;
                    } else {
                        op = LLVMCastOp::FPToUI;
                    }
                } else if (srcTyNT->isIntegerTy() && dstTyNT->isFloatTy()) {
                    if (srcTyNT->isSigned()) {
                        op = LLVMCastOp::SIToFP;
                    } else {
                        op = LLVMCastOp::UIToFP;
                    }
                } else if (srcTyNT->isBoolTy() && dstTyNT->isIntegerTy()) {
                    op = LLVMCastOp::ZExt;
//                } else if (srcTyNT->isIntegerTy() && dstTyNT->isBoolTy()) {
//                    // TODO this needs an icmp?
                }
                break;
            }
            
            auto msg = util::fmt::format("unable to resolve static_cast. No known conversion from '{}' to '{}'", srcTy, dstTy);
            diagnostics::emitError(castExpr->getSourceLocation(), msg);
        }
    }
    
    if (op == invalidCastOp) {
        auto msg = util::fmt::format("unable to resolve cast from '{}' to '{}'", srcTy, dstTy);
        diagnostics::emitError(castExpr->getSourceLocation(), msg);
    }
    
    emitDebugLocation(castExpr);
    return builder.CreateCast(op, codegen(castExpr->expr), dstTy->getLLVMType());
}







llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::MemberExpr> memberExpr, ValueKind returnValueKind) {
    const auto targetTy = getType(memberExpr->target);
    
    StructType *structTy = nullptr;
    const bool needsLoad = targetTy->isPointerTy() || targetTy->isReferenceTy();
        
    structTy = getUnderlyingStruct(targetTy);
    
    if (!structTy) {
        auto msg = util::fmt::format("invalid member expr base type: '{}'", targetTy);
        diagnostics::emitError(memberExpr->getSourceLocation(), msg);
    }
    
    const auto [memberIndex, memberType] = structTy->getMember(memberExpr->memberName);
    LKAssert(memberType != nullptr && "member does not exist");
    
    llvm::Value *offsets[] = {
        llvm::ConstantInt::get(builtinTypes.llvm.i32, 0),
        llvm::ConstantInt::get(builtinTypes.llvm.i32, memberIndex)
    };
    
    auto targetV = codegen(memberExpr->target, LValue, /*insertImplicitLoadInst*/ false);
    
    if (isTemporary(memberExpr->target)) {
        LKFatalError("TODO");
        // TODO should this come after the load below?
        includeInStackDestruction(targetTy, targetV);
    }
    
    emitDebugLocation(memberExpr);
    if (needsLoad) {
        targetV = builder.CreateLoad(targetV);
    }
    
    auto V = builder.CreateGEP(targetV, offsets);
    
    switch (returnValueKind) {
        case LValue:
            return V;
        case RValue:
            return builder.CreateLoad(V);
    }
}




std::shared_ptr<ast::CallExpr> subscriptExprToCall(std::shared_ptr<ast::SubscriptExpr> subscriptExpr) {
    auto callTarget = std::make_shared<ast::MemberExpr>(subscriptExpr->target, mangling::encodeOperator(ast::Operator::Subscript));
    callTarget->setSourceLocation(subscriptExpr->target->getSourceLocation());
    
    auto callExpr = std::make_shared<ast::CallExpr>(callTarget);
    callExpr->setSourceLocation(subscriptExpr->getSourceLocation());
    callExpr->arguments = { subscriptExpr->offset };
    
    return callExpr;
}


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::SubscriptExpr> subscript, ValueKind returnValueKind) {
    auto TT = getType(subscript->target);
    auto OT = getType(subscript->offset);
    
    if (!TT->isPointerTy()) {
        if (!typeIsSubscriptable(TT)) {
            auto msg = util::fmt::format("cannot subscript value of type '{}'", TT);
            diagnostics::emitError(subscript->target->getSourceLocation(), msg);
        }
        
        if (TT->isTupleTy()) {
            size_t index = llvm::cast<ast::NumberLiteral>(subscript->offset)->value;
            auto memberName = formatTupleMemberAtIndex(index)->value;
            auto memberExpr = std::make_shared<ast::MemberExpr>(subscript->target, memberName);
            memberExpr->setSourceLocation(subscript->getSourceLocation());
            return codegen(memberExpr, returnValueKind);
        } else {
            return codegen(subscriptExprToCall(subscript), returnValueKind);
        }
    }
    LKAssert(TT->isPointerTy());
    
    
    if (TT->isPointerTy() && (!OT->isNumericalTy() || !llvm::dyn_cast<NumericalType>(OT)->isIntegerTy())) {
        auto msg = util::fmt::format("expected integral type, got '{}'", OT);
        diagnostics::emitError(subscript->offset->getSourceLocation(), msg);
    }
    
    
    auto target = codegen(subscript->target);
    auto offset = codegen(subscript->offset);
    
    emitDebugLocation(subscript);
    auto GEP = builder.CreateGEP(target, offset);
    
    switch (returnValueKind) {
        case LValue: return GEP;
        case RValue: return builder.CreateLoad(GEP);
    }
}



bool isValidUnaryOpLogicalNegType(Type *ty) {
    if (ty == Type::getBoolType() || ty->isPointerTy()) return true;
    
    if (auto numTy = llvm::dyn_cast<NumericalType>(ty)) {
        return numTy->isIntegerTy();
    }
    
    return false;
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
            auto ty = getType(expr);
            if (!isValidUnaryOpLogicalNegType(ty)) {
                auto msg = util::fmt::format("type '{}' cannpt be used in logical negation", ty);
                diagnostics::emitError(unaryExpr->getSourceLocation(), msg);
            }
            auto V = codegen(expr);
            emitDebugLocation(unaryExpr);
            return builder.CreateIsNull(V); // TODO this seems like a cop-out answer?
        }
        
        case ast::UnaryExpr::Operation::AddressOf: {
            if (isTemporary(expr)) {
                diagnostics::emitError(unaryExpr->getSourceLocation(), "can't take address of temporary");
            }
            return codegen(expr, LValue);
        }
    }
}



// TODO this is bad code
bool isValidMatchPatternForMatchedExprType(std::shared_ptr<ast::Expr> patternExpr, Type *matchedExprType) {
    // Only patterns that are trivially and can be matched w/out side effects are allowed
    // TODO add the side effect checking
    
    if (patternExpr->getKind() == NK::Ident) {
        LKFatalError("TODO");
        return true;
    }
    
    if (matchedExprType->isNumericalTy()) {
        return llvm::isa<ast::NumberLiteral>(patternExpr);
    } else if (matchedExprType == Type::getBoolType()) {
        auto numberExpr = llvm::dyn_cast<ast::NumberLiteral>(patternExpr);
        return numberExpr && numberExpr->type == ast::NumberLiteral::NumberType::Boolean;
    } else {
        return false;
    }
}


llvm::Value *IRGenerator::codegen_HandleMatchPatternExpr(MatchExprPatternCodegenInfo info) {
    emitDebugLocation(info.patternExpr);
    
    auto TT = info.targetType;
    auto PE = info.patternExpr;
    auto PT = getType(PE);
    
    if (TT->isNumericalTy()) {
        if (auto numberLiteral = llvm::dyn_cast<ast::NumberLiteral>(PE)) {
            if (valueIsTriviallyConvertible(numberLiteral, TT)) {
                auto cmp = std::make_shared<ast::BinOp>(ast::Operator::EQ,
                                                        std::make_shared<ast::RawLLVMValueExpr>(info.targetLLVMValue, TT),
                                                        numberLiteral);
                cmp->setSourceLocation(numberLiteral->getSourceLocation());
                return codegen(cmp);
                
            }
        } else {
            diagnostics::emitError(PE->getSourceLocation(), util::fmt::format( "cannot match value of type '{}' against '{}'", TT, PT));
        }
    }
    
    diagnostics::emitError(PE->getSourceLocation(), "not a valid pattern expression");
}



bool lastBranchIsWildcard(const std::shared_ptr<ast::MatchExpr> &matchExpr) {
    const auto& lastBranch = matchExpr->branches.back();
    if (lastBranch.patterns.size() > 1) return false;
    if (auto ident = llvm::dyn_cast<ast::Ident>(lastBranch.patterns[0])) {
        return ident->value == "_";
    }
    return false;
}



// TODO should this go in the control flow section?
// TODO improve debug location emission for match expressions
llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::MatchExpr> matchExpr) {
    emitDebugLocation(matchExpr);
    
    // TODO require that match patterns cannot contain side effects? (this should go in _IsValidMatchPatternForMatchedExprType!)
    auto F = currentFunction.llvmFunction;
    auto matchedExprType = getType(matchExpr->target);
    auto resultType = getType(matchExpr->branches.front().expression);
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
            if (auto ident = llvm::dyn_cast<ast::Ident>(patternExpr)) {
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
            //diagnostics::emitError(branch.expression->getSourceLocation(),
            //                           "Invalud match branch pattern value: Type '%s' not compatible with expected type '%s'",
            //                           _initialTy->str().c_str(), resultType->str().c_str());
            LKFatalError("Invalid match branch result value: Type %s not compatible with expected type %s",
                         _initialTy->str_desc().c_str(), resultType->str_desc().c_str());
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




llvm::Value* IRGenerator::codegen(std::shared_ptr<ast::ArrayLiteralExpr> arrayLiteral, ValueKind VK) {
    if (arrayLiteral->elements.empty()) {
        LKFatalError(""); // this needs special handling to deduce the expected type!
    }
    
    LKFatalError("TODO implement");
}



StructType* IRGenerator::synthesizeUnderlyingStructTypeForTupleType(TupleType *tupleTy) {
    if (auto ST = tupleTy->getUnderlyingStructType()) {
        return ST;
    }
    
    auto SD = std::make_shared<ast::StructDecl>();
    SD->attributes.no_debug_info = true;
    SD->name = util::fmt::format("__tuple_{}", mangling::mangleFullyResolved(tupleTy));
    
    for (int64_t idx = 0; idx < tupleTy->memberCount(); idx++) {
        auto name = formatTupleMemberAtIndex(idx);
        auto type = ast::TypeDesc::makeResolved(tupleTy->getMembers()[idx]);
        SD->members.push_back(std::make_shared<ast::VarDecl>(name, type));
    }
    
    auto ST = withCleanSlate([this, SD] { return registerStructDecl(SD); });
    tupleTy->setUnderlyingStructType(ST);
    return ST;
}



llvm::Value* IRGenerator::codegen(std::shared_ptr<ast::TupleExpr> tupleExpr, ValueKind VK) {
    LKAssert(VK == RValue);
    
    auto tupleTy = llvm::cast<TupleType>(getType(tupleExpr));
    auto underlyingST = synthesizeUnderlyingStructTypeForTupleType(tupleTy);
    
    auto callExpr = std::make_shared<ast::CallExpr>(nullptr);
    callExpr->arguments = tupleExpr->elements;
    callExpr->setSourceLocation(tupleExpr->getSourceLocation());
    
    return constructStruct(underlyingST, callExpr, /* putInLocalScope */ false, RValue);
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
        || op == Op::LOr
        || op == Op::CR_Exc
        || op == Op::CR_Inc;
}



llvm::Value* IRGenerator::codegen(std::shared_ptr<ast::BinOp> binop) {
    if (!isValidBinopOperator(binop->getOperator())) {
        diagnostics::emitError(binop->getSourceLocation(), "not a valid binary operator");
    }
    
    auto callExpr = std::make_shared<ast::CallExpr>(makeIdent(mangling::mangleCanonicalName(binop->getOperator())),
                                                    std::vector<std::shared_ptr<ast::Expr>> { binop->getLhs(), binop->getRhs() });
    callExpr->setSourceLocation(binop->getSourceLocation());
    return codegen(callExpr);
    
}



bool IRGenerator::typecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *lhs, std::shared_ptr<ast::Expr> *rhs, Type **lhsTy_out, Type **rhsTy_out) {
    LKAssert(lhsTy_out && rhsTy_out);
    
    auto lhsTy = getType(*lhs);
    auto rhsTy = getType(*rhs);
    
    *lhsTy_out = lhsTy;
    *rhsTy_out = rhsTy;
    
    if (lhsTy == rhsTy) {
        return true;
    }
    
    // TODO add some kind of "types are compatible for this kind of binary operation" check
    
    if (!lhsTy->isNumericalTy() || !rhsTy->isNumericalTy()) {
        LKFatalError("oh no");
    }
    
    if (llvm::isa<ast::NumberLiteral>(*lhs)) {
        // lhs is literal, cast to type of ths
        auto loc = (*lhs)->getSourceLocation();
        *lhs = std::make_shared<ast::CastExpr>(*lhs, ast::TypeDesc::makeResolved(rhsTy), ast::CastExpr::CastKind::StaticCast);
        (*lhs)->setSourceLocation(loc);
        *lhsTy_out = rhsTy;
    } else if (llvm::isa<ast::NumberLiteral>(*rhs)) {
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




#pragma mark - lambdas


llvm::Value* IRGenerator::codegen(std::shared_ptr<ast::LambdaExpr> lambdaExpr, ValueKind VK) {
    if (VK != RValue) {
        LKFatalError("TODO?");
    }
    
    auto lambdaST = synthesizeLambdaExpr(lambdaExpr);
    
    
    auto callExpr = std::make_shared<ast::CallExpr>(nullptr);
    callExpr->setSourceLocation(lambdaExpr->getSourceLocation());
    
    for (const auto &captureElem : lambdaExpr->captureList) {
        if (auto E = captureElem.expr) {
            callExpr->arguments.push_back(E);
        }
    }
    
    return constructStruct(lambdaST, callExpr, /*putInLocalScope*/ false, VK);
}


StructType* IRGenerator::synthesizeLambdaExpr(std::shared_ptr<ast::LambdaExpr> lambdaExpr) {
    if (auto ST = lambdaExpr->_structType) {
        return ST;
    }
    
    auto &SL = lambdaExpr->getSourceLocation();
    
    auto SD = std::make_shared<ast::StructDecl>();
    SD->setSourceLocation(SL);
    SD->name = util::fmt::format("__{}_lambda_{}", currentFunction.decl->getName(), currentFunction.getCounter());;
    
    for (const auto &captureElem : lambdaExpr->captureList) {
        auto capturedTy = getType(captureElem.expr);
        if (captureElem.isReference && !capturedTy->isReferenceTy()) {
            capturedTy = capturedTy->getReferenceTo();
        }
        
        auto decl = std::make_shared<ast::VarDecl>(captureElem.ident, ast::TypeDesc::makeResolved(capturedTy));
        decl->setSourceLocation(SL);
        SD->members.push_back(decl);
    }
    
    auto ST = withCleanSlate([&] { return registerStructDecl(SD); });
    lambdaExpr->_structType = ST;
    
    auto &sig = lambdaExpr->signature;
//    resolveTypeDesc(sig.returnType);
//    for (auto &TD : sig.paramTypes) {
//        resolveTypeDesc(TD);
//    }
    
    sig.paramTypes.insert(sig.paramTypes.begin(), ast::TypeDesc::makeReference(ast::TypeDesc::makeNominal(SD->getName())));
    lambdaExpr->paramNames.insert(lambdaExpr->paramNames.begin(), makeIdent("self", SL));
    
    auto imp = std::make_shared<ast::FunctionDecl>(ast::FunctionKind::InstanceMethod,
                                                   mangling::encodeOperator(ast::Operator::FnCall),
                                                   sig, attributes::FunctionAttributes());
    imp->setBody(lambdaExpr->body);
    imp->setParamNames(lambdaExpr->paramNames);
    imp->setImplType(ST);
    imp->setSourceLocation(SL);
    
    withCleanSlate([&] {
        registerFunction(imp);
        codegen(imp);
    });
    
    return ST;
}





#pragma mark - function calls



// Resolve a struct's template parameters, using a constructor call
// This only looks at the explicitly passed template arguments, not
// This function returns fully resolved TypeDesc objects
TemplateTypeMapping
IRGenerator::resolveStructTemplateParametersFromExplicitTemplateArgumentList(std::shared_ptr<ast::StructDecl> SD, std::shared_ptr<ast::TemplateParamArgList> templateArgsList) {
    
    TemplateTypeMapping mapping;
    
    const auto &explicitArgs = templateArgsList->elements;
    
    auto &params = SD->templateParamsDecl->getParams();
    auto numParams = params.size();
    auto numArgs = explicitArgs.size();
    
    if (numParams > numArgs) {
        diagnostics::emitError(templateArgsList->getSourceLocation(), "too many template arguments");
    }
    
    // TODO:
    // - take potential additional constructor params into account!
    // - option to deduce type template params instead of requiring they be explicitly specified?
    for (size_t i = 0; i < numParams; i++) {
        auto &param = params[i];
        if (i < numArgs) {
            mapping[param.name->value] = ast::TypeDesc::makeResolved(resolveTypeDesc(explicitArgs[i]));
        } else if (auto defaultType = param.defaultType) {
            // TODO what if a default parameter depends on one of the previous params? (like what std::vector does?)
            // TODO issue? this resolves the type desc in the wrong context (caller vs callee!)
            mapping[param.name->value] = ast::TypeDesc::makeResolved(resolveTypeDesc(defaultType));
        } else {
            auto msg = util::fmt::format("unable to resolve template parameter '{}'", param.name->value);
            diagnostics::emitError(templateArgsList->getSourceLocation(), msg);
        }
    }
    
    return mapping;
}



uint8_t IRGenerator::argumentOffsetForCallingConvention(CallingConvention cc) {
    switch (cc) {
        case CallingConvention::C: return 0;
    }
    
    LKFatalError("unknown calling convention (raw value: %i)", static_cast<int>(cc));
}




// TODO this should somehow return an error message explaining why the template deduction failed (eg "too many template arguments", etc)
std::optional<TemplateTypeMapping>
IRGenerator::attemptToResolveTemplateArgumentTypesForCall(const ast::FunctionSignature &signature, std::shared_ptr<ast::CallExpr> call, unsigned int argumentOffset) {
    using TDK = ast::TypeDesc::Kind;
    
    LKAssert(signature.isTemplateDecl());
    
    if (signature.paramTypes.size() != call->arguments.size() + argumentOffset) {
        return std::nullopt;
    }
    
    if (call->numberOfExplicitTemplateArgs() > signature.numberOfTemplateParameters()) {
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
    
    
    const auto templateParamNames = util::vector::map(signature.templateParamsDecl->getParams(), [](auto &param) { return param.name->value; });
    
    
    enum class DeductionReason {
        Default,    // default value specified in decl
        Expr,       // deduced from argument expr
        Literal,    // deduced from argument expr that is a literal
        Explicit    // explicitly specified in call expr
    };
    using DR = DeductionReason;
    
    struct DeductionInfo {
        Type *type;
        DeductionReason reason;
    };
    
    
    std::map<std::string, DeductionInfo> mapping;
    
    
    // Handle explicitly specified types
    for (uint64_t idx = 0; idx < std::min<uint64_t>(signature.templateParamsDecl->size(), call->numberOfExplicitTemplateArgs()); idx++) {
        auto &param = signature.templateParamsDecl->getParams()[idx];
        mapping[param.name->value] = { resolveTypeDesc(call->explicitTemplateArgs->elements[idx]), DeductionReason::Explicit };
    }
    
    
    // Attempt to resolve template parameters from call arguments
    
    
    // retval: true -> success, false -> failure
    auto imp = [&](std::string name, Type *ty, uint64_t argIdx) -> bool {
        auto reason = call->arguments[argIdx]->isOfKind(NK::NumberLiteral) ? DR::Literal : DR::Expr;
        
        if (!util::map::has_key(mapping, name)) {
            mapping[name] = { ty, reason };
            return true;
        }
        
        auto &prevDeduction = mapping.at(name);
        
        if (prevDeduction.type == ty) {
            return true;
        
        } else if (prevDeduction.reason != reason && reason == DeductionReason::Literal) {
            // Prev wasn't a literal, but current is
            auto argExpr = llvm::dyn_cast<ast::NumberLiteral>(call->arguments[argIdx]);
            return valueIsTriviallyConvertible(argExpr, prevDeduction.type);
        
        } else if (prevDeduction.reason == DeductionReason::Literal && reason != prevDeduction.reason) {
            // Prev was a literal, but current isn't
            
            // The issue here is that we don't have the previous expression object anymore,
            // and therefore can't check whether its value would fit in `argTy`
            // The current workaround is to simply not perform bounds/conversion checks for the previous match at all.
            // This will actually still work, since there are additional type checks (with the actual implicit conversions)
            // performed when running call codegen.
            // The downside is that only calls to functions where a literal argument follows a non-literal argument will
            // get the does-the-value-actually-fit handling at this stage of codegen
            // TODO Fix!
            mapping[name] = { ty, reason };
            return true;
        
        } else {
            return false;
        }
    };
    
    
    std::function<bool(std::shared_ptr<ast::TypeDesc>, Type *, uint64_t)> handle;
    
    handle = [&](std::shared_ptr<ast::TypeDesc> typeDesc, Type *ty, uint64_t argIdx) -> bool {
        switch (typeDesc->getKind()) {
            case TDK::Nominal: {
                if (!util::vector::contains(templateParamNames, typeDesc->getName())) {
                    // A nominal type that is not shadowed by a template parameter
                    return true;
                } else {
                    // A nominal type which is shadowed by a template parameter
                    return imp(typeDesc->getName(), ty, argIdx);
                }
            }
            
            case TDK::Pointer: {
                if (auto tyPtrT = llvm::dyn_cast<PointerType>(ty)) {
                    return handle(typeDesc->getPointee(), tyPtrT->getPointee(), argIdx);
                } else {
                    return false;
                }
            }
            
            case TDK::Reference: {
                if (auto tyRefT = llvm::dyn_cast<ReferenceType>(ty)) {
                    return handle(typeDesc->getPointee(), tyRefT->getReferencedType(), argIdx);
                } else {
                    // Calling a function `<T>(&T)` with an argument of type `T` ?
                    return handle(typeDesc->getPointee(), ty, argIdx);
                }
            }
            
            case TDK::NominalTemplated: {
                auto tyStructT = llvm::dyn_cast<StructType>(ty);
                if (!tyStructT || tyStructT->getTemplateArguments().size() != typeDesc->getTemplateArgs().size()) {
                    // TODO does this need to take defaulted template args into account?
                    return false;
                }
                
                for (uint64_t idx = 0; idx < typeDesc->getTemplateArgs().size(); idx++) {
                    if (!handle(typeDesc->getTemplateArgs().at(idx), tyStructT->getTemplateArguments().at(idx), argIdx)) {
                        return false;
                    }
                }
                return true;
            }
            
            case TDK::Function:
            case TDK::Decltype:
            case TDK::Tuple:
            case TDK::Resolved:
                LKFatalError("TODO");
        }
    };
    
    
    for (size_t idx = 0; idx < call->arguments.size(); idx++) {
        auto sigTypeDesc = signature.paramTypes[idx + argumentOffset];
        auto argTy = getType(call->arguments[idx]);
        
        if (llvm::isa<ReferenceType>(argTy)) {
            argTy = llvm::dyn_cast<ReferenceType>(argTy)->getReferencedType();
        }
        
        if (!handle(sigTypeDesc, argTy, idx)) {
            return std::nullopt;
        }
    }
    
    
    // Make sure all parameters were resolved, apply default values where necessary
    
    for (auto &param : signature.templateParamsDecl->getParams()) {
        if (util::map::has_key(mapping, param.name->value)) continue;
        
        if (auto defaultTy = param.defaultType) {
            // TODO this resolves the default type in a different context than the one it was declared in!
            // Fix by using the specializer to rewrite functions instead of temporarily introducing type mappings?
            // Example:
            // ```
            // struct A {}
            // fn foo<T = A>() {}
            // fn bar<A>() { foo(); }
            // ```
            // In this case, even though foo's default type specified the global struct `A`, it'd get resolved to the locally shadowing template parameter
            mapping[param.name->value] = { resolveTypeDesc(defaultTy), DR::Default };
        } else {
            // no entry for type, and no default value
            return std::nullopt;
        }
    }
    
    
    TemplateTypeMapping retval;
    for (auto &[name, deduction] : mapping) {
        retval[name] = ast::TypeDesc::makeResolved(deduction.type);
    }
    return retval;
}





ast::FunctionSignature makeFunctionSignatureFromFunctionTypeInfo(const FunctionType *fnType) {
    ast::FunctionSignature sig;
    sig.returnType = ast::TypeDesc::makeResolved(fnType->getReturnType());
    sig.paramTypes = util::vector::map(fnType->getParameterTypes(), [](Type *ty) {
        return ast::TypeDesc::makeResolved(ty);
    });
    return sig;
}



// TODO use this in resolveCall below. would allow uniform type checking for all kinds of callables (most notable locals, which aren't relly typeckecked at all in resolveCall)
//// NOTE: This function returning true *does not* mean that the callable is the perfect (or even right, in some instances) target
//// for the function call. All this function does is run some checks to see if the provided arguments are compatible with the callable's
//// signature, and return true if that is the case
//bool callableIsSuitableForFunctionCall(const ResolvedCallable &callable, std::shared_ptr<ast::CallExpr> call) {
//    LKFatalError("implement");
//}




ResolvedCallable IRGenerator::specializeTemplateFunctionDeclForCallExpr(std::shared_ptr<ast::FunctionDecl> funcDecl, TemplateTypeMapping templateArgMapping, uint8_t argumentOffset, SkipCodegenOption codegenOption) {
    auto specializedDecl = TemplateSpecializer::specializeWithMapping(funcDecl, templateArgMapping);
    
    std::vector<Type *> templateArgTypes;
    for (const auto &param : funcDecl->getSignature().templateParamsDecl->getParams()) {
        // TODO does this take default arg values into account?
        templateArgTypes.push_back(resolveTypeDesc(templateArgMapping.at(param.name->value)));
    }
    // TODO use this assert to check whether all params (incl defaults) are taken into account here!!!!
    LKAssert(templateArgTypes.size() == funcDecl->getSignature().templateParamsDecl->size());
    specializedDecl->setResolvedTemplateArgTypes(templateArgTypes);
    
    
    // Avoid generating the same specialization multiple times
    // In theory, we should never end up here since the call target resolution code should prefer the already-specialized version
    // over re-instantiating the template. However, the code is not very good and cannot (yet?) do that
    
    // We need the function's types fully resolved for the `mangleFullyResolved` call below
    // TODO is withCleanSlate overkill here? we really just need the local scope to be empty so that we can insert the parameters
    // Not entirely true though, we also call resolveTypeDesc which otherwise might include local types defined only for the the scope of the current function
    withCleanSlate([&]() -> void {
        for (size_t i = 0; i < specializedDecl->getSignature().paramTypes.size(); i++) {
            auto type = resolveTypeDesc(specializedDecl->getSignature().paramTypes[i]);
            auto name = specializedDecl->getParamNames()[i]->value;
            localScope.insert(name, ValueBinding(type, nullptr, []() -> llvm::Value* {
                LKFatalError("Value cannot be read");
            }, [](llvm::Value *) {
                LKFatalError("Value cannot be written to");
            }, ValueBinding::Flags::None));
        }
        resolveTypeDesc(specializedDecl->getSignature().returnType);
    });
    
    
    auto mangled = mangleFullyResolved(specializedDecl);
    if (auto decl = getResolvedFunctionWithName(mangled)) {
        if (this->equal(specializedDecl->getSignature(), decl->funcDecl->getSignature())) {
            return decl.value();
        }
    }
    
    llvm::Function *llvmFunction = nullptr;
    if (codegenOption == kRunCodegen && !specializedDecl->getAttributes().intrinsic) {
        llvmFunction = withCleanSlate([&]() {
            specializedDecl->getAttributes().int_isDelayed = false;
            registerFunction(specializedDecl);
            return llvm::dyn_cast_or_null<llvm::Function>(codegen(specializedDecl));
        });
    }
    return ResolvedCallable(specializedDecl, llvmFunction, argumentOffset);
}





bool IRGenerator::isImplicitConversionAvailable(Type *src, Type *dst) {
    LKFatalError("implement");
}





/// This function assumes that both targets match the call (ie, are valid targets)
CandidateViabilityComparisonResult FunctionCallTargetCandidate::compare(IRGenerator &irgen, const FunctionCallTargetCandidate &other, std::shared_ptr<ast::CallExpr> callExpr, const std::vector<Type *> &argTys) const {
    const auto &lhs = *this;
    const auto &rhs = other;
    
    auto getResolvedParamTys = [&](const FunctionCallTargetCandidate &candidate) -> std::vector<Type *> {
        std::vector<decltype(irgen.nominalTypes)::ID> tempTypeIds;
        for (const auto &[name, typeDesc] : candidate.templateArgumentMapping) {
            tempTypeIds.push_back(irgen.nominalTypes.insert(name, typeDesc->getResolvedType()));
        }
        auto tys = util::vector::map(candidate.getSignature().paramTypes, [&](auto typeDesc) {
            return irgen.resolveTypeDesc(typeDesc, false);
        });
        irgen.nominalTypes.removeAll(tempTypeIds);
        return tys;
    };
    
    
    auto lhsParamTys = getResolvedParamTys(lhs);
    auto rhsParamTys = getResolvedParamTys(rhs);
    
    
    for (uint32_t idx = lhs.target.argumentOffset; idx < lhs.getSignature().paramTypes.size(); idx++) {
        auto argTy = argTys[idx];
        auto lhsTy = lhsParamTys[idx];
        auto rhsTy = rhsParamTys[idx];
        
//        util::fmt::print("[{}] act: {}, exp(L): {}, exp(R): {}", idx, argTy, lhsTy, rhsTy);
        
        if (argTy == lhsTy && argTy == rhsTy) {
            continue;
        } else if (callExpr->arguments[idx]->isOfKind(NK::NumberLiteral)) {
            LKFatalError("TODO");
        } else {
            continue;
        }
    }
    
    // prefer non-template overloads
    if (!lhs.getSignature().isTemplateDecl() && rhs.getSignature().isTemplateDecl()) {
        return CandidateViabilityComparisonResult::MoreViable;
    }
    
    // if both are templates, prefer the more specialized one
    if (lhs.getSignature().isTemplateDecl() && rhs.getSignature().isTemplateDecl()) {
        
        // What's going on here?
        // We somehow need to figure out which of these two versions is "more specialized".
        //
        // What does this mean? Consider the following example:
        // ```
        // struct A<T> {}
        //
        // fn f<T>(T, T) {}       // 1
        // fn f<T>(A<T>, A<T>) {} // 2
        //
        // let a = A<i64>();
        // f(a, a);
        // ```
        //
        // clearly, the best target for the call to `f` is the second overload
        
        
        auto imp = [&irgen](const FunctionCallTargetCandidate &lhs, const FunctionCallTargetCandidate &rhs) -> bool {
            uint64_t counter = 0;
            TemplateTypeMapping mapping;
            std::vector<std::pair<Type *, decltype(irgen.nominalTypes)::ID>> tmpTypes;
            
            for (auto &param : lhs.getSignature().templateParamsDecl->getParams()) {
                auto tmpName = util::fmt::format("U{}", ++counter);
                mapping[param.name->value] = ast::TypeDesc::makeNominal(tmpName);
            }
            
            auto specSig = TemplateSpecializer(mapping).specialize(lhs.getSignature());
            
            for (const auto &[_ignored_name, typeDesc] : mapping) {
                auto name = typeDesc->getName();
                auto type = Type::createTemporary(mangling::mangleAsStruct(name));
                typeDesc->setResolvedType(type);
                tmpTypes.emplace_back(type, irgen.nominalTypes.insert(name, type));
            }
            
            auto argTys = util::vector::map(specSig.paramTypes, [&irgen](auto TD) { return irgen.resolveTypeDesc(TD, false); });
            
            auto tmpCallExpr = std::make_shared<ast::CallExpr>(nullptr);
            tmpCallExpr->arguments = util::vector::map(argTys, [](Type *type) -> std::shared_ptr<ast::Expr> {
                return std::make_shared<ast::RawLLVMValueExpr>(nullptr, type);
            });
            
            auto deduction = irgen.attemptToResolveTemplateArgumentTypesForCall(rhs.getSignature(), tmpCallExpr, rhs.target.argumentOffset);
            
            std::function<bool(Type *, std::set<Type *> &)> collectTempTypes;
            collectTempTypes = [&](Type *type, std::set<Type *> &found) -> bool {
                bool retval = false;
                
                if (type->hasFlag(Type::Flags::IsTemporary)) {
                    found.insert(type);
                    retval = true;
                }
                
                switch (type->getTypeId()) {
                    case Type::TypeID::Void:
                        break;
                    
                    case Type::TypeID::Numerical:
                        break;
                    
                    case Type::TypeID::Pointer: {
                        auto ptrTy = llvm::cast<PointerType>(type);
                        if (collectTempTypes(ptrTy->getPointee(), found)) {
                            retval = true;
                        }
                        break;
                    }
                    
                    case Type::TypeID::Reference: {
                        auto refTy = llvm::cast<ReferenceType>(type);
                        if (collectTempTypes(refTy->getReferencedType(), found)) {
                            retval = true;
                        }
                        break;
                    }
                    
                    case Type::TypeID::Function: {
                        auto fnTy = llvm::cast<FunctionType>(type);
                        for (auto ty : fnTy->getParameterTypes()) {
                            if (collectTempTypes(ty, found)) {
                                retval = true;
                            }
                        }
                        if (collectTempTypes(fnTy->getReturnType(), found)) {
                            retval = true;
                        }
                        break;
                    }
                    
                    case Type::TypeID::Struct: {
                        auto structTy = llvm::cast<StructType>(type);
                        for (auto ty : structTy->getTemplateArguments()) {
                            if (collectTempTypes(ty, found)) {
                                retval = true;
                            }
                        }
                        break;
                    }
                    
                    case Type::TypeID::Tuple: {
                        auto tupleTy = llvm::cast<TupleType>(type);
                        for (auto ty : tupleTy->getMembers()) {
                            if (collectTempTypes(ty, found)) {
                                retval = true;
                            }
                        }
                        break;
                    }
                }
                
                if (retval) {
                    found.insert(type);
                }
                return retval;
            };
            
            
            for (auto [type, id] : tmpTypes) {
                irgen.nominalTypes.remove(id);
            }
            
            std::set<Type *> foundTempTypes;
            for (Type *type : argTys) {
                collectTempTypes(type, foundTempTypes);
            }
//            for (Type *type : foundTempTypes) {
//                util::fmt::print("[DEL] {}", static_cast<void*>(type));
//                delete type; // TODO why does this not work as expected?
//            }
            return !deduction.has_value();
        };
        
        bool l2r = imp(lhs, rhs);
        bool r2l = imp(rhs, lhs);
        
        if (l2r == r2l) {
            return CandidateViabilityComparisonResult::Ambiguous;
        } else if (l2r) {
            return CandidateViabilityComparisonResult::LessViable;
        } else if (r2l) {
            return CandidateViabilityComparisonResult::MoreViable;
        }
        
        
        LKFatalError("TODO");
    }
    
    
    return CandidateViabilityComparisonResult::LessViable;
}






// This function will only return if the call can be resolved
ResolvedCallable IRGenerator::resolveCall(std::shared_ptr<ast::CallExpr> callExpr, SkipCodegenOption codegenOption) {
    // TODO this function is rather long, refactor!!!
    
    const bool skipCodegen = codegenOption == kSkipCodegen;
    std::string targetName;
    auto targetKind = ast::FunctionKind::GlobalFunction;
    
    
    if (auto ident = llvm::dyn_cast<ast::Ident>(callExpr->target)) {
        targetName = ident->value;
        
        // this is in here because at this point targetName is still just an identifier
        if (auto valueBinding = localScope.get(targetName)) {
            auto ty = valueBinding->type;
            if (ty->isFunctionTy()) {
                auto fnTy = llvm::dyn_cast<FunctionType>(ty);
                return ResolvedCallable(makeFunctionSignatureFromFunctionTypeInfo(fnTy), nullptr,
                                        skipCodegen ? nullptr : codegen(ident),
                                        argumentOffsetForCallingConvention(fnTy->getCallingConvention()));
            } else if (implementsInstanceMethod(ty, mangling::encodeOperator(ast::Operator::FnCall))) {
                // calling a local variable of a type which happens to overload the `()` operator
                targetName = mangling::mangleCanonicalName(getUnderlyingStruct(ty)->getName(),
                                                           mangling::encodeOperator(ast::Operator::FnCall),
                                                           ast::FunctionKind::InstanceMethod);
                targetKind = ast::FunctionKind::InstanceMethod;
            } else {
                diagnostics::emitError(callExpr->getSourceLocation(), "unable to resolve call target");
            }
        }
        
    } else if (auto staticDeclRefExpr = llvm::dyn_cast<ast::StaticDeclRefExpr>(callExpr->target)) {
        // <typename>::<methodname>()
        
        auto TD = staticDeclRefExpr->typeDesc;
        
        if (TD->isNominal()) {
            targetName = mangling::mangleCanonicalName(mangling::mangleAsStruct(TD->getName()),
                                                       staticDeclRefExpr->memberName,
                                                       ast::FunctionKind::StaticMethod);
        } else if (TD->isNominalTemplated()) {
            if (auto structDecl = util::map::get_opt(templateStructs, TD->getName())) {
                // TODO can we do better if `codegenOption = kSkipCodegen`?
                // at the end of the day, it doesn't really matter, but still ...
                auto structTy = withCleanSlate([&]() { return instantiateTemplateStruct(*structDecl, TD); });
                targetName = mangling::mangleCanonicalName(structTy->getName(),
                                                           staticDeclRefExpr->memberName,
                                                           ast::FunctionKind::StaticMethod);
            
            } else {
                diagnostics::emitError(staticDeclRefExpr->getSourceLocation(),
                                       util::fmt::format("unable to resolve type '{}'", TD->getName()));
            }
            
        } else {
            // TODO what about decltypes that resolve to nominal types?
            // TODO once `impl`s are typedesc-based, all types should support static methods
            LKFatalError("unsupported type desc");
        }
        
    } else if (auto memberExpr = llvm::dyn_cast<ast::MemberExpr>(callExpr->target)) {
        // TODO does this take overloaded instance methods into account?

        // <memberExpr>()
        // two options:
        // - calling a method
        // - calling a property that happens to be a function

        auto targetTy = getType(memberExpr->target);
        
        // TODO can this use `getUnderlyingType`?
        StructType *structTy = nullptr;
        switch (targetTy->getTypeId()) {
            case Type::TypeID::Struct:
                structTy = static_cast<StructType *>(targetTy);
                break;
            case Type::TypeID::Pointer:
                structTy = llvm::dyn_cast<StructType>(llvm::dyn_cast<PointerType>(targetTy)->getPointee());
                break;
            case Type::TypeID::Reference:
                structTy = llvm::dyn_cast<StructType>(llvm::dyn_cast<ReferenceType>(targetTy)->getReferencedType());
                break;
            default:
                diagnostics::emitError(memberExpr->getSourceLocation(), "invalid call target member expr target type");
        }

        LKAssert(structTy);
        
        auto structName = structTy->getName();

        if (auto [memberIndex, memberTy] = structTy->getMember(memberExpr->memberName); memberTy != nullptr) {
            if (implementsInstanceMethod(structTy, memberExpr->memberName)) {
                // TODO prevent this when registering instance methods!
                diagnostics::emitError(callExpr->getSourceLocation(), "call target ambiguous");
            }
            if (memberTy->isFunctionTy()) {
                // calling a struct property, which is a function pointer
                // struct properties cannot be overloaded, simply return what we found
                auto fnTy = llvm::dyn_cast<FunctionType>(memberTy);
                return ResolvedCallable(makeFunctionSignatureFromFunctionTypeInfo(fnTy), nullptr,
                                        skipCodegen ? nullptr : codegen(memberExpr),
                                        argumentOffsetForCallingConvention(fnTy->getCallingConvention()));
            } else {
                // calling a struct property of a type which overloads the `()` operator
                targetName = mangling::mangleCanonicalName(getUnderlyingStruct(memberTy)->getName(),
                                                           mangling::encodeOperator(ast::Operator::FnCall),
                                                           ast::FunctionKind::InstanceMethod);
                targetKind = ast::FunctionKind::InstanceMethod;
            }
        } else {
            // calling an instance method
            targetName = mangling::mangleCanonicalName(structName, memberExpr->memberName, ast::FunctionKind::InstanceMethod);
            targetKind = ast::FunctionKind::InstanceMethod;
        }
    } else {
        diagnostics::emitError(callExpr->getSourceLocation(), "unable to resolve call target");
    }
    
    
    auto argumentOffset = argumentOffsetForFunctionKind(targetKind);
    
    // TODO does this mean we might miss another potential target
    // Not if the compiler disallows free functions w/ the same name as a defined type
    //if (nominalTypes.get(mangling::mangleAsStruct(targetName)) || util::map::has_key(templateStructs, targetName)) {
    if (bool isNominalTy = nominalTypes.get(mangling::mangleAsStruct(targetName)).has_value(), isTemplate = util::map::has_key(templateStructs, targetName);isNominalTy || isTemplate) {
        if (isNominalTy) targetName = mangling::mangleAsStruct(targetName); // TODO this is awful!!!
        auto mangledTargetName = mangling::mangleCanonicalName(targetName, targetName, ast::FunctionKind::StaticMethod);
        
        const auto &targets = functions[mangledTargetName];
        if (targets.size() != 1) {
            diagnostics::emitError(callExpr->getSourceLocation(), "umable to resolve struct constructor");
        }
        auto target = targets.front();
        LKAssert(target.funcDecl->getAttributes().int_isCtor);
        
        if (target.funcDecl->getSignature().isTemplateDecl()) {
            auto templateDecl = templateStructs.at(targetName);
            // TODO what about initializer-based type deduction here?
            auto mapping = resolveStructTemplateParametersFromExplicitTemplateArgumentList(templateDecl, callExpr->explicitTemplateArgs);
            return specializeTemplateFunctionDeclForCallExpr(target.funcDecl, mapping, argumentOffset, codegenOption);
        }
        return target;
    }
    
    
    // find a matching target
    
    const auto &possibleTargets = functions[targetName];
    
    if (possibleTargets.empty()) {
        diagnostics::emitError(callExpr->getSourceLocation(), util::fmt::format("unable to resolve call to '{}'", targetName));
    }
    
    
    struct TargetRejectionInfo {
        std::string reason;
        const ast::FunctionDecl &decl; // using a reference should be fine since the rejection info objects never leave the current scope
        
        TargetRejectionInfo(std::string reason, const ast::FunctionDecl &decl) : reason(reason), decl(decl) {}
    };
    
    util::Counter<> candidateIndexCounter;
    std::vector<TargetRejectionInfo> rejections;
    std::vector<FunctionCallTargetCandidate> candidates;
    
    // Q: Why are the argument types fetched up here?
    // A: If we were to call `getType` in the check loop below, this might get messed up
    //    by the resolved template mapping temporarily inserted into the nominal types list
    std::vector<Type *> argTypes;
    argTypes.reserve(callExpr->arguments.size() + argumentOffsetForFunctionKind(targetKind));
    if (targetKind == ast::FunctionKind::InstanceMethod) {
        if (auto memberExpr = llvm::dyn_cast<ast::MemberExpr>(callExpr->target)) {
            argTypes.push_back(getType(memberExpr->target));
        } else {
            argTypes.push_back(getType(callExpr->target));
        }
    } else {
        LKAssert(argumentOffset == 0); // just to make sure we don't miss anything
    }
    for (auto argExpr : callExpr->arguments) {
        argTypes.push_back(getType(argExpr));
    }
    
    
    for (const auto &target : possibleTargets) {
        const auto &decl = target.funcDecl;
        auto &sig = decl->getSignature();
        bool isVariadicWithCLinkage = sig.isVariadic && target.funcDecl->getAttributes().extern_;
        
        if (decl->isTemplateInstantiation()) {
            continue;
        }
        
        if (!sig.isVariadic && callExpr->arguments.size() != sig.paramTypes.size() - argumentOffset) {
            rejections.emplace_back("argument count mismatch", *decl);
            continue;
        }
        if (sig.isVariadic && (callExpr->arguments.size() < sig.paramTypes.size() - argumentOffset - !isVariadicWithCLinkage)) {
            rejections.emplace_back("variadic arguments count mismatch", *decl);
            continue;
        }
        if (decl->getName() != kInitializerMethodName && !sig.isTemplateDecl() && callExpr->numberOfExplicitTemplateArgs() > 0) {
            // Reject a non-template target if the call expression contains explicit template arguments.
            // The sole exception here are calls to initializers, since in this case the call expression
            // is being reused by the compiler, with the original target (the ctor) replaced w/ the initializer
            if (!decl->isTemplateInstantiation()) {
                rejections.emplace_back("cannot pass explicit template arguments to non-template target", *decl);
            }
            continue;
        }
        
        // extern variadic functions are treated as having C linkage and therefore allowed any variadic arguments
        // another important distinction is that for functions w/ C linkage, the variadic parameter cannot be omitted.
        // for example, printf(*i8...) cannot be called w/ zero arguments, since that would also leave out the format string

        FunctionCallTargetCandidate candidate(candidateIndexCounter.increment(), target);
        size_t lastTypecheckedArgument = isVariadicWithCLinkage ? sig.paramTypes.size() : callExpr->arguments.size();
        
        std::vector<decltype(nominalTypes)::ID> tempTypeIds;
        
        if (sig.isTemplateDecl()) {
            if (auto mapping = attemptToResolveTemplateArgumentTypesForCall(decl->getSignature(), callExpr, argumentOffset)) {
                candidate.templateArgumentMapping = *mapping;
                for (const auto &[name, typeDesc] : *mapping) {
                    LKAssert(typeDesc->isResolved());
                    tempTypeIds.push_back(nominalTypes.insert(name, typeDesc->getResolvedType()));
                }
            } else {
                rejections.emplace_back("unable to resolve template parameter types", *decl);
                goto discard_potential_match;
            }
            
        //} else if (sig.templateArgumentNames.empty() != decl->getResolvedTemplateArgTypes().empty()) {
        } else if (sig.isTemplateDecl() != decl->getSignature().isTemplateDecl()) {
            // Discard already instantiated template functions. Not necessarily necessary,
            // but also not a huge issue since it'll just resolve to the already instantiated version
            //util::fmt::print("[{}] discarding {} bc already instantiated", targetName, decl->getName());
            goto discard_potential_match;
        }
        
        for (size_t idx = 0; idx < lastTypecheckedArgument; idx++) {
            // These checks deliberately ignore the implicit self parameter.
            // Since the list of possible targets was fetched based on the receiver type, this isn't an issue
            auto arg = callExpr->arguments[idx];
            auto argTy = argTypes[idx + argumentOffset];
            Type *expectedTy = nullptr;

            if (idx < sig.paramTypes.size()) {
                expectedTy = resolveTypeDesc(sig.paramTypes[idx + argumentOffset], false);
            } else {
                LKFatalError("is this non-C-linkage varargs?");
            }

            LKAssert(expectedTy);
            
            if (argTy == expectedTy) {
                continue;
            }
            
            if (arg->isOfKind(NK::NumberLiteral)) {
                auto numberLiteral = llvm::dyn_cast<ast::NumberLiteral>(arg);
                if (valueIsTriviallyConvertible(numberLiteral, expectedTy)) {
                    candidate.implicitConversionArgumentIndices.push_back(idx);
                    continue;
                } else {
                    auto str = util::fmt::format("argument #{} not trivially convertible from '{}' to '{}'", idx, argTy, expectedTy);
                    rejections.emplace_back(str, *decl);
                    goto discard_potential_match;
                }
            }
            
            if (expectedTy->isReferenceTy()) {
                if (!isTemporary(arg) && !argTy->isReferenceTy() && argTy->getReferenceTo() == expectedTy) {
                    continue;
                } // TODO if we reach here bc we're passing an arg that cannot become an lvalue to a function expecting an lvalue reference, add that to the reason!
            } else if (!expectedTy->isReferenceTy() && argTy->isReferenceTy()) {
                if (llvm::dyn_cast<ReferenceType>(argTy)->getReferencedType() == expectedTy) {
                    continue;
                }
            }
            
            rejections.emplace_back(util::fmt::format("argument #{} of type '{}' incompatible with expected type '{}'", idx, argTy, expectedTy), *decl);
            goto discard_potential_match;
        }
        
        candidates.push_back(candidate);
        
    discard_potential_match:
        nominalTypes.removeAll(tempTypeIds);
    }
    
    
    std::map<FunctionCallTargetCandidate::ID, std::vector<FunctionCallTargetCandidate::ID>> ambiguousCandidates;
    
    std::sort(candidates.begin(), candidates.end(), [&](const FunctionCallTargetCandidate &lhs, const FunctionCallTargetCandidate &rhs) -> bool {
        switch (lhs.compare(*this, rhs, callExpr, argTypes)) {
            case CandidateViabilityComparisonResult::MoreViable:
                return true;
            case CandidateViabilityComparisonResult::LessViable:
                return false;
            case CandidateViabilityComparisonResult::Ambiguous:
                ambiguousCandidates[lhs.id].push_back(rhs.id);
                ambiguousCandidates[rhs.id].push_back(lhs.id);
                return false;
        }
    });
    
    
    if (candidates.empty()) {
        for (const auto &rejection : rejections) {
            diagnostics::emitNote(rejection.decl.getSourceLocation(), util::fmt::format("not viable: {}", rejection.reason));
        }
        diagnostics::emitError(callExpr->getSourceLocation(), util::fmt::format("unable to resolve call to '{}'", targetName));
    }
    
    
    if (candidates.size() > 1 && !ambiguousCandidates[candidates[0].id].empty()) {
        for (const auto &candidate : candidates) {
            std::ostringstream OS;
            OS << "potential target";
            if (candidate.getSignature().isTemplateDecl()) {
                OS << " [with ";
                util::map::iterl(candidate.templateArgumentMapping, [&OS](auto &name, auto &typeDesc, bool isLast) {
                    OS << name << " = " << typeDesc->str();
                    if (!isLast) OS << ", ";
                });
                OS << "]";
            }
            diagnostics::emitNote(candidate.getSignature().getSourceLocation(), OS.str());
        }
        
        auto msg = util::fmt::format("ambiguous call to '{}'", mangling::demangleCanonicalName(targetName));
        diagnostics::emitError(callExpr->getSourceLocation(), msg);
    }
    
    
    
    auto bestMatch = candidates.front();
    
    if (auto &attr = bestMatch.target.funcDecl->getAttributes();
        !skipCodegen && attr.int_isDelayed && !bestMatch.getSignature().isTemplateDecl())
    {
        attr.int_isDelayed = false;
        withCleanSlate([&]() {
            codegen(bestMatch.target.funcDecl);
        });
    }
    
    if (bestMatch.getSignature().isTemplateDecl() && !bestMatch.target.llvmValue) {
        return specializeTemplateFunctionDeclForCallExpr(bestMatch.target.funcDecl, bestMatch.templateArgumentMapping, argumentOffset, codegenOption);
    }
    return ResolvedCallable(bestMatch.target.funcDecl, bestMatch.target.llvmValue, argumentOffset);
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


llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::CallExpr> call, ValueKind VK) {
    emitDebugLocation(call);
    
    auto resolvedTarget = resolveCall(call, kRunCodegen);
    
    if (resolvedTarget.funcDecl->getAttributes().int_isCtor) {
        auto structTy = llvm::dyn_cast<StructType>(resolvedTarget.funcDecl->getImplType());
        return constructStruct(structTy, call, /*putInLocalScope*/ false, VK); // TODO should `putInLocalScope` be true?
    }
    
    
    // TODO:
    // - run argument type checks for intrinsics as well
    // - check that the number of supplied explicit template arguments does't exceed the total number of supplied template arguments
    
    
    if (auto calledFuncDecl = resolvedTarget.funcDecl) {
        // TODO properly implement side effects!
        if (!callerCalleeSideEffectsCompatible(currentFunction.decl->getAttributes().side_effects, calledFuncDecl->getAttributes().side_effects)) {
            auto targetName = mangling::mangleCanonicalName(calledFuncDecl);
            LKFatalError("cannot call '%s' because side effects", targetName.c_str());
        }
    }
    
    enum class ArgumentHandlingPolicy {
        PassByValue,
        PassByValue_ExtractReference,
        PassByReference
    };
    std::map<uint64_t, ArgumentHandlingPolicy> argumentHandlingPolicies;
    
    for (size_t i = resolvedTarget.argumentOffset; i < resolvedTarget.signature.paramTypes.size(); i++) {
        auto expectedType = resolveTypeDesc(resolvedTarget.signature.paramTypes[i]);
        auto expr = call->arguments[i - resolvedTarget.argumentOffset];
        argumentHandlingPolicies[i] = ArgumentHandlingPolicy::PassByValue;
        Type *T;
        if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&expr, expectedType, &T)) {
            auto exprTy = getType(expr);
            if (expectedType->isReferenceTy() && !exprTy->isReferenceTy() && exprTy->getReferenceTo() == expectedType) {
                argumentHandlingPolicies[i] = ArgumentHandlingPolicy::PassByReference;
                goto cont;
            } else if (!expectedType->isReferenceTy() && exprTy->isReferenceTy() && expectedType->getReferenceTo() == exprTy) {
                argumentHandlingPolicies[i] = ArgumentHandlingPolicy::PassByValue_ExtractReference;
                goto cont;
            }
            
            auto msg = util::fmt::format("incompatible type for argument #{}. Expected '{}', got '{}'", i, expectedType, T);
            diagnostics::emitError(expr->getSourceLocation(), msg);
        }
        cont:
        // TODO is modifying the arguments in-place necessarily a good idea?
        call->arguments[i - resolvedTarget.argumentOffset] = expr;
    }
    
    
    if (resolvedTarget.funcDecl && resolvedTarget.funcDecl->getAttributes().intrinsic) {
        emitDebugLocation(call);
        return codegen_HandleIntrinsic(resolvedTarget.funcDecl, call);
    }
    
    llvm::Value *llvmFunction = resolvedTarget.llvmValue;
    LKAssert(llvmFunction->getType()->isPointerTy() && llvmFunction->getType()->getContainedType(0)->isFunctionTy());
    auto llvmFunctionTy = llvm::dyn_cast<llvm::FunctionType>(llvmFunction->getType()->getContainedType(0));
    auto isVariadic = llvmFunctionTy->isVarArg();
    
    LKAssert(call->arguments.size() >= llvmFunctionTy->getNumParams() - resolvedTarget.argumentOffset - isVariadic);
    
    std::vector<llvm::Value *> args(resolvedTarget.argumentOffset, nullptr);
    auto numFixedArgs = llvmFunctionTy->getNumParams() - resolvedTarget.argumentOffset;

    
    // TODO what about just adding the implicit argument(s) to the callExpr and getting rid of the whole argumentOffset dance?
    for (uint64_t i = resolvedTarget.argumentOffset; i < llvmFunctionTy->getNumParams(); i++) {
        auto expr = call->arguments[i - resolvedTarget.argumentOffset];
        auto argTy = getType(expr);
        auto expectedTy = resolveTypeDesc(resolvedTarget.signature.paramTypes[i]);
        
        const auto policy = argumentHandlingPolicies[i];
        
        if (policy == ArgumentHandlingPolicy::PassByReference) {
            args.push_back(codegen(expr, LValue));
            continue;
        }
        
        if (policy == ArgumentHandlingPolicy::PassByValue && expectedTy->isReferenceTy() && argTy->isReferenceTy()) {
            args.push_back(codegen(expr, RValue));
            continue;
        }
        
        bool didConstructCopy;
        auto V = constructCopyIfNecessary(argTy, expr, &didConstructCopy);
        if (policy == ArgumentHandlingPolicy::PassByValue_ExtractReference) {
            LKAssert(argTy->isReferenceTy());
            if (!didConstructCopy) {
                V = builder.CreateLoad(V); // TODO only insert this load if no copy was made? (otherwise, the copy constructor already returns a non-reference object)
            }
        }
        args.push_back(V);
    }
    
    if (resolvedTarget.argumentOffset == kInstanceMethodCallArgumentOffset) {
        std::shared_ptr<ast::Expr> implicitSelfArg;
        
        if (call->target->isOfKind(NK::MemberExpr) && !resolvedTarget.funcDecl->isCallOperatorOverload()) {
            implicitSelfArg = llvm::dyn_cast<ast::MemberExpr>(call->target)->target;
        } else {
            implicitSelfArg = call->target;
        }
        
        
        // TODO this is missing checks to make sure selfTy actually matches / is convertible to the expected argument type !?
        
        
        if (isTemporary(implicitSelfArg)) {
            // if an instance method call target is a temporary, we need to make sure the object outlives the call
            // we do this by putting it on the stack, thus implicitly registering it for destruction once we leave the current scope
            // TODO the object should be destructed immediately after the call returns / at the end of the enclosing statement !
            auto ident = makeIdent(currentFunction.getTmpIdent(), implicitSelfArg->getSourceLocation());
            auto varDecl = std::make_shared<ast::VarDecl>(ident, nullptr, implicitSelfArg);
            args[0] = codegen(varDecl);
        } else {
            args[0] = codegen(implicitSelfArg, LValue);
        }
    }
    
    
    if (isVariadic && getResolvedFunctionWithName(llvmFunction->getName().str())->funcDecl->getAttributes().extern_) {
        // TODO extract references if possible, disallow otherwise, promote types as expected by C?
        for (auto it = call->arguments.begin() + numFixedArgs; it != call->arguments.end(); it++) {
            auto arg = *it;
            auto argTy = getType(arg);
            auto V = codegen(arg, RValue);
            
            if (argTy == builtinTypes.yo.f32) {
                V = builder.CreateFPCast(V, builtinTypes.llvm.Double);
            }
            
            args.push_back(V);
        }
    } else if (isVariadic) {
        LKFatalError("TODO: implement");
    }
    
    emitDebugLocation(call);
    // TODO do we need to take VK into account here?
    return builder.CreateCall(llvmFunction, args);
}






#pragma mark - Intrinsics


enum class irgen::Intrinsic : uint8_t {
    Unknown,
    
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    
    EQ,
    LT,
    
    LAnd,
    LOr,
    
    StaticCast,
    ReinterpretCast,
    Sizeof,
    Trap, Typename,
    IsSame, IsPointer,
    IsConstructible, IsDestructible,
    Func, PrettyFunc, MangledFunc
};

static const std::map<std::string, Intrinsic> intrinsics = {
    { "__add", Intrinsic::Add },
    { "__sub", Intrinsic::Sub },
    { "__mul", Intrinsic::Mul },
    { "__div", Intrinsic::Div },
    { "__mod", Intrinsic::Mod },
    { "__and", Intrinsic::And },
    { "__or",  Intrinsic::Or  },
    { "__xor", Intrinsic::Xor },
    { "__shl", Intrinsic::Shl },
    { "__shr", Intrinsic::Shr },
    
    { "__eq", Intrinsic::EQ },
    { "__lt", Intrinsic::LT },
    
    { "__land", Intrinsic::LAnd },
    { "__lor",  Intrinsic::LOr  },
    { mangling::mangleCanonicalName(ast::Operator::LAnd), Intrinsic::LAnd },
    { mangling::mangleCanonicalName(ast::Operator::LOr),  Intrinsic::LOr  },
    
    { "cast", Intrinsic::StaticCast },
    { "bitcast", Intrinsic::ReinterpretCast },
    { "sizeof", Intrinsic::Sizeof },
    { "__trap", Intrinsic::Trap },
    { "__typename", Intrinsic::Typename },
    { "__is_same", Intrinsic::IsSame },
    { "__is_pointer", Intrinsic::IsPointer },
    { "__is_constructible", Intrinsic::IsConstructible },
    { "__is_destructible", Intrinsic::IsDestructible },
    { "__func", Intrinsic::Func },
    { "__pretty_func", Intrinsic::PrettyFunc },
    { "__mangled_func", Intrinsic::MangledFunc }
};


static const std::map<Intrinsic, ast::Operator> intrinsicsArithmeticOperationMapping = {
#define MAPPING(name) { Intrinsic::name, ast::Operator::name },
    MAPPING(Add) MAPPING(Sub) MAPPING(Mul) MAPPING(Div) MAPPING(Mod)
    MAPPING(And) MAPPING(Or) MAPPING(Xor)
    MAPPING(Shl) MAPPING(Shr)
#undef MAPPING
};

static const std::map<Intrinsic, ast::Operator> intrinsicsComparisonOperationMapping = {
#define MAPPING(name) { Intrinsic::name, ast::Operator::name },
    MAPPING(EQ) MAPPING(LT)
#undef MAPPING
};


llvm::Value *IRGenerator::codegen_HandleIntrinsic(std::shared_ptr<ast::FunctionDecl> funcDecl, std::shared_ptr<ast::CallExpr> call) {
    emitDebugLocation(call);
    
    auto name = mangling::mangleCanonicalName(funcDecl);
    auto intrinsic = intrinsics.at(name);
    
    switch (intrinsic) {
        case Intrinsic::StaticCast:
        case Intrinsic::ReinterpretCast: {
            if (call->numberOfExplicitTemplateArgs() != 1) {
                auto msg = util::fmt::format("invalid number of explicit template arguments. expected 1, got {}", call->numberOfExplicitTemplateArgs());
                diagnostics::emitError(call->getSourceLocation(), msg);
            }
            auto dstTy = call->explicitTemplateArgs->at(0);
            auto arg = call->arguments[0];
            auto castKind = intrinsic == Intrinsic::StaticCast
                ? ast::CastExpr::CastKind::StaticCast
                : ast::CastExpr::CastKind::Bitcast;
            auto castExpr = std::make_shared<ast::CastExpr>(arg, dstTy, castKind);
            castExpr->setSourceLocation(funcDecl->getSourceLocation());
            return codegen(castExpr);
        }
        
        case Intrinsic::Sizeof: {
            auto ty = resolveTypeDesc(call->explicitTemplateArgs->at(0))->getLLVMType();
            return llvm::ConstantInt::get(builtinTypes.llvm.i64, module->getDataLayout().getTypeAllocSize(ty));
        }
        
        case Intrinsic::Trap:
            return builder.CreateIntrinsic(llvm::Intrinsic::ID::trap, {}, {});
        
        case Intrinsic::Typename: {
            auto ty = resolveTypeDesc(call->explicitTemplateArgs->at(0));
            return builder.CreateGlobalStringPtr(ty->str_mangled());
        }
        
        case Intrinsic::IsSame: {
            auto ty1 = resolveTypeDesc(call->explicitTemplateArgs->at(0));
            auto ty2 = resolveTypeDesc(call->explicitTemplateArgs->at(1));
            return llvm::ConstantInt::get(builtinTypes.llvm.i1, ty1 == ty2);
        }
        
        case Intrinsic::IsPointer:
            return llvm::ConstantInt::get(builtinTypes.llvm.i1, resolveTypeDesc(call->explicitTemplateArgs->at(0))->isPointerTy());
        
        case Intrinsic::IsConstructible: {
            auto ty = resolveTypeDesc(call->explicitTemplateArgs->at(0));
            return llvm::ConstantInt::get(builtinTypes.llvm.i1, typeIsConstructible(ty));
        }
        
        case Intrinsic::IsDestructible: {
            auto ty = resolveTypeDesc(call->explicitTemplateArgs->at(0));
            return llvm::ConstantInt::get(builtinTypes.llvm.i1, typeIsDestructible(ty));
        }
        
        case Intrinsic::LAnd:
        case Intrinsic::LOr:
            LKAssert(call->arguments.size() == 2);
            return codegen_HandleLogOpIntrinsic(intrinsic, call);
        
        case Intrinsic::Func:
            return builder.CreateGlobalStringPtr(currentFunction.decl->getName());
        
        case Intrinsic::PrettyFunc: {
            std::ostringstream OS;
            OS << currentFunction.decl->getName();
            OS << currentFunction.decl->getSignature();
            return builder.CreateGlobalStringPtr(OS.str());
        }
        
        case Intrinsic::MangledFunc:
            return builder.CreateGlobalStringPtr(currentFunction.llvmFunction->getName());
        
        default: break;
    }
    
    
    if (auto op = util::map::get_opt(intrinsicsArithmeticOperationMapping, intrinsic)) {
        LKAssert(call->arguments.size() == 2);
        return codegen_HandleArithmeticIntrinsic(op.value(), call);
    }
    
    if (auto op = util::map::get_opt(intrinsicsComparisonOperationMapping, intrinsic)) {
        LKAssert(call->arguments.size() == 2);
        return codegen_HandleComparisonIntrinsic(op.value(), call);
    }
    
    
    diagnostics::emitError(call->getSourceLocation(), util::fmt::format("unhandled call to intrinsic '{}'", name));
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


llvm::Value* IRGenerator::codegen_HandleArithmeticIntrinsic(ast::Operator op, std::shared_ptr<ast::CallExpr> call) {
    //emitDebugLocation(lhs);
    LKAssert(call->arguments.size() == 2);
    
    Type *lhsTy = nullptr, *rhsTy = nullptr;
    
    auto lhs = call->arguments.at(0);
    auto rhs = call->arguments.at(1);
    
    if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(&lhs, &rhs, &lhsTy, &rhsTy)) {
        auto msg = util::fmt::format("unable to create binop for operand types '{}' and '{}'", lhsTy, rhsTy);
        diagnostics::emitError(call->getSourceLocation(), msg);
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
    
    const auto llvmOp = numTy->isFloatTy() ? getLLVMBinaryOpInstruction_Float(op) : getLLVMBinaryOpInstruction_Int(op, numTy->isSigned());
    const auto lhsVal = codegen(lhs);
    const auto rhsVal = codegen(rhs);
    emitDebugLocation(call);
    return builder.CreateBinOp(llvmOp, lhsVal, rhsVal);
}



llvm::CmpInst::Predicate getMatchingLLVMCmpInstPredicateForComparisonOperator_Int(ast::Operator op, bool isSigned) {
    using Op = ast::Operator;
    using Pred = llvm::CmpInst::Predicate;
    
    switch (op) {
        case Op::EQ: return Pred::ICMP_EQ;
        case Op::LT: return isSigned ? Pred::ICMP_SLT : Pred::ICMP_ULT;
        default: LKFatalError("");
    }
}


llvm::CmpInst::Predicate getMatchingLLVMCmpInstPredicateForComparisonOperator_Float(ast::Operator op) {
    using Op = ast::Operator;
    using Pred = llvm::CmpInst::Predicate;
    
    switch (op) {
        case Op::EQ: return Pred::FCMP_OEQ;
        case Op::LT: return Pred::FCMP_OLT;
        default: LKFatalError("");
    }
}



llvm::Value* IRGenerator::codegen_HandleComparisonIntrinsic(ast::Operator op, std::shared_ptr<ast::CallExpr> call) {
    LKAssert(call->arguments.size() == 2);
    
    auto lhsExpr = call->arguments.at(0);
    auto rhsExpr = call->arguments.at(1);
    
    auto lhsTy = getType(lhsExpr);
    auto rhsTy = getType(rhsExpr);
    
    
    // Pointers?
    if (lhsTy->isPointerTy() && rhsTy->isPointerTy()) {
        if (lhsTy != rhsTy) {
            auto msg = util::fmt::format("cannot compare pointers to unrelated types '{}' and '{}'", lhsTy, rhsTy);
            diagnostics::emitError(call->getSourceLocation(), msg);
        }
        auto lhs = codegen(lhsExpr);
        auto rhs = codegen(rhsExpr);
        
        emitDebugLocation(call);
        return builder.CreateICmpEQ(lhs, rhs);
    }
    
    // Floats?
    if (lhsTy == rhsTy && lhsTy == builtinTypes.yo.f64) {
        auto lhs = codegen(lhsExpr);
        auto rhs = codegen(rhsExpr);
        auto pred = getMatchingLLVMCmpInstPredicateForComparisonOperator_Float(op);
        
        emitDebugLocation(call);
        return builder.CreateFCmp(pred, lhs, rhs);
    }
    
    
    if (!(lhsTy->isNumericalTy() && rhsTy->isNumericalTy())) {
        auto msg = util::fmt::format("no known comparison for unrelated types '{}' and '{}'", lhsTy, rhsTy);
        diagnostics::emitError(call->getSourceLocation(), msg);
    }
    
    
    // From here onwards, we assume that we're comparing two numeric values
    
    LKAssert(lhsTy->isNumericalTy() && rhsTy->isNumericalTy());
    
    llvm::CmpInst::Predicate pred;
    llvm::Value *lhsVal, *rhsVal;
    
    auto numTyLhs = llvm::dyn_cast<NumericalType>(lhsTy);
    auto numTyRhs = llvm::dyn_cast<NumericalType>(rhsTy);
    
    if (numTyLhs == numTyRhs) {
        pred = getMatchingLLVMCmpInstPredicateForComparisonOperator_Int(op, numTyLhs->isSigned());
        lhsVal = codegen(lhsExpr);
        rhsVal = codegen(rhsExpr);
    } else {
        // Both are integers, but different types
        
        Type *castDestTy;
        auto largerSize = std::max(numTyLhs->getSize(), numTyRhs->getSize());
        
        if (largerSize <= builtinTypes.yo.i32->getSize()) {
            castDestTy = builtinTypes.yo.i32;
        } else {
            LKAssert(largerSize == builtinTypes.yo.i64->getSize());
            castDestTy = builtinTypes.yo.i64;
        }
        
        auto lhsCast = std::make_shared<ast::CastExpr>(lhsExpr, ast::TypeDesc::makeResolved(castDestTy), ast::CastExpr::CastKind::StaticCast);
        lhsCast->setSourceLocation(lhsExpr->getSourceLocation());
        auto rhsCast = std::make_shared<ast::CastExpr>(rhsExpr, ast::TypeDesc::makeResolved(castDestTy), ast::CastExpr::CastKind::StaticCast);
        rhsCast->setSourceLocation(rhsExpr->getSourceLocation());
        
        lhsVal = codegen(lhsCast);
        rhsVal = codegen(rhsCast);
        pred = getMatchingLLVMCmpInstPredicateForComparisonOperator_Int(op, numTyLhs->isSigned() || numTyRhs->isSigned());
        
    }
    
    emitDebugLocation(call);
    return builder.CreateICmp(pred, lhsVal, rhsVal);
}





llvm::Value* IRGenerator::codegen_HandleLogOpIntrinsic(Intrinsic I, std::shared_ptr<ast::CallExpr> call) {
    LKAssert(call->arguments.size() == 2);
    LKAssert(I == Intrinsic::LAnd || I == Intrinsic::LOr);
    
    const auto lhs = call->arguments[0];
    const auto rhs = call->arguments[1];
    
    LKAssert(getType(lhs) == builtinTypes.yo.Bool && getType(rhs) == builtinTypes.yo.Bool);
    
    auto isAnd = I == Intrinsic::LAnd;
    
    auto llvmTrueVal = llvm::ConstantInt::getTrue(builtinTypes.llvm.i1);
    auto llvmFalseVal = llvm::ConstantInt::getFalse(builtinTypes.llvm.i1);
    auto F = currentFunction.llvmFunction;
    
    auto lhsBB = builder.GetInsertBlock();
    auto rhsBB = llvm::BasicBlock::Create(C, "rhs");
    auto mergeBB = llvm::BasicBlock::Create(C, "merge");
    
    auto lhsCmp = builder.CreateICmpEQ(codegen(lhs), llvmTrueVal);
    
    emitDebugLocation(call); // call's SL is the original binop's SL
    builder.CreateCondBr(lhsCmp,
                         isAnd ? rhsBB : mergeBB,
                         isAnd ? mergeBB : rhsBB);
    
    
    F->getBasicBlockList().push_back(rhsBB);
    builder.SetInsertPoint(rhsBB);
    auto rhsVal = builder.CreateICmpEQ(codegen(rhs), llvmTrueVal);
    builder.CreateBr(mergeBB);
    
    F->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);
    
    emitDebugLocation(call);
    auto phi = builder.CreatePHI(builtinTypes.llvm.i1, 2);
    phi->addIncoming(isAnd ? llvmFalseVal : llvmTrueVal, lhsBB);
    phi->addIncoming(rhsVal, rhsBB);
    
    return phi;
}






#pragma mark - Control Flow

llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::IfStmt> ifStmt) {
    // TODO does this need more debug locations?
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
    
    enum class ConstevalResult {
        None,           // no consteval going on
        FirstBranch,    // the first branch evaluated to true
        SecondBranch,   // the second branch evaluated to true
        NoBranch        // no branch evaluated to true
    };
    auto constevalResult = ConstevalResult::None;
    
    for (size_t i = 0; i < ifStmt->branches.size(); i++) {
        const auto &branch = ifStmt->branches[i];
        
        if (branch->kind == BK::Else) break;
        if (i > 0) {
            auto BB = branchConditionBlocks[i];
            F->getBasicBlockList().push_back(BB);
            builder.SetInsertPoint(BB);
        }
        
        auto condTy = getType(branch->condition);
        
        if (auto BoolTy = builtinTypes.yo.Bool; condTy != BoolTy && condTy != BoolTy->getReferenceTo()) {
            auto msg = util::fmt::format("type of expression ('{}') incompatible with expected type '{}'", condTy, BoolTy);
            diagnostics::emitError(branch->getSourceLocation(), msg);
        }
        
        auto condV = codegen(branch->condition);
        if (condTy == builtinTypes.yo.Bool->getReferenceTo()) {
            condV = builder.CreateLoad(condV);
        }
        
        if (i == 0 && llvm::isa<llvm::ConstantInt>(condV) && condV->getType() == builtinTypes.llvm.i1
            && branch->kind == BK::If && (ifStmt->branches.size() == 1 || ifStmt->branches[1]->kind == BK::Else)
        ) {
            auto value = llvm::dyn_cast<llvm::ConstantInt>(condV)->getLimitedValue();
            if (value == 1) {
                constevalResult = ConstevalResult::FirstBranch;
                builder.CreateBr(branchBodyBlocks[i]);
            } else if (ifStmt->branches.size() == 2) {
                constevalResult = ConstevalResult::SecondBranch;
                builder.CreateBr(branchBodyBlocks[i+1]);
            } else {
                constevalResult = ConstevalResult::NoBranch;
                needsMergeBB = true;
                builder.CreateBr(mergeBB);
            }
            break;
        } else {
            builder.CreateCondBr(condV, branchBodyBlocks[i], branchConditionBlocks[i + 1]);
        }
    }
    
    auto codegenBodyBranch = [&](uint64_t index) {
        auto BB = branchBodyBlocks[index];
        F->getBasicBlockList().push_back(BB);
        builder.SetInsertPoint(BB);
        
        codegen(ifStmt->branches[index]->body);
        if (builder.GetInsertBlock()->empty() || !builder.GetInsertBlock()->back().isTerminator()) {
            needsMergeBB = true;
            builder.CreateBr(mergeBB);
        }
    };
    
    
    switch (constevalResult) {
        case ConstevalResult::None:
            for (size_t i = 0; i < ifStmt->branches.size(); i++) {
                codegenBodyBranch(i);
            }
            break;
        
        case ConstevalResult::FirstBranch:
            codegenBodyBranch(0);
            break;
        
        case ConstevalResult::SecondBranch:
            codegenBodyBranch(1);
            break;
        
        case ConstevalResult::NoBranch:
            break;
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
    
    currentFunction.breakContDestinations.push({mergeBB, condBB});
    
    codegen(whileStmt->body);
    builder.CreateBr(condBB);
    
    currentFunction.breakContDestinations.pop();
    
    F->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);
    
    return nullptr;
}



std::shared_ptr<ast::CallExpr> makeInstanceMethodCallExpr(std::shared_ptr<ast::Expr> target, std::string methodName) {
    auto callTarget = std::make_shared<ast::MemberExpr>(target, methodName);
    callTarget->setSourceLocation(target->getSourceLocation());
    
    auto call = std::make_shared<ast::CallExpr>(callTarget);
    call->setSourceLocation(target->getSourceLocation());
    
    return call;
}



llvm::Value *IRGenerator::codegen(std::shared_ptr<ast::ForLoop> forLoop) {
    auto targetTy = getType(forLoop->expr);
    
    if (!implementsInstanceMethod(targetTy, kIteratorMethodName)) {
        auto msg = util::fmt::format("expression of type '{}' is not iterable", targetTy);
        diagnostics::emitError(forLoop->expr->getSourceLocation(), msg);
    }
    
    
    auto loopExprIdent = makeIdent(currentFunction.getTmpIdent(), forLoop->getSourceLocation());
    auto iteratorIdent = makeIdent(currentFunction.getTmpIdent(), forLoop->getSourceLocation());
    
    // make sure the target's lifetime exceeds the iterator's
    auto loopExprVarDecl = std::make_shared<ast::VarDecl>(loopExprIdent, nullptr, forLoop->expr);
    if (!isTemporary(forLoop->expr)) {
        loopExprVarDecl->declaresUntypedReference = true;
    }
    
    auto iteratorCallExpr = makeInstanceMethodCallExpr(loopExprIdent, kIteratorMethodName);
    iteratorCallExpr->setSourceLocation(forLoop->getSourceLocation());
    
    // let it = <target>.iterator();
    auto iteratorVarDecl = std::make_shared<ast::VarDecl>(iteratorIdent, nullptr, iteratorCallExpr);
    iteratorVarDecl->setSourceLocation(forLoop->getSourceLocation());
    
    
    // while it.hasNext()
    auto whileCond = makeInstanceMethodCallExpr(iteratorIdent, kIteratorHasNextMethodName);
    auto whileBody = std::make_shared<ast::CompoundStmt>();
    whileBody->setSourceLocation(forLoop->body->getSourceLocation());
    
    // let [&]<ident> = it.next();
    auto nextElemCall = makeInstanceMethodCallExpr(iteratorIdent, kIteratorNextMethodName);
    nextElemCall->setSourceLocation(forLoop->expr->getSourceLocation());
    auto elemDecl = std::make_shared<ast::VarDecl>(forLoop->ident, nullptr, nextElemCall);
    elemDecl->declaresUntypedReference = forLoop->capturesByReference;
    elemDecl->setSourceLocation(forLoop->ident->getSourceLocation());
    
    whileBody->statements.push_back(elemDecl);
    util::vector::append(whileBody->statements, forLoop->body->statements);
    
    auto whileStmt = std::make_shared<ast::WhileStmt>(whileCond, whileBody);
    whileStmt->setSourceLocation(forLoop->getSourceLocation());
    
    // Wrap in a compound to make sure the iterator gets deallocated immediately after the loop exits
    auto stmt = std::make_shared<ast::CompoundStmt>();
    stmt->setSourceLocation(forLoop->getSourceLocation());
    stmt->statements.push_back(loopExprVarDecl);
    stmt->statements.push_back(iteratorVarDecl);
    stmt->statements.push_back(whileStmt);
    return codegen(stmt);
}




llvm::Value* IRGenerator::codegen(std::shared_ptr<ast::BreakContStmt> stmt) {
    if (currentFunction.breakContDestinations.empty()) {
        auto msg = util::fmt::format("'{}' statement may only be used in a loop", stmt->isBreak() ? "break" : "continue");
        diagnostics::emitError(stmt->getSourceLocation(), msg);
    }
    
    llvm::BasicBlock *dest;
    const auto &destBBs = currentFunction.breakContDestinations.top();
    if (stmt->isBreak()) {
        dest = destBBs.breakDest;
    } else {
        dest = destBBs.contDest;
    }
    
    // TODO clear stack?!
    return builder.CreateBr(dest);
}







#pragma mark - Globals


void IRGenerator::handleStartupAndShutdownFunctions() {
    std::vector<llvm::Type *> structTys = {
        builtinTypes.llvm.i32, llvm::FunctionType::get(builtinTypes.llvm.Void, false)->getPointerTo(), builtinTypes.llvm.i8Ptr
    };
    auto structTy = llvm::StructType::create(C, structTys);
    
    auto imp = [&](llvm::StringRef dest, bool attributes::FunctionAttributes::* attr) {
        std::vector<ResolvedCallable> functions;

        for (const auto &[name, callable] : resolvedFunctions) {
            if (callable.funcDecl && callable.funcDecl->getAttributes().*attr) {
                functions.push_back(callable);
            }
        }
        
        if (functions.empty()) return;
        
        auto arrayTy = llvm::ArrayType::get(structTy, functions.size());
        module->getOrInsertGlobal(dest, arrayTy);
        
        std::vector<llvm::Constant *> arrayElements;
        for (const auto &fn : functions) {
            std::vector<llvm::Constant *> values = {
                llvm::ConstantInt::get(builtinTypes.llvm.i32, 65535), // TODO how should this be ordered?
                llvm::dyn_cast<llvm::Function>(fn.llvmValue),
                llvm::ConstantPointerNull::get(llvm::dyn_cast<llvm::PointerType>(builtinTypes.llvm.i8Ptr))
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




#pragma mark - Allocation & Memory Management


llvm::Value* IRGenerator::constructStruct(StructType *structTy, std::shared_ptr<ast::CallExpr> call, bool putInLocalScope, ValueKind VK) {
    if (auto SD = util::map::get_opt(templateStructs, structTy->getName())) {
        if (!call->explicitTemplateArgs) {
            LKFatalError("needs explicit args for instantiating template");
        }
        auto mapping = resolveStructTemplateParametersFromExplicitTemplateArgumentList(*SD, call->explicitTemplateArgs);
        structTy = instantiateTemplateStruct(*SD, mapping);
    }
    
    emitDebugLocation(call);
    auto alloca = builder.CreateAlloca(getLLVMType(structTy));
    auto ident = currentFunction.getTmpIdent();
    alloca->setName(ident);
    
    builder.CreateMemSet(alloca,
                         llvm::ConstantInt::get(builtinTypes.llvm.i8, 0),
                         module->getDataLayout().getTypeAllocSize(alloca->getAllocatedType()),
                         alloca->getAlignment());
    
    localScope.insert(ident, ValueBinding(structTy, alloca, [=]() {
        emitDebugLocation(call);
        return builder.CreateLoad(alloca);
    }, [=](llvm::Value *V) {
        LKFatalError("use references to write to object?");
    }, ValueBinding::Flags::ReadWrite));
    
    auto callTarget = std::make_shared<ast::MemberExpr>(makeIdent(ident), kInitializerMethodName);
    auto callExpr = std::make_shared<ast::CallExpr>(*call);
    callExpr->target = callTarget;
    
    codegen(callExpr);
    
    if (!putInLocalScope) {
        localScope.remove(ident);
    }
    
    // TODO should this even be allowed to return anything but an RValue?
    switch (VK) {
        case LValue:
            LKFatalError("why?");
            return alloca;
        case RValue:
            return builder.CreateLoad(alloca);
    }
}


llvm::Value* IRGenerator::constructCopyIfNecessary(Type *type, std::shared_ptr<ast::Expr> expr, bool *didConstructCopy) {
    if (!isTemporary(expr) && (type->isStructTy() || (type->isReferenceTy() && static_cast<ReferenceType *>(type)->getReferencedType()->isStructTy()))) {
        // TODO skip the copy constructor if the type is trivially copyable?
        StructType *structTy = llvm::dyn_cast<StructType>(type) ?: static_cast<StructType *>(static_cast<ReferenceType *>(type)->getReferencedType());
        auto call = std::make_shared<ast::CallExpr>(nullptr);
        call->setSourceLocation(expr->getSourceLocation());
        call->arguments = { expr };
        if (didConstructCopy) *didConstructCopy = true;
        return constructStruct(structTy, call, /*putInLocalScope*/ false, RValue);
    } else {
        if (didConstructCopy) *didConstructCopy = false;
        return codegen(expr);
    }
}


llvm::Value* IRGenerator::destructValueIfNecessary(Type *type, llvm::Value *value, bool includeReferences) {
    LKAssert(value->getType()->isPointerTy());
    auto expr = std::make_shared<ast::RawLLVMValueExpr>(value, type->isReferenceTy() ? type : type->getReferenceTo());
    if (auto destructStmt = createDestructStmtIfDefined(type, expr, includeReferences)) {
        return codegen(destructStmt);
    } else {
        return nullptr;
    }
}

std::shared_ptr<ast::LocalStmt> IRGenerator::createDestructStmtIfDefined(Type *type, llvm::Value *value, bool includeReferences) {
    auto expr = std::make_shared<ast::RawLLVMValueExpr>(value, type->isReferenceTy() ? type : type->getReferenceTo());
    return createDestructStmtIfDefined(type, expr, includeReferences);
}

std::shared_ptr<ast::LocalStmt> IRGenerator::createDestructStmtIfDefined(Type *type, std::shared_ptr<ast::Expr> expr, bool includeReferences) {
    if (includeReferences && type->isReferenceTy()) type = static_cast<ReferenceType *>(type)->getReferencedType();
    
    auto structTy = llvm::dyn_cast<StructType>(type);
    if (!structTy) return nullptr;
    
    auto canonicalDeallocName = mangling::mangleCanonicalName(structTy->getName(), kSynthesizedDeallocMethodName, ast::FunctionKind::InstanceMethod);
    if (!util::map::has_key(functions, canonicalDeallocName)) return nullptr;
    
    auto callExpr = std::make_shared<ast::CallExpr>(makeIdent(canonicalDeallocName));
    callExpr->arguments = { expr };
    callExpr->setSourceLocation(currentFunction.decl->getSourceLocation());
    
    auto stmt = std::make_shared<ast::ExprStmt>(callExpr);
    stmt->setSourceLocation(callExpr->getSourceLocation());
    return stmt;
}


void IRGenerator::includeInStackDestruction(Type *type, llvm::Value *value) {
    auto ident = currentFunction.getTmpIdent();
    value->setName(ident);
    localScope.insert(ident, ValueBinding(type, value, nullptr, nullptr, ValueBinding::Flags::None));
}


void IRGenerator::destructLocalScopeUntilMarker(NamedScope<ValueBinding>::Marker M, bool removeFromLocalScope) {
    auto entries = localScope.getEntriesSinceMarker(M);
    
    for (auto it = entries.rbegin(); it != entries.rend(); it++) {
        const auto &[name, id, binding] = *it;
        if (!binding.hasFlag(ValueBinding::Flags::DontDestroy)) {
            destructValueIfNecessary(binding.type, binding.value, /*includeReferences*/ false);
        }
    }
    if (removeFromLocalScope) {
        localScope.removeAllSinceMarker(M);
    }
}



#pragma mark - Types


Type* IRGenerator::resolvePrimitiveType(std::string_view name) {
#define HANDLE(_name, ty) if (name == _name) return ty;
    HANDLE("void", builtinTypes.yo.Void)
    HANDLE("bool", builtinTypes.yo.Bool)
    HANDLE("i8",   builtinTypes.yo.i8)
    HANDLE("i16",  builtinTypes.yo.i16)
    HANDLE("i32",  builtinTypes.yo.i32)
    HANDLE("i64",  builtinTypes.yo.i64)
    HANDLE("u8",   builtinTypes.yo.u8)
    HANDLE("u16",  builtinTypes.yo.u16)
    HANDLE("u32",  builtinTypes.yo.u32)
    HANDLE("u64",  builtinTypes.yo.u64)
    HANDLE("f32",  builtinTypes.yo.f32)
    HANDLE("f64",  builtinTypes.yo.f64)
#undef HANDLE
    return nullptr;
}



// Attempts to resolve an AST TypeDesc and returns a unique `yo::Type*` pointer.
// Also creates the `yo::Type`'s `llvm::Type` and `llvm::DIType` and sets the respective member fields
Type* IRGenerator::resolveTypeDesc(std::shared_ptr<ast::TypeDesc> typeDesc, bool setInternalResolvedType) {
    // HUGE FUCKING PROBLEM: typedescs should be resolved in the context which they were declared, not the one in which they might be used
    // (this isn't that big an issue rn, but might become in the future)
    
    using TDK = ast::TypeDesc::Kind;
    
    if (!typeDesc) LKFatalError("NULL TYPE DESC");
    
    auto handleResolvedTy = [this, typeDesc, setInternalResolvedType](Type *ty) {
        if (setInternalResolvedType) typeDesc->setResolvedType(ty);
        // these will set the respective members
        getLLVMType(ty);
        getDIType(ty);
        return ty;
    };
    
    if (auto ty = typeDesc->getResolvedType()) {
        return handleResolvedTy(ty);
    }
    
    if (typeDesc->isReference() && typeDesc->getPointee()->isReference()) {
        diagnostics::emitError(typeDesc->getSourceLocation(), "reference type cannot have indirection count > 1");
    }
    
    switch (typeDesc->getKind()) {
        case TDK::Resolved:
            // Should actually never reach here since we already have the nonnull check above
            return typeDesc->getResolvedType();
        
        case TDK::Nominal: {
            auto name = typeDesc->getName();
            if (auto ty = resolvePrimitiveType(name)) {
                return handleResolvedTy(ty);
            } else {// a nominal, non-primitive type
                
                if (auto entry = nominalTypes.get(name)) {
                    return handleResolvedTy(entry.value());
                }
                
                // TODO this is a disgusting workaround!!!!!
                // Basically the issue is that we need the mangled key for the map lookup
//                auto structDeclForNameMangling = std::make_shared<ast::StructDecl>();
//                structDeclForNameMangling->name = typeDesc->getName();
//                name = mangling::mangleFullyResolved(structDeclForNameMangling);
                name = mangling::mangleAsStruct(name);
                
                // If there is already an entry for that type, return that
                if (auto entry = nominalTypes.get(name)) {
                    return handleResolvedTy(entry.value());
                }
                
                diagnostics::emitError(typeDesc->getSourceLocation(), util::fmt::format("unable to resolve nominal type '{}'", name));
            }
            break;
        }
        case TDK::Pointer:
            return handleResolvedTy(resolveTypeDesc(typeDesc->getPointee(), setInternalResolvedType)->getPointerTo());
        
        case TDK::Reference:
            return handleResolvedTy(resolveTypeDesc(typeDesc->getPointee(), setInternalResolvedType)->getReferenceTo());
        
        case TDK::Tuple: {
            auto resolvedMembers = util::vector::map(typeDesc->getTupleMembers(), [&](auto &TD) -> Type* {
                return resolveTypeDesc(TD, setInternalResolvedType);
            });
            return handleResolvedTy(TupleType::get(resolvedMembers));
        }
        
        case TDK::Function: {
            LKFatalError("TODO: rewrite to return application handlers or whatever its called");
            const auto &FTI = typeDesc->getFunctionTypeInfo();
            const auto paramTypes = util::vector::map(FTI.parameterTypes, [&](const auto &TD) { return resolveTypeDesc(TD, setInternalResolvedType); });
            return handleResolvedTy(FunctionType::create(resolveTypeDesc(FTI.returnType, setInternalResolvedType), paramTypes, FTI.callingConvention));
        }
        
        case TDK::Decltype:
            return handleResolvedTy(getType(typeDesc->getDecltypeExpr()));
        
        case TDK::NominalTemplated: {
            auto structDecl = util::map::get_opt(templateStructs, typeDesc->getName());
            if (!structDecl) {
                diagnostics::emitError(typeDesc->getSourceLocation(), "unable to resolve type");
            }
            
            // Make sure the template args are resolved in the current context, instead of the clean-slate context used for instantiating the template
            // (this is important for example for decltype template args dependent on local variables)
            for (auto tmplArg : typeDesc->getTemplateArgs()) {
                resolveTypeDesc(tmplArg);
            }
            
            StructType *ST = withCleanSlate([&]() { return instantiateTemplateStruct(*structDecl, typeDesc); });
            return handleResolvedTy(ST);
        }
    }
    
    LKFatalError("unhandled type desc: %s", typeDesc->str().c_str());
}




bool IRGenerator::equal(const ast::FunctionSignature &lhs, const ast::FunctionSignature &rhs) {
    if (resolveTypeDesc(lhs.returnType, false) != resolveTypeDesc(rhs.returnType, false)) return false;
    
    if (lhs.paramTypes.size() != rhs.paramTypes.size()) return false;
    
    for (size_t i = 0; i < lhs.paramTypes.size(); i++) {
        if (resolveTypeDesc(lhs.paramTypes[i], false) != resolveTypeDesc(rhs.paramTypes[i], false)) return false;
    }
    
    // TODO this only compares address equality. too relaxed?
    if (lhs.templateParamsDecl != rhs.templateParamsDecl) return false;
    
    return true;
}




llvm::Type *IRGenerator::getLLVMType(Type *type) {
    if (auto T = type->getLLVMType()) return T;
    
    auto handle_llvm_type = [type](llvm::Type *llvmTy) -> llvm::Type* {
        type->setLLVMType(llvmTy);
        return llvmTy;
    };
    
    switch (type->getTypeId()) {
        case Type::TypeID::Void:
            return handle_llvm_type(builtinTypes.llvm.Void);
        
        case Type::TypeID::Numerical: {
            using NTID = NumericalType::NumericalTypeID;
            
            auto numTy = llvm::dyn_cast<NumericalType>(type);
            switch (numTy->getNumericalTypeID()) {
                case NTID::Bool:
                    return handle_llvm_type(builtinTypes.llvm.i1);
                
                case NTID::Int8:
                case NTID::UInt8:
                    return handle_llvm_type(builtinTypes.llvm.i8);
                
                case NTID::Int16:
                case NTID::UInt16:
                    return handle_llvm_type(builtinTypes.llvm.i16);
                
                case NTID::Int32:
                case NTID::UInt32:
                    return handle_llvm_type(builtinTypes.llvm.i32);
                
                case NTID::Int64:
                case NTID::UInt64:
                    return handle_llvm_type(builtinTypes.llvm.i64);
                
                case NTID::Float32:
                    return handle_llvm_type(builtinTypes.llvm.Float);
                
                case NTID::Float64:
                    return handle_llvm_type(builtinTypes.llvm.Double);
            }
        }
        
        case Type::TypeID::Pointer:
        case Type::TypeID::Reference: {
            Type *pointee = type->isPointerTy()
                ? llvm::dyn_cast<PointerType>(type)->getPointee()
                : llvm::dyn_cast<ReferenceType>(type)->getReferencedType();
            return handle_llvm_type(getLLVMType(pointee)->getPointerTo());
        }
        
        case Type::TypeID::Struct: {
            auto structTy = llvm::dyn_cast<StructType>(type);
            auto llvmStructTy = llvm::StructType::create(C, structTy->getName());
            llvmStructTy->setBody(util::vector::map(structTy->getMembers(), [this](const auto &member) -> llvm::Type* {
                return getLLVMType(member.second);
            }));
            return handle_llvm_type(llvmStructTy);
        }
        
        case Type::TypeID::Tuple: {
            auto tupleTy = llvm::cast<TupleType>(type);
            if (auto ST = tupleTy->getUnderlyingStructType()) {
                return handle_llvm_type(getLLVMType(ST));
            } else {
                return handle_llvm_type(getLLVMType(synthesizeUnderlyingStructTypeForTupleType(tupleTy)));
            }
        }
        
        case Type::TypeID::Function: {
            auto fnTy = llvm::dyn_cast<FunctionType>(type);
            auto paramTypes = util::vector::map(fnTy->getParameterTypes(), [this](auto ty) { return getLLVMType(ty); });
            auto llvmFnTy = llvm::FunctionType::get(getLLVMType(fnTy->getReturnType()), paramTypes, false); // TODO support variadic function types?
            return handle_llvm_type(llvmFnTy->getPointerTo());
        }
        
//        case Type::TypeID::Tuple: {
//            auto TT = llvm::dyn_cast<TupleType>(type);
//            auto memberTypes = util::vector::map(type, TT->getMembers());
//            return handle_llvm_type(llvm::StructType::get(C, memberTypes));
//        }
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
        
        case Type::TypeID::Pointer:
        case Type::TypeID::Reference: {
            Type *pointee = type->isPointerTy()
                ? llvm::dyn_cast<PointerType>(type)->getPointee()
                : llvm::dyn_cast<ReferenceType>(type)->getReferencedType();
            return handle_di_type(builder.createPointerType(getDIType(pointee), pointerWidth));
        }
        
        case Type::TypeID::Numerical: {
            auto numTy = llvm::dyn_cast<NumericalType>(type);
            unsigned int encoding = 0;
            
            if (numTy->isBoolTy())         encoding = llvm::dwarf::DW_ATE_boolean;
            else if (numTy->isFloatTy())   encoding = llvm::dwarf::DW_ATE_float;
            else if (numTy->isIntegerTy()) encoding = numTy->isSigned() ? llvm::dwarf::DW_ATE_signed : llvm::dwarf::DW_ATE_unsigned;
            else LKFatalError("Unhandled type: %s", numTy->str_desc().c_str());
            
            auto ty = builder.createBasicType(numTy->str_desc(), numTy->getPrimitiveSizeInBits(), encoding);
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
        
//        case Type::TypeID::Struct: {
//            auto structTy = llvm::dyn_cast<StructType>(type);
//            auto llvmStructTy = llvm::dyn_cast<llvm::StructType>(getLLVMType(structTy));
//            auto llvmStructTyLayout = DL.getStructLayout(llvmStructTy);
//            auto unit = DIFileForSourceLocation(builder, structTy->getSourceLocation());
//
//            std::vector<llvm::Metadata *> llvmMembers = util::vector::mapi(structTy->getMembers(), [&](auto idx, auto &member) -> llvm::Metadata* {
//                auto llvmMemberTy = getLLVMType(member.second);
//                return  builder.createMemberType(unit, member.first, unit, 0, // TODO struct member line number?
//                                                 DL.getTypeSizeInBits(llvmMemberTy), DL.getPrefTypeAlignment(llvmMemberTy),
//                                                 llvmStructTyLayout->getElementOffsetInBits(idx),
//                                                 llvm::DINode::DIFlags::FlagZero, getDIType(member.second));
//            });
//
//            auto ty = builder.createStructType(unit, structTy->getName(), unit, structTy->getSourceLocation().line,
//                                               DL.getTypeSizeInBits(llvmStructTy), DL.getPrefTypeAlignment(llvmStructTy),
//                                               llvm::DINode::DIFlags::FlagZero, nullptr, builder.getOrCreateArray(llvmMembers));
//            return handle_di_type(ty);
//        }
        
        case Type::TypeID::Struct:
        case Type::TypeID::Tuple: {
            // We know that `getLLVMType` returns a StructType for both structs and tuples
            auto llvmStructTy = llvm::cast<llvm::StructType>(getLLVMType(type));
            auto llvmStructTyLayout = DL.getStructLayout(llvmStructTy);
            llvm::DIScope *scope;
            std::vector<llvm::Metadata *> llvmMembers;
            
            auto registerMember = [&](uint32_t index, std::string name, llvm::Type *llvmTy, llvm::DIType *DIType, llvm::DIFile *file, uint32_t lineNo) {
                auto DIMember = builder.createMemberType(scope, name, file, lineNo,
                                                         DL.getTypeSizeInBits(llvmTy), DL.getPrefTypeAlignment(llvmTy),
                                                         llvmStructTyLayout->getElementOffsetInBits(index),
                                                         llvm::DINode::DIFlags::FlagZero, DIType);
                
                llvmMembers.push_back(DIMember);
            };
            
            if (auto ST = llvm::dyn_cast<StructType>(type)) {
                scope = DIFileForSourceLocation(builder, ST->getSourceLocation());
                for (size_t idx = 0; idx < ST->memberCount(); idx++) {
                    const auto &[name, type] = ST->getMembers()[idx];
                    registerMember(idx, name, getLLVMType(type), getDIType(type), llvm::cast<llvm::DIFile>(scope), 0 /* TODO struct member line number? */);
                }
            } else if (auto TT = llvm::dyn_cast<TupleType>(type)) {
                scope = debugInfo.compileUnit; // TODO  can we do better here?
                for (size_t idx = 0; idx < TT->memberCount(); idx++) {
                    auto memberName = util::fmt::format("{}", idx);
                    auto type = TT->getMembers()[idx];
                    registerMember(idx, memberName, getLLVMType(type), getDIType(type), nullptr, 0);
                }
            } else {
                LKFatalError("unexpected type");
            }
            
            auto ty = builder.createStructType(scope, type->str_desc(), type->isStructTy() ? llvm::cast<llvm::DIFile>(scope) : nullptr,
                                               type->isStructTy() ? llvm::cast<StructType>(type)->getSourceLocation().line : 0,
                                               DL.getTypeSizeInBits(llvmStructTy), DL.getPrefTypeAlignment(llvmStructTy),
                                               llvm::DINode::DIFlags::FlagZero, /*derivedFrom*/ nullptr, builder.getOrCreateArray(llvmMembers));
            return handle_di_type(ty);
        }
    }
    
    LKFatalError("should never reach here");
}




llvm::DISubroutineType* IRGenerator::toDISubroutineType(const ast::FunctionSignature& signature) {
    // Looking at [godbolt]( https://godbolt.org/z/EKfzqi ), it seems like the first element should be the function's return type?
    
    std::vector<llvm::Metadata *>types;
    types.reserve(signature.paramTypes.size() + 1);
    
    types.push_back(resolveTypeDesc(signature.returnType)->getLLVMDIType());
    for (const auto& paramTy : signature.paramTypes) {
        types.push_back(resolveTypeDesc(paramTy)->getLLVMDIType());
    }
    return debugInfo.builder.createSubroutineType(debugInfo.builder.getOrCreateTypeArray(types));
}





bool IRGenerator::valueIsTriviallyConvertible(std::shared_ptr<ast::NumberLiteral> numberExpr, Type *dstTy) {
    // TODO is this function strict enough?
    using NT = ast::NumberLiteral::NumberType;
    
    if (!dstTy->isNumericalTy()) return false; // TODO is this too strict?
    auto dstTyNT = static_cast<NumericalType *>(dstTy);
    
    // Allowed trivial conversions:
    // int literal to any int type (as long as the value fits)
    // int literal to double
    
    
    if (numberExpr->type == NT::Boolean) {
        return dstTyNT == builtinTypes.yo.Bool;
    }
    
    if (dstTyNT == builtinTypes.yo.f64) {
        switch (numberExpr->type) {
            case NT::Double:
                return true;
            case NT::Integer:
                return value_fits_in_type<double>(numberExpr->value);
            case NT::Boolean:
            case NT::Character:
                return false;
        }
    } else if (dstTyNT->isIntegerTy()) {
        switch (numberExpr->type) {
            case NT::Double:
                return false; // TODO is this a good idea?
            case NT::Integer:
                return integerLiteralFitsInIntegralType(numberExpr->value, dstTyNT);
            case NT::Boolean:
                return false; // TODO this should be true, right?
            case NT::Character:
                return true;
        }
    }
    
    LKFatalError("TODO?");
}




Type* IRGenerator::getType(std::shared_ptr<ast::Expr> expr) {
    switch (expr->getKind()) {
        case NK::NumberLiteral: {
            using NT = ast::NumberLiteral::NumberType;
            auto numberLiteral = llvm::dyn_cast<ast::NumberLiteral>(expr);
            switch (numberLiteral->type) {
                case NT::Boolean:   return builtinTypes.yo.Bool;
                case NT::Integer:   return builtinTypes.yo.i64;
                case NT::Character: return builtinTypes.yo.i8; // TODO introduce a dedicated char type?
                case NT::Double:    return builtinTypes.yo.f64;
            }
        }
        
        case NK::StringLiteral: {
            using SLK = ast::StringLiteral::StringLiteralKind;
            switch (static_cast<ast::StringLiteral *>(expr.get())->kind) {
                case SLK::ByteString:
                    return builtinTypes.yo.i8Ptr;
                case SLK::NormalString: {
                    if (auto StringTy = nominalTypes.get(mangling::mangleAsStruct("String"))) {
                        return *StringTy;
                    } else {
                        diagnostics::emitError(expr->getSourceLocation(), "unable to find 'String' type");
                    }
                }
            }
        }
        
        case NK::Ident: {
            auto identExpr = static_cast<ast::Ident *>(expr.get());
            if (auto VB = localScope.get(identExpr->value)) {
                return VB->type;
            } else {
                diagnostics::emitError(identExpr->getSourceLocation(), util::fmt::format("unable to resolve identifier '{}'", identExpr->value));
            }
        }
        
        case NK::ArrayLiteralExpr: {
            auto literal = llvm::dyn_cast<ast::ArrayLiteralExpr>(expr);
            if (literal->elements.empty()) {
                diagnostics::emitError(literal->getSourceLocation(), "unable to deduce type from empty array literal");
            }
            return getType(literal->elements.front());
        }
        
        case NK::CastExpr:
            return resolveTypeDesc(static_cast<ast::CastExpr *>(expr.get())->destType);
        
        case NK::CallExpr:
            return resolveTypeDesc(resolveCall(llvm::dyn_cast<ast::CallExpr>(expr), kSkipCodegen).signature.returnType);
        
        case NK::MatchExpr:
            return getType(static_cast<ast::MatchExpr *>(expr.get())->branches.front().expression); // TODO add a check somewhere to make sure all branches return the same type
        
        case NK::RawLLVMValueExpr:
            return static_cast<ast::RawLLVMValueExpr *>(expr.get())->type;
        
        case NK::SubscriptExpr: {
            auto subscriptExpr = llvm::dyn_cast<ast::SubscriptExpr>(expr);
            auto targetTy = getType(subscriptExpr->target);
            
            // Note: `typeIsSubscriptable` also returns true for pointers, but we have a separate check
            // since pointer subscripts shouldn't get resolved to custom subscript overloads (same for tuples)
            if (targetTy->isPointerTy()) {
                return llvm::dyn_cast<PointerType>(targetTy)->getPointee()->getReferenceTo();
            
            } else if (targetTy->isTupleTy()) {
                auto diag_invalid_offset = [&]() {
                    diagnostics::emitError(subscriptExpr->getSourceLocation(), "tuple subscript offset expression must be an integer literal");
                };
                
                if (auto offsetExprNumLiteral = llvm::dyn_cast<ast::NumberLiteral>(subscriptExpr->offset)) {
                    if (offsetExprNumLiteral->type != ast::NumberLiteral::NumberType::Integer) {
                        diag_invalid_offset();
                    }
                    auto tupleTy = llvm::cast<TupleType>(targetTy);
                    auto index = offsetExprNumLiteral->value;
                    
                    if (auto ty = tupleTy->getTypeOfElementAtIndex(index)) {
                        return ty;
                    } else {
                        auto msg = util::fmt::format("tuple type '{}' has no member at index {}", tupleTy, index);
                        diagnostics::emitError(subscriptExpr->getSourceLocation(), msg);
                    }
                    
                } else {
                    diag_invalid_offset();
                }
            
            } else if (typeIsSubscriptable(targetTy)) {
                auto callExpr = subscriptExprToCall(subscriptExpr);
                return resolveTypeDesc(resolveCall(callExpr, kSkipCodegen).signature.returnType);
            
            } else {
                auto msg = util::fmt::format("type '{}' is not subscriptable", targetTy);
                diagnostics::emitError(subscriptExpr->getSourceLocation(), msg);
            }
        }
        
        // <target>.<member>
        case NK::MemberExpr: {
            auto memberExpr = static_cast<ast::MemberExpr *>(expr.get());
            auto targetTy = getType(memberExpr->target);
            
            Type *underlyingTy = nullptr;
            
            switch (targetTy->getTypeId()) {
                case Type::TypeID::Struct:
                    underlyingTy = static_cast<StructType *>(targetTy);
                    break;
                
                case Type::TypeID::Reference:
                    underlyingTy = static_cast<ReferenceType *>(targetTy)->getReferencedType();
                    break;
                
                case Type::TypeID::Pointer:
                    underlyingTy = static_cast<PointerType *>(targetTy)->getPointee();
                    break;
                
                default: LKFatalError("OOF");
            }
            
            if (auto structTy = llvm::dyn_cast<StructType>(underlyingTy)) {
                if (auto memberTy = structTy->getMember(memberExpr->memberName).second) {
                    return memberTy;
                } else {
                    auto msg = util::fmt::format("type '{}' does not have a member named '{}'", structTy->getName(), memberExpr->memberName);
                    diagnostics::emitError(memberExpr->getSourceLocation(), msg);
                }
            } else {
                auto msg = util::fmt::format("member expr target type '{}' not a struct", underlyingTy->str_desc());
                diagnostics::emitError(memberExpr->getSourceLocation(), msg);
            }
        }
        
        case NK::UnaryExpr: {
            using Op = ast::UnaryExpr::Operation;
            
            auto unaryExpr = llvm::dyn_cast<ast::UnaryExpr>(expr);
            switch (unaryExpr->op) {
                case Op::LogicalNegation:
                    return builtinTypes.yo.Bool;
                
                case Op::Negate:
                case Op::BitwiseNot:
                    return getType(unaryExpr->expr);
                
                case Op::AddressOf: {
                    auto targetTy = getType(unaryExpr->expr);
                    if (auto refTy = llvm::dyn_cast<ReferenceType>(targetTy)) {
                        return refTy->getReferencedType()->getPointerTo();
                    } else {
                        return targetTy->getPointerTo();
                    }
                }
            }
        }
        
        case NK::CompOp:
            return builtinTypes.yo.Bool;
        
        case NK::BinOp: {
            auto binopExpr = static_cast<ast::BinOp *>(expr.get());
            auto mangledCanonicalName = mangling::mangleCanonicalName(binopExpr->getOperator());
            // TODO don't allocate an object for every check!
            auto tempCallExpr = std::make_shared<ast::CallExpr>(makeIdent(mangledCanonicalName),
                                                                std::vector<std::shared_ptr<ast::Expr>>{ binopExpr->getLhs(), binopExpr->getRhs() });
            tempCallExpr->setSourceLocation(binopExpr->getSourceLocation());
            return resolveTypeDesc(resolveCall(tempCallExpr, kSkipCodegen).signature.returnType);
        }
        
        case NK::LambdaExpr: {
            auto lambdaExpr = llvm::dyn_cast<ast::LambdaExpr>(expr);
            return synthesizeLambdaExpr(lambdaExpr);
        }
        
        case NK::TupleExpr: {
            auto tupleExpr = llvm::dyn_cast<ast::TupleExpr>(expr);
            auto elementTys = util::vector::map(tupleExpr->elements, [&](auto E) { return getType(E); });
            return TupleType::get(elementTys);
        }
        
        default:
            unhandled_node(expr);
            LKFatalError("TODO");
    }
}




// Whether the expression evaluates to a temporary value. ??This implies that `expr` cannot become an lvalue (except for references, of course)??
bool IRGenerator::isTemporary(std::shared_ptr<ast::Expr> expr) {
    // TODO this seems way too simple?
    // TODO what about ast::RawLLVMValueExpr?
    auto ty = getType(expr);
    if (ty->isReferenceTy()) return false;
    
    switch (expr->getKind()) {
        case NK::Ident:
        case NK::MemberExpr:
        case NK::SubscriptExpr:
            return false;
        
        default:
            return true;
    }
}




bool IRGenerator::typeIsConstructible(Type *ty) {
    return implementsInstanceMethod(ty, kInitializerMethodName);
}

bool IRGenerator::typeIsDestructible(Type *ty) {
    return implementsInstanceMethod(ty, kSynthesizedDeallocMethodName);
}

bool IRGenerator::typeIsSubscriptable(Type *ty) {
    return ty->isPointerTy() || ty->isTupleTy() || implementsInstanceMethod(ty, mangling::encodeOperator(ast::Operator::Subscript));
}


bool IRGenerator::implementsInstanceMethod(Type *ty, std::string_view methodName) {
    if (auto ST = getUnderlyingStruct(ty)) {
        auto canonicalName = mangling::mangleCanonicalName(ST->getName(), methodName, ast::FunctionKind::InstanceMethod);
        return util::map::has_key(functions, canonicalName);
    } else {
        return false;
    }
}






StructType* IRGenerator::instantiateTemplateStruct(std::shared_ptr<ast::StructDecl> SD, std::shared_ptr<ast::TypeDesc> TD) {
    LKAssert(SD->isTemplateDecl() && TD->isOfKind(ast::TypeDesc::Kind::NominalTemplated));
    LKAssert(SD->getName() == TD->getName());
    
    // Build a mapping for all explicitly specified types and forward that to the actual implementation
    
    TemplateTypeMapping mapping;
    auto &explicitArgs = TD->getTemplateArgs();
    
    for (uint64_t idx = 0; idx < SD->templateParamsDecl->size(); idx++) {
        if (idx < explicitArgs.size()) {
            mapping[SD->templateParamsDecl->getParams()[idx].name->value] = explicitArgs[idx];
        }
    }
    
    return instantiateTemplateStruct(SD, mapping);
}



// TODO add a SourceLocation parameter?
StructType* IRGenerator::instantiateTemplateStruct(std::shared_ptr<ast::StructDecl> SD, const TemplateTypeMapping &mapping) {
    LKAssert(SD->isTemplateDecl());
    
    TemplateTypeMapping fullMapping;
    
    for (uint64_t idx = 0; idx < SD->templateParamsDecl->size(); idx++) {
        Type *ty = nullptr;
        auto &param = SD->templateParamsDecl->getParams()[idx];
        
        if (auto TD = util::map::get_opt(mapping, param.name->value)) {
            ty = resolveTypeDesc(*TD);
        } else if (auto defaultTD = param.defaultType) {
            ty = resolveTypeDesc(defaultTD);
        } else {
            diagnostics::emitError(util::fmt::format("unable to resolve template parameter {}", param.name));
        }
        
        fullMapping[param.name->value] = ast::TypeDesc::makeResolved(ty);
    }
    
    auto specializedDecl = TemplateSpecializer::specializeWithMapping(SD, fullMapping);
    
    for (uint64_t idx = 0; idx < SD->templateParamsDecl->size(); idx++) {
        auto &name = SD->templateParamsDecl->getParams()[idx].name;
        specializedDecl->resolvedTemplateArgTypes.push_back(fullMapping.at(name->value)->getResolvedType());
    }
    
    auto mangledName = mangling::mangleFullyResolved(specializedDecl);
    
    bool isTemporary = util::map::contains_where(fullMapping, [](const std::string &name, const std::shared_ptr<ast::TypeDesc> &TD) -> bool {
        return TD->getResolvedType() && TD->getResolvedType()->hasFlag(Type::Flags::IsTemporary);
    });
    if (isTemporary) {
        return Type::createTemporary(mangledName, specializedDecl->resolvedTemplateArgTypes);
    }
    
    
    if (auto ST = nominalTypes.get(mangledName)) {
        return llvm::dyn_cast<StructType>(*ST);
    }
    
    StructType *structTy = registerStructDecl(specializedDecl);
    
    // Instantiate & codegen impl blocks
    for (auto &implBlock : specializedDecl->implBlocks) {
        for (auto &FD : implBlock->methods) {
            if (!(FD->getAttributes().int_isSynthesized || FD->getName() == kInitializerMethodName)) {
                FD->getAttributes().int_isDelayed = true;
            }
        }
        
        auto TD = implBlock->typeDesc;
        implBlock->typeDesc = ast::TypeDesc::makeNominal(mangledName);
        registerImplBlock(implBlock);
        implBlock->typeDesc = TD;
        
    }
    
    for (auto &implBlock : specializedDecl->implBlocks) {
        for (auto &FD : implBlock->methods) {
            if (!FD->getAttributes().int_isDelayed) {
                if (auto F = module->getFunction(mangleFullyResolved(FD)); F && !F->empty()) {
                    // It can happen that a function w/ delayed codegen is invoked by a function w/out delayed codegen,
                    // in which case we have to avoid running codegen twice (ie int_isDelayed being false in here is not
                    // indicative of the function actually needing codegen)
                    continue;
                }
                codegen(FD);
            }
        }
    }
    
    return structTy;
}






#pragma mark - Synthesized Functions

// TODO for all 3 functions below, the kSkipCodegen option is implemented really bad, mainly since we still create the ast, only to then not run kt through codegen
// TODO there's a lot of duplicate code in the functions below!


llvm::Value* IRGenerator::synthesizeDefaultMemberwiseInitializer(std::shared_ptr<ast::StructDecl> structDecl, SkipCodegenOption codegenOption) {
    // TODO set the source location for all nodes generated in here!
        
    auto &SL = structDecl->getSourceLocation();
    auto ST = llvm::dyn_cast<StructType>(*nominalTypes.get(mangling::mangleFullyResolved(structDecl)));
    auto &SM = ST->getMembers();
    
    auto selfIdent = makeIdent("self", SL);
    
    ast::FunctionSignature sig;
    std::vector<std::shared_ptr<ast::Ident>> paramNames;
    attributes::FunctionAttributes attr;
    
    attr.no_debug_info = structDecl->attributes.no_debug_info;
    attr.int_isFwdDecl = true;
    attr.int_isSynthesized = true;
    sig.setSourceLocation(SL);
    sig.returnType = ast::TypeDesc::makeResolved(builtinTypes.yo.Void);
    sig.paramTypes.reserve(ST->memberCount() + 1);
    sig.paramTypes.push_back(ast::TypeDesc::makeResolved(ST->getReferenceTo()));
    
    paramNames.reserve(ST->memberCount() + 1);
    paramNames.push_back(selfIdent);
    
    util::vector::iteri(SM, [&](uint64_t idx, const std::pair<std::string, Type *> &elem) -> void {
        sig.paramTypes.push_back(ast::TypeDesc::makeResolved(elem.second, SL));
        paramNames.push_back(makeIdent(util::fmt::format("__arg{}", idx), SL));
    });
    
    auto FD = std::make_shared<ast::FunctionDecl>(ast::FunctionKind::InstanceMethod, kInitializerMethodName, sig, attr);
    FD->setParamNames(paramNames);
    FD->setSourceLocation(SL);
    FD->setImplType(ST);
    
    auto mangledName = mangleFullyResolved(FD);
//    if (auto F = module->getFunction(mangledName)) {
//        // Only generate an initializer if there is no existing overload
//        return F;
//    }
    
    if (auto F = getResolvedFunctionWithName(mangledName)) {
        //auto &F = resolvedFunctions.at(mangledName);
        if (!F->funcDecl->getAttributes().int_isFwdDecl) return F->llvmValue;
//        else F->funcDecl->getAttributes().int_isFwdDecl = false;
    }
    
    auto body = std::make_shared<ast::CompoundStmt>();
    body->setSourceLocation(SL);
    
    for (uint64_t idx = 0; idx < SM.size(); idx++) {
        auto target = std::make_shared<ast::MemberExpr>(selfIdent, SM.at(idx).first);
        target->setSourceLocation(SL);
        auto A = std::make_shared<ast::Assignment>(target, paramNames.at(idx + 1));
        A->shouldDestructOldValue = false;
        A->overwriteReferences = true;
        A->setSourceLocation(SL);
        body->statements.push_back(A);
    }
    
    FD->setBody(body);
    
    registerFunction(FD);
    
    if (codegenOption == kSkipCodegen) return nullptr;
    else return codegen(FD);
}



llvm::Value* IRGenerator::synthesizeDefaultCopyConstructor(std::shared_ptr<ast::StructDecl> structDecl, SkipCodegenOption codegenOption) {
    auto &SL = structDecl->getSourceLocation();
    auto ST = llvm::dyn_cast<StructType>(*nominalTypes.get(mangling::mangleFullyResolved(structDecl)));
    auto &SM = ST->getMembers();
    
    ast::FunctionSignature sig;
    std::vector<std::shared_ptr<ast::Ident>> paramNames;
    attributes::FunctionAttributes attr;
    
    attr.no_debug_info = structDecl->attributes.no_debug_info;
    attr.int_isFwdDecl = true;
    attr.int_isSynthesized = true;
    sig.setSourceLocation(SL);
    sig.returnType = ast::TypeDesc::makeResolved(builtinTypes.yo.Void);
    sig.paramTypes.push_back(ast::TypeDesc::makeResolved(ST->getReferenceTo()));
    sig.paramTypes.push_back(ast::TypeDesc::makeResolved(ST->getReferenceTo()));
    
    auto selfIdent = makeIdent("self", SL);
    auto arg0Ident = makeIdent("__arg0", SL);
    
    paramNames.push_back(selfIdent);
    paramNames.push_back(arg0Ident);
    
    auto FD = std::make_shared<ast::FunctionDecl>(ast::FunctionKind::InstanceMethod, kInitializerMethodName, sig, attr);
    FD->setParamNames(paramNames);
    FD->setSourceLocation(SL);
    FD->setImplType(ST);
    
    auto mangledName = mangleFullyResolved(FD);
//    if (auto F = module->getFunction(mangledName)) {
//        // type already has a user-defined copy constructor
//        return F;
//    }
    
    if (auto F = getResolvedFunctionWithName(mangledName)) {
        //auto &F = resolvedFunctions.at(mangledName);
        if (!F->funcDecl->getAttributes().int_isFwdDecl) return F->llvmValue;
//        else F->funcDecl->getAttributes().int_isFwdDecl = false;
    }
    
    auto body = std::make_shared<ast::CompoundStmt>();
    body->setSourceLocation(SL);
    
    for (uint64_t idx = 0; idx < SM.size(); idx++) {
        auto &memberName = SM.at(idx).first;
        auto lhs = std::make_shared<ast::MemberExpr>(selfIdent, memberName);
        lhs->setSourceLocation(SL);
        auto rhs = std::make_shared<ast::MemberExpr>(arg0Ident, memberName);
        rhs->setSourceLocation(SL);
        auto ass = std::make_shared<ast::Assignment>(lhs, rhs);
        ass->setSourceLocation(SL);
        ass->shouldDestructOldValue = false;
        ass->overwriteReferences = true;
        body->statements.push_back(ass);
    }
    
    FD->setBody(body);
    
    registerFunction(FD);
    
    if (codegenOption == kSkipCodegen) return nullptr;
    else return codegen(FD);
}



llvm::Value* IRGenerator::synthesizeDefaultDeallocMethod(std::shared_ptr<ast::StructDecl> structDecl, SkipCodegenOption codegenOption) {
    auto &SL = structDecl->getSourceLocation();
    auto ST = llvm::dyn_cast<StructType>(*nominalTypes.get(mangling::mangleFullyResolved(structDecl)));
    auto &SM = ST->getMembers();
    
    auto selfIdent = makeIdent("self", SL);
    
    ast::FunctionSignature sig;
    attributes::FunctionAttributes attr;
    std::vector<std::shared_ptr<ast::Ident>> paramNames = { selfIdent };
    
    attr.no_debug_info = structDecl->attributes.no_debug_info;
    attr.int_isFwdDecl = true;
    attr.int_isSynthesized = true;
    sig.returnType = ast::TypeDesc::makeResolved(builtinTypes.yo.Void);
    sig.paramTypes = { ast::TypeDesc::makeResolved(ST->getReferenceTo()) };
    
    auto FD = std::make_shared<ast::FunctionDecl>(ast::FunctionKind::InstanceMethod, kSynthesizedDeallocMethodName, sig, attr);
    FD->setParamNames(paramNames);
    FD->setSourceLocation(SL);
    FD->setImplType(ST);
    
    auto body = std::make_shared<ast::CompoundStmt>();
    body->setSourceLocation(SL);
    
    // Invoke custom destructor if defined
    if (auto CN = mangling::mangleCanonicalName(ST->getName(), "dealloc", ast::FunctionKind::InstanceMethod); util::map::has_key(functions, CN)) {
        auto callExpr = std::make_shared<ast::CallExpr>(makeIdent(CN));
        callExpr->arguments = { selfIdent };
        callExpr->setSourceLocation(SL);
        auto stmt = std::make_shared<ast::ExprStmt>(callExpr);
        stmt->setSourceLocation(SL);
        body->statements.push_back(stmt);
    }
    
    auto prevDecl = currentFunction.decl;
    currentFunction.decl = FD;
    
    // Destruct all members
    for (const auto &[name, type] : SM) {
        auto memberAccess = std::make_shared<ast::MemberExpr>(selfIdent, name);
        if (auto stmt = createDestructStmtIfDefined(type, memberAccess, /*includeReferences*/ false)) {
            body->statements.push_back(stmt);
        }
    }
    
    currentFunction.decl = prevDecl;
    
    FD->setBody(body);
    
    registerFunction(FD);
    
    if (codegenOption == kSkipCodegen) return nullptr;
    else return codegen(FD);
}
