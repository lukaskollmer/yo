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
#include "ASTVisitor.h"

#include <optional>
#include <limits>
#include <set>


using namespace yo;
using namespace yo::irgen;


using NK = ast::Node::Kind;


#define unhandled_node(node) \
{ std::cout << __PRETTY_FUNCTION__ << ": Unhandled Node: " << util::typeinfo::getTypename(*(node)) << std::endl; \
throw; }




uint8_t irgen::argumentOffsetForFunctionKind(ast::FunctionKind kind) {
    switch (kind) {
        case yo::ast::FunctionKind::GlobalFunction:
        case yo::ast::FunctionKind::StaticMethod:
        case yo::ast::FunctionKind::OperatorOverload: // TODO what about instance methods that are operator overloads?
            return 0;
        case yo::ast::FunctionKind::InstanceMethod:
            return kInstanceMethodCallArgumentOffset;
    }
}


std::shared_ptr<ast::Ident> irgen::makeIdent(const std::string& str, lex::SourceLocation SL) {
    auto ident = std::make_shared<ast::Ident>(str);
    ident->setSourceLocation(SL);
    return ident;
}

std::shared_ptr<ast::Ident> irgen::formatTupleMemberAtIndex(size_t index) {
    return makeIdent(util::fmt::format("__{}", index));
}


std::string irgen::mangleFullyResolved(const std::shared_ptr<ast::FunctionDecl>& functionDecl) {
    if (functionDecl->getAttributes().no_mangle) {
        return functionDecl->getName();
    } else if (!functionDecl->getAttributes().mangledName.empty()) {
        return functionDecl->getAttributes().mangledName;
    }
    return mangling::mangleFullyResolved(functionDecl);
}



// TODO is this a good idea?
// Note: this assumes that the type has already been fully initialized (important for tuples and lambdas)
StructType* irgen::getUnderlyingStruct(Type *ty) {
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


template <typename Dst, typename Src>
bool value_fits_in_type(Src value) {
    // TODO this seems way too easy?
    return static_cast<Src>(static_cast<Dst>(value)) == value;
}


bool irgen::integerLiteralFitsInIntegralType(uint64_t value, Type *type) {
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


std::shared_ptr<ast::CallExpr> irgen::subscriptExprToCall(std::shared_ptr<ast::SubscriptExpr> subscriptExpr) {
    auto callTarget = std::make_shared<ast::MemberExpr>(subscriptExpr->target, mangling::encodeOperator(ast::Operator::Subscript));
    callTarget->setSourceLocation(subscriptExpr->target->getSourceLocation());
    
    auto callExpr = std::make_shared<ast::CallExpr>(callTarget);
    callExpr->setSourceLocation(subscriptExpr->getSourceLocation());
    callExpr->arguments = { subscriptExpr->offset };
    
    return callExpr;
}


llvm::DIFile* irgen::DIFileForSourceLocation(llvm::DIBuilder& builder, const lex::SourceLocation& loc) {
    const auto [directory, filename] = util::string::extractPathAndFilename(loc.filepath);
    return builder.createFile(filename, directory);
}







// IRGenerator

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




void IRGenerator::codegen(const ast::AST &ast) {
    preflight(ast);
    
    for (const auto &node : ast) {
        codegenTLS(node);
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





void IRGenerator::preflight(const ast::AST &inputAST) {
    
    ast::AST ast;
    ast.reserve(inputAST.size());
    
    std::map<std::string, std::shared_ptr<ast::StructDecl>> structDecls;
    for (auto &node : inputAST) {
        if (auto structDecl = llvm::dyn_cast<ast::StructDecl>(node)) {
            LKAssert(!util::map::has_key(structDecls, structDecl->name));
            structDecls[structDecl->name] = structDecl;
        }
    }
    
    for (auto &node : inputAST) {
        if (auto implBlock = llvm::dyn_cast<ast::ImplBlock>(node)) {
            LKAssert(implBlock->typeDesc->isNominal());
            LKAssert(util::map::has_key(structDecls, implBlock->typeDesc->getName()));
            auto &SD = structDecls[implBlock->typeDesc->getName()];
            for (auto FD : implBlock->methods) {
                FD->implTypeDesc = implBlock->typeDesc;
                SD->methods.push_back(FD);
                ast.push_back(FD);
            }
        } else {
            ast.push_back(node);
        }
    }
    
    
//    auto dbg_ast = [&]() {
//        for (const auto &node : ast) {
//            util::fmt::print("{}", node_summary(node));
//        }
//    };
    
//    puts("\n\n");
//    dbg_ast();
//    puts("\n\n");
//    std::stable_sort(ast.begin(), ast.end(), [](auto lhs, auto rhs) { return isDependentOn(rhs, lhs); });
//    puts("\n\n");
//    dbg_ast();
//    puts("\n\n");
    
//    if (!sema::resolveTopLevelDependencies(ast)) {
//        LKAssert("sema AST restructure failed");
//    }
    
    for (auto &node : ast) {
        if (auto typealiasDecl = llvm::dyn_cast<ast::TypealiasDecl>(node)) {
            registerTypealias(typealiasDecl);
        }
    }
    
    for (auto &node : ast) {
        switch (node->getKind()) {
            case NK::StructDecl:
                registerStructDecl(llvm::cast<ast::StructDecl>(node));
                break;
            
            case NK::FunctionDecl:
                registerFunction(llvm::cast<ast::FunctionDecl>(node));
                break;
            
            case NK::TypealiasDecl:
                //registerTypealias(llvm::cast<ast::TypealiasDecl>(node));
                break;
            
            default:
                LKFatalError("?");
        }
    }
    
    //LKFatalError("TODO");
}
















void IRGenerator::registerTypealias(std::shared_ptr<ast::TypealiasDecl> typealiasDecl) {
    nominalTypes.insert(typealiasDecl->typename_, resolveTypeDesc(typealiasDecl->type));
}







// TODO why does this function exist?
std::optional<ResolvedCallable> IRGenerator::getResolvedFunctionWithName(const std::string &name) {
    return util::map::get_opt(resolvedFunctions, name);
}


// TODO come up w/ a better implementation than this!
StructType* UselessStructTypeForTemplateStructCtor(std::string name, const lex::SourceLocation &SL) {
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




ast::FunctionKind getFunctionKind(const ast::FunctionDecl &FD) {
    auto &sig = FD.getSignature();
    if (sig.paramTypes.empty()) {
        LKAssert(!FD.isOperatorOverload()); // TODO would be cool to support this
        return ast::FunctionKind::StaticMethod;
    }
    if (FD.getParamNames()[0]->value != "self") {
        return ast::FunctionKind::StaticMethod;
    }
    auto T = sig.paramTypes[0];
    if (T->isReference() && T->getPointee()->isNominal() && T->getPointee()->getName() == "Self") {
        return ast::FunctionKind::InstanceMethod;
    }
    return ast::FunctionKind::StaticMethod;
};


void assertIsValidMemberFunction(const ast::FunctionDecl &FD, std::shared_ptr<ast::TemplateParamDeclList> implTypeTemplateParams = nullptr) {
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
}



// Note: this only registers the struct decl, it does not register the type's methods
StructType* IRGenerator::registerStructDecl(std::shared_ptr<ast::StructDecl> structDecl) {
    auto structName = structDecl->name;
    
    if (structDecl->attributes.trivial && structDecl->isTemplateDecl()) {
        diagnostics::emitError(structDecl->getSourceLocation(), "trivial struct cannot be a template");
        // TODO implement the other checks!!!
    }
    
    if (structDecl->isTemplateDecl()) {
        LKAssert(!structDecl->attributes.no_init && "struct template cannot have no_init attribute");
        
        ensureTemplateParametersAreDistinct(*structDecl->templateParamsDecl);
        for (auto FD : structDecl->methods) {
            assertIsValidMemberFunction(*FD, structDecl->templateParamsDecl);
        }
        
        structTemplateDecls[structDecl->name] = structDecl;
        
        ast::FunctionSignature ctorSig;
        ctorSig.returnType = ast::TypeDesc::makeNominalTemplated(structName, util::vector::map(structDecl->templateParamsDecl->getParams(), [](auto &param) {
            return ast::TypeDesc::makeNominal(param.name->value);
        }));
        ctorSig.templateParamsDecl = std::make_shared<ast::TemplateParamDeclList>(*structDecl->templateParamsDecl);
        ctorSig.setSourceLocation(structDecl->getSourceLocation());

        auto ctorFnDecl = std::make_shared<ast::FunctionDecl>(ast::FunctionKind::StaticMethod,
                                                              structName, ctorSig,
                                                              attributes::FunctionAttributes());
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
    
    
    
    const auto &structAttrs = structDecl->attributes;
    
    for (auto &fn : structDecl->methods) {
        assertIsValidMemberFunction(*fn);
        auto functionKind = getFunctionKind(*fn);
        if (functionKind == ast::FunctionKind::InstanceMethod) {
            fn->getSignature().paramTypes[0] = ast::TypeDesc::makeResolved(structTy->getReferenceTo());
        }
        fn->getAttributes().no_debug_info = structAttrs.no_debug_info;
        fn->setFunctionKind(functionKind);
        fn->setImplType(structTy);
    }
    
    return structTy;
}



//void IRGenerator::registerImplBlock(std::shared_ptr<ast::ImplBlock> implBlock) {
//    return;
//
//
//    using ast::FunctionKind;
//
//    auto getFunctionKind = [](const ast::FunctionDecl &FD) -> FunctionKind {
//        auto &sig = FD.getSignature();
//        if (sig.paramTypes.empty()) {
//            LKAssert(!FD.isOperatorOverload()); // TODO would be cool to support this
//            return FunctionKind::StaticMethod;
//        }
//        if (FD.getParamNames()[0]->value != "self") {
//            return FunctionKind::StaticMethod;
//        }
//        auto T = sig.paramTypes[0];
//        if (T->isReference() && T->getPointee()->isNominal() && T->getPointee()->getName() == "Self") {
//            return FunctionKind::InstanceMethod;
//        }
//        return FunctionKind::StaticMethod;
//    };
//
//
//    auto assertIsValidMemberFunction = [](const ast::FunctionDecl &FD, std::shared_ptr<ast::TemplateParamDeclList> implTypeTemplateParams = nullptr) {
//        auto &sig = FD.getSignature();
//        auto &attr = FD.getAttributes();
//
//        if (attr.no_mangle) {
//            diagnostics::emitError(FD.getSourceLocation(), "type member function cannot have 'no_mangle' attribute");
//        }
//
//        if (!attr.mangledName.empty()) {
//            diagnostics::emitError(FD.getSourceLocation(), "type member function cannot have explicitly set mangled name");
//        }
//
//        if (sig.isTemplateDecl()) {
//            ensureTemplateParametersAreDistinct(*sig.templateParamsDecl);
//            if (implTypeTemplateParams) {
//                ensureTemplateParametersDontShadow({implTypeTemplateParams, sig.templateParamsDecl});
//            }
//        }
//    };
//
//
//
//    auto typeDesc = implBlock->typeDesc;
//    if (auto templateDecl = util::map::get_opt(structTemplateDecls, typeDesc->getName())) {
//        implBlock->isNominalTemplateType = true;
//        // impl blocks for templated types are registered when their respective type is instantiated
//        // TODO static methods of struct templates should be registered anyways!!!!
//
//        for (auto &FD : implBlock->methods) {
//            assertIsValidMemberFunction(*FD, templateDecl.value()->templateParamsDecl);
//
//            // TODO what about registering static method in here, so that we don't have to instantiate the type just to resolve them?
//        }
//
//        return;
//    }
//
//
//    auto implTy = resolveTypeDesc(implBlock->typeDesc);
//    auto structTy = llvm::cast<StructType>(implTy);
//
//    const auto &structAttrs = structDecls.at(structTy->getName())->attributes;
//
//    for (auto &fn : implBlock->methods) {
//        assertIsValidMemberFunction(*fn);
//        auto functionKind = getFunctionKind(*fn);
//        if (functionKind == FunctionKind::InstanceMethod) {
//            fn->getSignature().paramTypes[0] = ast::TypeDesc::makeResolved(structTy->getReferenceTo());
//        }
//        fn->getAttributes().no_debug_info = structAttrs.no_debug_info;
//        fn->setFunctionKind(functionKind);
//        fn->setImplType(structTy);
//        registerFunction(fn);
//    }
//}





#pragma mark - Codegen



//#define CASE(node, kind, ...) case NK::kind: return codegen(llvm::dyn_cast<ast::kind>(node), ## __VA_ARGS__);

#define CASE(N, T) case NK::T: return codegen##T(llvm::cast<ast::T>(N));

llvm::Value *IRGenerator::codegenTLS(std::shared_ptr<ast::TopLevelStmt> TLS) {
    switch (TLS->getKind()) {
        //case NK::StructDecl:
          //  return codegenStructDecl(llvm::cast<ast::StructDecl>(node));
        
        //case NK::FunctionDecl:
          //  return codegenFunctionDecl(llvm::cast<ast::FunctionDecl>(node));
        CASE(TLS, StructDecl)
        CASE(TLS, FunctionDecl)
        
        case NK::ImplBlock:
        case NK::TypealiasDecl:
            return nullptr;
        
        default:
            unhandled_node(TLS);
    }
}


llvm::Value *IRGenerator::codegenLocalStmt(std::shared_ptr<ast::LocalStmt> localStmt) {
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
        default:
            unhandled_node(localStmt);
    }
}

#undef CASE


llvm::Value *IRGenerator::codegenExpr(std::shared_ptr<ast::Expr> expr, ValueKind VK, bool insertImplicitLoadInst) {
#define CASE(T) case NK::T: V = codegen##T(llvm::cast<ast::T>(expr), VK); break;
    
    llvm::Value *V = nullptr;
    
    switch (expr->getKind()) {
        CASE(NumberLiteral)
        CASE(Ident)
        CASE(CastExpr)
        CASE(StringLiteral)
        CASE(UnaryExpr)
        CASE(MatchExpr)
        CASE(RawLLVMValueExpr)
        CASE(MemberExpr)
        CASE(SubscriptExpr)
        CASE(CallExpr)
        CASE(BinOp)
        CASE(LambdaExpr)
        CASE(ArrayLiteralExpr)
        CASE(TupleExpr)
        default:
            unhandled_node(expr)
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
    if (auto SD = util::map::get_opt(structTemplateDecls, structTy->getName())) {
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
    
    codegenExpr(callExpr);
    
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
        return codegenExpr(expr);
    }
}


llvm::Value* IRGenerator::destructValueIfNecessary(Type *type, llvm::Value *value, bool includeReferences) {
    LKAssert(value->getType()->isPointerTy());
    auto expr = std::make_shared<ast::RawLLVMValueExpr>(value, type->isReferenceTy() ? type : type->getReferenceTo());
    if (auto destructStmt = createDestructStmtIfDefined(type, expr, includeReferences)) {
        return codegenLocalStmt(destructStmt);
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
            auto structDecl = util::map::get_opt(structTemplateDecls, typeDesc->getName());
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
    
    if (lhs.numberOfParameters() != rhs.numberOfParameters()) return false;
    
    for (size_t i = 0; i < lhs.numberOfParameters(); i++) {
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
    types.reserve(signature.numberOfParameters() + 1);
    
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




bool IRGenerator::applyImplicitConversionIfNecessary(std::shared_ptr<ast::Expr> &expr, Type *dstTy) {
    auto srcTy = getType(expr);
    
    if (srcTy == dstTy) {
        return true;
    }
    
    auto wrapInStaticCast = [](std::shared_ptr<ast::Expr> expr, Type *type) {
        auto loc = expr->getSourceLocation();
        auto ret = std::make_shared<ast::CastExpr>(expr, ast::TypeDesc::makeResolved(type), ast::CastExpr::CastKind::StaticCast);
        ret->setSourceLocation(loc);
        return ret;
    };
    
    if (auto literal = llvm::dyn_cast<ast::NumberLiteral>(expr)) {
        if (!dstTy->isNumericalTy()) {
            return false;
        }
        if (valueIsTriviallyConvertible(literal, dstTy)) {
            expr = wrapInStaticCast(expr, dstTy);
            return true;
        }
    }
    
    if (auto *srcNumTy = llvm::dyn_cast<NumericalType>(srcTy), *dstNumTy = llvm::dyn_cast<NumericalType>(dstTy); srcNumTy && dstNumTy) {
        if (srcNumTy->isSigned() && dstNumTy->isUnsigned()) {
            return false;
        }
        if (srcNumTy->getSize() < dstNumTy->getSize()) {
            expr = wrapInStaticCast(expr, dstTy);
            return true;
        }
    }
    
    return false;
}






//bool IRGenerator::typecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *expr, Type *expectedType, Type **initialTypeOfExpr) {
//    auto type = getType(*expr);
//    util::fmt::print("[typecheck&cast] <expr> of '{}' to '{}'", type, expectedType);
//
//    if (initialTypeOfExpr) {
//        *initialTypeOfExpr = type;
//    }
//
//    if (type == expectedType) {
//        return true; // No conversion needed
//    }
//
//    if (expectedType->isReferenceTy()) {
//        // types don't match and we're trying to get a reference out of this
//        return false;
//    }
//}










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
    
    for (auto FD : specializedDecl->methods) {
        if (!(FD->getAttributes().int_isSynthesized || FD->getName() == kInitializerMethodName)) {
            FD->getAttributes().int_isDelayed = true;
        }
    }
    
    
    StructType *structTy = registerStructDecl(specializedDecl);
    
    for (auto funcDecl : specializedDecl->methods) {
        if (!(funcDecl->getAttributes().int_isSynthesized || funcDecl->getName() == kInitializerMethodName)) {
            funcDecl->getAttributes().int_isDelayed = true;
        }
        registerFunction(funcDecl);
    }
    
    for (auto funcDecl : specializedDecl->methods) {
        if (!funcDecl->getAttributes().int_isDelayed) {
            if (auto F = module->getFunction(mangleFullyResolved(funcDecl)); F && !F->empty()) {
                // It can happen that a function w/ delayed codegen is invoked by a function w/out delayed codegen,
                // in which case we have to avoid running codegen twice (ie int_isDelayed being false in here is not
                // indicative of the function actually needing codegen)
                continue; // TODO is this still relevant?
            }
            codegenFunctionDecl(funcDecl);
        }
    }
    
//    // Instantiate & codegen impl blocks
//    for (auto &implBlock : specializedDecl->implBlocks) {
//        for (auto &FD : implBlock->methods) {
//            if (!(FD->getAttributes().int_isSynthesized || FD->getName() == kInitializerMethodName)) {
//                FD->getAttributes().int_isDelayed = true;
//            }
//        }
//
//        auto TD = implBlock->typeDesc;
//        implBlock->typeDesc = ast::TypeDesc::makeNominal(mangledName);
//        registerImplBlock(implBlock);
//        implBlock->typeDesc = TD;
//
//    }
//
//    for (auto &implBlock : specializedDecl->implBlocks) {
//        for (auto &FD : implBlock->methods) {
//            if (!FD->getAttributes().int_isDelayed) {
//                if (auto F = module->getFunction(mangleFullyResolved(FD)); F && !F->empty()) {
//                    // It can happen that a function w/ delayed codegen is invoked by a function w/out delayed codegen,
//                    // in which case we have to avoid running codegen twice (ie int_isDelayed being false in here is not
//                    // indicative of the function actually needing codegen)
//                    continue;
//                }
//                codegen(FD);
//            }
//        }
//    }
    
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
    else return codegenFunctionDecl(FD);
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
    else return codegenFunctionDecl(FD);
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
    else return codegenFunctionDecl(FD);
}
