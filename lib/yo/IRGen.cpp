//
//  IRGen.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "IRGen.h"

#include "Mangling.h"
#include "ASTRewriter.h"
#include "lex/Diagnostics.h"
#include "parse/Attributes.h"
#include "util_llvm.h"
#include "util/llvm_casting.h"
#include "util/MapUtils.h"

#include <optional>
#include <limits>
#include <set>


using namespace yo;
using namespace yo::irgen;


using NK = ast::Node::Kind;



#define unhandled_node(node) \
LKFatalError("Unhandled Node: '%s'", ast::nodeKindToString(node->getKind()).c_str());


// just to make sure the additions to llvm's casting APIs are picked up correctly
static_assert(std::is_same_v<decltype(llvm::cast<yo::ast::Expr>(std::declval<std::shared_ptr<yo::ast::Node>&>())), std::shared_ptr<yo::ast::Expr>>);




std::shared_ptr<ast::Ident> irgen::makeIdent(const std::string& str, lex::SourceLocation SL) {
    auto ident = std::make_shared<ast::Ident>(str);
    ident->setSourceLocation(SL);
    return ident;
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
    auto *numTy = llvm::cast<NumericalType>(type);
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
    callExpr->arguments = subscriptExpr->args;
    
    return callExpr;
}


llvm::DIFile* irgen::DIFileForSourceLocation(llvm::DIBuilder& builder, const lex::SourceLocation& loc) {
    const auto [directory, filename] = util::string::extractPathAndFilename(loc.getFilepath());
    return builder.createFile(filename, directory);
}







// IRGenerator

llvm::LLVMContext IRGenerator::C;

IRGenerator::IRGenerator(ast::AST &ast, const std::string &translationUnitPath, const driver::Options &options)
    : ast(ast), module(llvm::make_unique<llvm::Module>(util::fs::path_get_filename(translationUnitPath), C)),
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
    
    if (!node || node->getSourceLocation().isEmpty())  {
        builder.SetCurrentDebugLocation(llvm::DebugLoc());
        return;
    }
    const auto &SL = node->getSourceLocation();
    builder.SetCurrentDebugLocation(llvm::DebugLoc::get(SL.getLine(), SL.getColumn(), debugInfo.lexicalBlocks.back()));
}


void IRGenerator::runCodegen() {
    preflight();
    
    // note that, since the ast is potentially mutated during codegen, this is an index-based loop
    for (size_t idx = 0; idx < ast.size(); idx++) {
        codegenTLS(ast[idx]);
    }
    
    for (const auto &[name, decl] : structDecls) {
        if (decl->attributes.no_init) continue;
        
        synthesizeDefaultMemberwiseInitializer(decl, kRunCodegen);
        synthesizeDefaultCopyConstructor(decl, kRunCodegen);
        synthesizeDefaultDeallocMethod(decl, kRunCodegen);
    }
    
    handleStartupAndShutdownFunctions();
    debugInfo.builder.finalize();
    
//    for (llvm::Function &F : *module) {
//        util::fmt::print("\n{}", F.getName());
//        util::fmt::print("{}", mangling::demangle(F.getName().str()));
//    }
}





void IRGenerator::preflight() {
    std::vector<std::shared_ptr<ast::ImplBlock>> implBlocks;
    
    for (auto node : ast) {
        switch (node->getKind()) {
            case NK::FunctionDecl: {
                auto FD = llvm::cast<ast::FunctionDecl>(node);
                namedDeclInfos[FD->getName()].emplace_back(FD);
                break;
            }
                
            case NK::StructDecl: {
                auto SD = llvm::cast<ast::StructDecl>(node);
                namedDeclInfos[SD->getName()].emplace_back(SD);
                break;
            }
            
            case NK::VariantDecl: {
                auto VD = llvm::cast<ast::VariantDecl>(node);
                namedDeclInfos[VD->name->value].emplace_back(VD);
                break;
            }
            
            case NK::TypealiasDecl: {
                auto TD = llvm::cast<ast::TypealiasDecl>(node);
                namedDeclInfos[TD->name].emplace_back(TD);
                break;
            }
            
            case NK::ImplBlock: {
                implBlocks.push_back(llvm::cast<ast::ImplBlock>(node));
                break;
            }
            
            default:
                unhandled_node(node);
        }
    }
    
    for (auto implBlock : implBlocks) {
        preflightImplBlock(implBlock);
    }
    
    for (auto &[name, infos] : namedDeclInfos) {
        for (auto &info : infos) {
            registerNamedDecl(info);
        }
    }
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


void assertIsValidMemberFunction(const ast::FunctionDecl &FD, std::shared_ptr<ast::TemplateParamDeclList> implBlockTemplateParams = nullptr) {
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
        if (implBlockTemplateParams) {
            ensureTemplateParametersDontShadow({implBlockTemplateParams, sig.templateParamsDecl});
        }
    }
}






void IRGenerator::preflightImplBlock(std::shared_ptr<ast::ImplBlock> implBlock) {
    auto typeDesc = implBlock->typeDesc;
    
    if (typeDesc->isReference()) {
        // TODO is this limitation actually necessary / good?
        diagnostics::emitError(typeDesc->getSourceLocation(), "impl block type desc cannot be an lvalue reference");
    }
    
    if (implBlock->isTemplateDecl()) {
        for (auto &param : implBlock->templateParamsDecl->getParams()) {
            if (param.defaultType) {
                diagnostics::emitError(param.name->getSourceLocation(), "template parameter in impl block cannot have a default value");
            }
        }
    }
    
    auto isInstanceMethod = [](std::shared_ptr<ast::FunctionDecl> decl) -> bool {
        const auto &sig = decl->getSignature();
        if (sig.numberOfParameters() == 0) {
            return false;
        }
        auto &fstParam = sig.paramTypes[0];
        // TODO can we relax this requirement?
        if (fstParam->isReference() && fstParam->getPointee()->isNominal() && fstParam->getPointee()->getName() == "Self" && decl->paramNames[0]->value == "self") {
            return true;
        }
        return false;
    };
    
    for (auto &funcDecl : implBlock->methods) {
        // using a copy is important here so that each function gets its own typedesc, and they don't interfere w/ each other
        auto implBlockTypeDesc = ASTRewriter().handleTypeDesc(implBlock->typeDesc);
        
        // substitute all uses of `Self` in the function
        // TODO this will mess up functions w/ a template parameter named "Self"
        
        if (isInstanceMethod(funcDecl)) {
            assertIsValidMemberFunction(*funcDecl, implBlock->templateParamsDecl);
            
            // TODO this will cause the arrow to be correct, but there should also be a note saying smth like "resolved to xxx" where xxx is the impl block type desc
//            implBlockTypeDesc->setSourceLocationNested(funcDecl->signature.paramTypes[0]->getPointee()->getSourceLocation);
            
            // TODO if this is a type desc which for some reason cannot be resolved, the diag when registering a function will point to the impl block, instead of the func decl
            funcDecl->setFunctionKind(ast::FunctionKind::InstanceMethod);
            
            funcDecl = ASTRewriter({{ "Self", implBlockTypeDesc }}).handleFunctionDecl(funcDecl);
            
            if (implBlock->isTemplateDecl()) {
                funcDecl->hasInsertedImplBlockTemplateParams = true;
                funcDecl->implBlockTmplParamsStartIndex = funcDecl->signature.numberOfTemplateParameters();
                if (!funcDecl->getSignature().templateParamsDecl) {
                    funcDecl->getSignature().templateParamsDecl = std::make_shared<ast::TemplateParamDeclList>();
                }
                // TODO ensure there are no duplicates in the tmpl lists!
                for (const ast::TemplateParamDeclList::Param &param : implBlock->templateParamsDecl->getParams()) {
                    // TODO this is not a perfect approach, since it would allow explicitly specifying the template params added from the impl block?
                    funcDecl->getSignature().templateParamsDecl->addParam(param);
                }
            }
        } else { // static method
            // TODO assert that the function's template parameters (if any) are distinct and don't shadow any of the impl block's
            
            funcDecl->setFunctionKind(ast::FunctionKind::StaticMethod);
            util::vector::insert_at_front(funcDecl->signature.paramTypes, implBlockTypeDesc);
            util::vector::insert_at_front(funcDecl->paramNames, makeIdent("__unused"));
            
            LKAssert(funcDecl->signature.numberOfParameters() > 0);
        }
        
        ast.push_back(funcDecl);
        namedDeclInfos[mangling::mangleCanonicalName(funcDecl)].emplace_back(funcDecl);
    }
}










// TODO why does this function exist?
std::optional<ResolvedCallable> IRGenerator::getResolvedFunctionWithName(const std::string &name) {
    return util::map::get_opt(resolvedFunctions, name);
}





// TODO move the struct decl stuff to IRGen+Decl.cpp !!


StructType* IRGenerator::registerStructDecl(std::shared_ptr<ast::StructDecl> structDecl, NamedDeclInfo &declInfo) {
    declInfo.isRegistered = true;
    
    auto canonicalName = structDecl->name;
    auto structName = structDecl->name;
    
    if (structDecl->attributes.trivial && structDecl->isTemplateDecl()) {
        diagnostics::emitError(structDecl->getSourceLocation(), "trivial struct cannot be a template");
        // TODO implement the other checks!!!
    }
    
    if (structDecl->isTemplateDecl()) {
        LKAssert(!structDecl->attributes.no_init && "struct template cannot have no_init attribute");
        
        ensureTemplateParametersAreDistinct(*structDecl->templateParamsDecl);
        
        ast::FunctionSignature ctorSig;
        ctorSig.returnType = ast::TypeDesc::makeNominalTemplated(structName, util::vector::map(structDecl->templateParamsDecl->getParams(), [](auto &param) {
            return ast::TypeDesc::makeNominal(param.name->value);
        }));
        ctorSig.paramTypes.push_back(ASTRewriter().handleTypeDesc(ctorSig.returnType));
        ctorSig.templateParamsDecl = std::make_shared<ast::TemplateParamDeclList>(*structDecl->templateParamsDecl);
        ctorSig.setSourceLocation(structDecl->getSourceLocation());

        auto ctorFnDecl = std::make_shared<ast::FunctionDecl>(ast::FunctionKind::StaticMethod,
                                                              structName, ctorSig,
                                                              attributes::FunctionAttributes());
        ctorFnDecl->setSourceLocation(structDecl->getSourceLocation());
        ctorFnDecl->getAttributes().int_isCtor = true;
        ctorFnDecl->getAttributes().int_isSynthesized = true;
        ctorFnDecl->paramNames = { makeIdent("__unused") };

        addToAstAndRegister(ctorFnDecl);
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
    
    auto structTy = StructType::create(structName, canonicalName, structMembers, structDecl->templateInstantiationArguments, structDecl->getSourceLocation());
    structDecl->type = structTy;
    if (structDecl->attributes.int_isSynthesized) {
        structTy->setFlag(Type::Flags::IsSynthesized);
    }
    
    // TOOD should this insert using a canonical name? (so that all nominal types have the same name scheme) (also, this would be a good point to take type scopes into account?
    if (structDecl->isInstantiatedTemplateDecl()) {
        nominalTypes.insert(structName, structTy);
    } else {
        nominalTypes.insert(structDecl->name, structTy);
    }
    
    LKAssert(!util::map::has_key(structDecls, structName));
    structDecls[structName] = structDecl;
    
    if (!structDecl->attributes.no_init) {
        ast::FunctionSignature signature;
        signature.paramTypes = { ast::TypeDesc::makeResolved(structTy) };
        signature.returnType = ast::TypeDesc::makeResolved(structTy);
        signature.setSourceLocation(structDecl->getSourceLocation());

        attributes::FunctionAttributes attributes;
        attributes.no_debug_info = structDecl->attributes.no_debug_info;

        auto ctorFnDecl = std::make_shared<ast::FunctionDecl>(ast::FunctionKind::StaticMethod,
                                                              structName, signature, attributes);
        ctorFnDecl->setSourceLocation(structDecl->getSourceLocation());
        ctorFnDecl->getAttributes().int_isCtor = true;
        ctorFnDecl->getAttributes().int_isSynthesized = true;
        ctorFnDecl->paramNames = { makeIdent("__unused") };

        addToAstAndRegister(ctorFnDecl);

        // TODO instead of this, find custom implementations, if provided, and codegen them instead
        synthesizeDefaultMemberwiseInitializer(structDecl, kSkipCodegen);
        synthesizeDefaultCopyConstructor(structDecl, kSkipCodegen);
        synthesizeDefaultDeallocMethod(structDecl, kSkipCodegen);
    }
    
    declInfo.type = structTy;
    return structTy;
}




#pragma mark - Codegen



#define CASE(N, T) case NK::T: return codegen##T(llvm::cast<ast::T>(N));

llvm::Value *IRGenerator::codegenTLS(std::shared_ptr<ast::TopLevelStmt> TLS) {
    switch (TLS->getKind()) {
        CASE(TLS, FunctionDecl)
        
        case NK::StructDecl:
        case NK::ImplBlock:
        case NK::TypealiasDecl:
        case NK::VariantDecl:
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
                llvm::cast<llvm::Function>(fn.llvmValue),
                llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(builtinTypes.llvm.i8Ptr))
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
    emitDebugLocation(call);
    auto alloca = builder.CreateAlloca(getLLVMType(structTy));
    auto ident = currentFunction.getTmpIdent();
    alloca->setName(ident);
    
    builder.CreateMemSet(alloca, llvm::ConstantInt::get(builtinTypes.llvm.i8, 0),
                         module->getDataLayout().getTypeAllocSize(alloca->getAllocatedType()),
                         alloca->getAlignment());
    
    auto id = localScope.insert(ident, ValueBinding(structTy, alloca, [=]() {
        emitDebugLocation(call);
        return builder.CreateLoad(alloca);
    }, [=](llvm::Value *V) {
        LKFatalError("use references to write to object?");
    }, ValueBinding::Flags::ReadWrite));
    
    // TODO rewrite to use an rawllvmvalue as self param instead of temporarily inserting this into the local scope!
    auto callTarget = std::make_shared<ast::MemberExpr>(makeIdent(ident), kInitializerMethodName);
    auto callExpr = std::make_shared<ast::CallExpr>(*call);
    callExpr->target = callTarget;
    
    codegenExpr(callExpr);
    
    if (!putInLocalScope) {
        localScope.remove(id);
    }
    
    // TODO should this even be allowed to return anything but an RValue?
    switch (VK) {
        case LValue:
//            LKFatalError("why?");
            return alloca;
        case RValue:
            return builder.CreateLoad(alloca);
    }
}


llvm::Value* IRGenerator::constructCopyIfNecessary(Type *type, std::shared_ptr<ast::Expr> expr, bool *didConstructCopy) {
    // TODO if we let the lambda mutate `type`, we can get rid of the unpacking below
    auto shouldMakeCopy = [&, type]() mutable -> bool {
        if (isTemporary(expr)) {
            return false;
        }
        if (auto refTy = llvm::dyn_cast<ReferenceType>(type)) {
            type = refTy->getReferencedType();
        }
        
        if (llvm::isa<StructType>(type)) {
            return true;
        }
        // TODO add support for all other types w/ value semantics (tuples, ?variants?)
        return false;
    };
    
    if (shouldMakeCopy()) {
        // TODO skip the copy constructor if the type is trivially copyable?
        StructType *structTy;
        if (auto ST = llvm::dyn_cast<StructType>(type)) {
            structTy = ST;
        } else {
            auto refTy = llvm::cast<ReferenceType>(type);
            structTy = llvm::cast<StructType>(refTy->getReferencedType());
        }
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
    if (includeReferences && type->isReferenceTy()) {
        type = llvm::cast<ReferenceType>(type)->getReferencedType();
    }
    
    auto structTy = llvm::dyn_cast<StructType>(type);
    if (!structTy) {
        return nullptr;
    }
    
    auto canonicalDeallocName = mangling::mangleCanonicalName(ast::FunctionKind::InstanceMethod, kSynthesizedDeallocMethodName);
    if (!util::map::has_key(functions, canonicalDeallocName)) {
        return nullptr;
    }
    
    auto callTarget = std::make_shared<ast::MemberExpr>(expr, kSynthesizedDeallocMethodName);
    auto callExpr = std::make_shared<ast::CallExpr>(callTarget);
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


void IRGenerator::destructLocalScopeUntilMarker(util::NamedScope<ValueBinding>::Marker M, bool removeFromLocalScope) {
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






llvm::Value* IRGenerator::constructVariant(VariantType *type, const std::string &elementName) {
    LKAssert(type->hasElement(elementName));
    
    auto tagValue = type->getIndexOfElement(elementName);
    
    if (!type->hasAssociatedData()) {
        // if the variant does not carry any associated data, it is simply an integer
        auto intType = llvm::cast<llvm::IntegerType>(getLLVMType(type));
        return llvm::ConstantInt::get(intType, tagValue);
    }
    
    auto underlyingStructType = llvm::cast<StructType>(type->getUnderlyingType());
    auto alloca = builder.CreateAlloca(getLLVMType(underlyingStructType));
    auto ptr = builder.CreateStructGEP(alloca, 0);
    auto TagType = llvm::cast<llvm::IntegerType>(getLLVMType(underlyingStructType->getMember("__index").second)); // todo extract __index into a global constant!
    builder.CreateStore(llvm::ConstantInt::get(TagType, tagValue), ptr);
    return alloca;
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
    // ^ Update: this is pretty much fixed now?
    
    using TDK = ast::TypeDesc::Kind;
    
    if (!typeDesc) {
        LKFatalError("NULL TYPE DESC");
    }
    
    auto handleResolvedTy = [this, typeDesc, setInternalResolvedType](Type *ty) {
        if (setInternalResolvedType) {
            typeDesc->setResolvedType(ty);
        }
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
            
            } else {
                // a nominal, non-primitive type
                registerNamedDecls(name, [](const std::shared_ptr<ast::TopLevelStmt> &decl) -> bool {
                    // we're trying to resolve a nominal type, therefore we can ignore function decls in here
                    return !decl->isOfKind(NK::FunctionDecl);
                });
                
                if (auto entry = nominalTypes.get(name)) {
                    return handleResolvedTy(*entry);
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
            LKFatalError("TODO: rewrite to return application handlers or whatever its called"); // TODO wtf does this mean?
            const auto &FTI = typeDesc->getFunctionTypeInfo();
            const auto paramTypes = util::vector::map(FTI.parameterTypes, [&](const auto &TD) {
                return resolveTypeDesc(TD, setInternalResolvedType);
            });
            return handleResolvedTy(FunctionType::get(resolveTypeDesc(FTI.returnType, setInternalResolvedType), paramTypes, false)); // TODO add support for variadics!!
        }
        
        case TDK::Decltype:
            return handleResolvedTy(getType(typeDesc->getDecltypeExpr()));
        
        case TDK::NominalTemplated: {
            registerNamedDecls(typeDesc->getName(), [](const std::shared_ptr<ast::TopLevelStmt> &decl) -> bool {
                return !decl->isOfKind(NK::FunctionDecl);
            });
            
            llvm::SmallVector<std::shared_ptr<ast::TopLevelStmt>, 2> matchingDecls;
            
            for (const auto &[name, declInfos] : namedDeclInfos) {
                if (name != typeDesc->getName()) continue;
                for (const NamedDeclInfo &DI : declInfos) {
                    switch (DI.decl->getKind()) {
                        case NK::StructDecl:
                            if (llvm::cast<ast::StructDecl>(DI.decl)->isInstantiatedTemplateDecl()) {
                                continue;
                            }
                            break;
                        case NK::VariantDecl:
                            if (llvm::cast<ast::VariantDecl>(DI.decl)->isInstantiatedTemplateDecl()) {
                                continue;
                            }
                            break;
                        default:
                            continue;
                    }
                    matchingDecls.push_back(DI.decl);
                }
            }
            if (matchingDecls.size() == 0) {
                diagnostics::emitError(typeDesc->getSourceLocation(), "unable to resolve type");
            } else if (matchingDecls.size() > 1) {
                LKFatalError("multiple matching types?");
            }
            LKAssert(matchingDecls.size() == 1);
            
            auto argsList = std::make_shared<ast::TemplateParamArgList>();
            argsList->setSourceLocation(typeDesc->getSourceLocation());
            argsList->elements = util::vector::map(typeDesc->getTemplateArgs(), [&](auto &tmplArg) {
                resolveTypeDesc(tmplArg, setInternalResolvedType);
                return tmplArg;
            });
            
            auto &decl = matchingDecls[0];
            
            switch (decl->getKind()) {
                case NK::StructDecl:
                    return handleResolvedTy(instantiateTemplateDecl(llvm::cast<ast::StructDecl>(decl), argsList));
                
                case NK::VariantDecl:
                    return handleResolvedTy(instantiateTemplateDecl(llvm::cast<ast::VariantDecl>(decl), argsList));
                
                default:
                    LKFatalError("");
            }
        }
    }
    
    LKFatalError("unhandled type desc: %s", typeDesc->str().c_str());
}




bool IRGenerator::equal(const ast::FunctionSignature &lhs, const ast::FunctionSignature &rhs) {
    if (resolveTypeDesc(lhs.returnType, false) != resolveTypeDesc(rhs.returnType, false)) {
        return false;
    }
    
    if (lhs.numberOfParameters() != rhs.numberOfParameters()) {
        return false;
    }
    
    for (size_t i = 0; i < lhs.numberOfParameters(); i++) {
        if (resolveTypeDesc(lhs.paramTypes[i], false) != resolveTypeDesc(rhs.paramTypes[i], false)) {
            return false;
        }
    }
    
    // TODO this only compares address equality. too relaxed?
    if (lhs.templateParamsDecl != rhs.templateParamsDecl) {
        return false;
    }
    
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
            
            auto numTy = llvm::cast<NumericalType>(type);
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
                ? llvm::cast<PointerType>(type)->getPointee()
                : llvm::cast<ReferenceType>(type)->getReferencedType();
            return handle_llvm_type(getLLVMType(pointee)->getPointerTo());
        }
        
        case Type::TypeID::Struct: {
            auto structTy = llvm::cast<StructType>(type);
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
            auto fnTy = llvm::cast<FunctionType>(type);
            auto paramTypes = util::vector::map(fnTy->getParameterTypes(), [this](auto ty) { return getLLVMType(ty); });
            auto llvmFnTy = llvm::FunctionType::get(getLLVMType(fnTy->getReturnType()), paramTypes, fnTy->isVariadic()); // TODO support variadic function types?
            return handle_llvm_type(llvmFnTy);
        }
        
        case Type::TypeID::Variant: {
            auto variantTy = llvm::cast<VariantType>(type);
            return handle_llvm_type(getLLVMType(variantTy->getUnderlyingType()));
//            if (!variantTy->hasAssociatedData()) {
//                if (variantTy->numberOfElements() < std::numeric_limits<uint8_t>::max()) {
//                    return handle_llvm_type(builtinTypes.llvm.i8);
//                } else {
//                    return handle_llvm_type(builtinTypes.llvm.i64);
//                }
//            } else {
//                LKFatalError("TODO");
//            }
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
    if (auto ty = type->getLLVMDIType()) {
        return ty;
    }
    
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
                ? llvm::cast<PointerType>(type)->getPointee()
                : llvm::cast<ReferenceType>(type)->getReferencedType();
            return handle_di_type(builder.createPointerType(getDIType(pointee), pointerWidth));
        }
        
        case Type::TypeID::Numerical: {
            auto numTy = llvm::cast<NumericalType>(type);
            unsigned int encoding = 0;
            
            if (numTy->isBoolTy())         encoding = llvm::dwarf::DW_ATE_boolean;
            else if (numTy->isFloatTy())   encoding = llvm::dwarf::DW_ATE_float;
            else if (numTy->isIntegerTy()) encoding = numTy->isSigned() ? llvm::dwarf::DW_ATE_signed : llvm::dwarf::DW_ATE_unsigned;
            else LKFatalError("Unhandled type: %s", numTy->str_desc().c_str());
            
            auto ty = builder.createBasicType(numTy->str_desc(), numTy->getPrimitiveSizeInBits(), encoding);
            return handle_di_type(ty);
        }
        
        case Type::TypeID::Function: {
            auto fnTy = llvm::cast<FunctionType>(type);
            
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
                                               type->isStructTy() ? llvm::cast<StructType>(type)->getSourceLocation().getLine() : 0,
                                               DL.getTypeSizeInBits(llvmStructTy), DL.getPrefTypeAlignment(llvmStructTy),
                                               llvm::DINode::DIFlags::FlagZero, /*derivedFrom*/ nullptr, builder.getOrCreateArray(llvmMembers));
            return handle_di_type(ty);
        }
        
        case Type::TypeID::Variant: {
            // TODO can this use the DIBuilder.createVariant functions?
            auto variantTy = llvm::cast<VariantType>(type);
            return handle_di_type(getDIType(variantTy->getUnderlyingType()));
            
            //scope = DIFileForSourceLocation(builder, variantTy->getSourceLoc());
            
//            if (underlyingLLVMType->isIntegerTy()) {
//                if (underlyingLLVMType == builtinTypes.llvm.i8) {
//                    return handle_di_type(getDIType(builtinTypes.yo.u8));
//                } else if (underlyingLLVMType == builtinTypes.llvm.i64) {
//                    return handle_di_type(getDIType(builtinTypes.yo.u64));
//                } else {
//                    LKFatalError("");
//                }
//            } else {
//                LKFatalError("TODO");
//            }
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





Type* IRGenerator::getType(std::shared_ptr<ast::Expr> expr) {
    switch (expr->getKind()) {
        case NK::NumberLiteral: {
            using NT = ast::NumberLiteral::NumberType;
            auto numberLiteral = llvm::cast<ast::NumberLiteral>(expr);
            switch (numberLiteral->type) {
                case NT::Boolean:   return builtinTypes.yo.Bool;
                case NT::Integer:   return builtinTypes.yo.i64;
                case NT::Character: return builtinTypes.yo.i8; // TODO introduce a dedicated char type?
                case NT::Double:    return builtinTypes.yo.f64;
            }
        }
        
        case NK::StringLiteral: {
            using SLK = ast::StringLiteral::StringLiteralKind;
            switch (llvm::cast<ast::StringLiteral>(expr)->kind) {
                case SLK::ByteString:
                    return builtinTypes.yo.i8Ptr;
                case SLK::NormalString: {
                    if (auto StringTy = nominalTypes.get("String")) {
                        return *StringTy;
                    } else {
                        diagnostics::emitError(expr->getSourceLocation(), "unable to find 'String' type");
                    }
                }
            }
        }
        
        case NK::Ident: {
            auto identExpr = llvm::cast<ast::Ident>(expr);
            if (auto binding = localScope.get(identExpr->value)) {
                return binding->type;
            } else {
                diagnostics::emitError(identExpr->getSourceLocation(), util::fmt::format("unable to resolve identifier '{}'", identExpr->value));
            }
        }
        
        case NK::ArrayLiteralExpr: {
            auto literal = llvm::cast<ast::ArrayLiteralExpr>(expr);
            if (literal->elements.empty()) {
                diagnostics::emitError(literal->getSourceLocation(), "unable to deduce type from empty array literal");
            }
            return getType(literal->elements.front());
        }
        
        case NK::CastExpr:
            return resolveTypeDesc(llvm::cast<ast::CastExpr>(expr)->destType);
        
        case NK::CallExpr:
            return resolveTypeDesc(resolveCall(llvm::cast<ast::CallExpr>(expr), kSkipCodegen).signature.returnType);
        
        case NK::MatchExpr: {
            // TODO re-implement this to infer the return type using the match maker
            return getType(llvm::cast<ast::MatchExpr>(expr)->branches.at(0).expr);
        }
        
        case NK::RawLLVMValueExpr:
            return llvm::cast<ast::RawLLVMValueExpr>(expr)->type;
        
        case NK::SubscriptExpr: {
            auto subscriptExpr = llvm::cast<ast::SubscriptExpr>(expr);
            Type *type = nullptr;
            codegenSubscriptExpr(subscriptExpr, RValue, kSkipCodegen, &type);
            return type;
        }
        
        // <target>.<member>
        case NK::MemberExpr: {
            auto memberExpr = llvm::cast<ast::MemberExpr>(expr);
            Type *type = nullptr;
            codegenMemberExpr(memberExpr, RValue, kSkipCodegen, &type);
            return type;
        }
        
        case NK::UnaryExpr: {
            using Op = ast::UnaryExpr::Operation;
            
            auto unaryExpr = llvm::cast<ast::UnaryExpr>(expr);
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
            // TODO take custom overloads into account!
            return builtinTypes.yo.Bool;
        
        case NK::BinOp: {
            auto binopExpr = llvm::cast<ast::BinOp>(expr);
            auto mangledCanonicalName = mangling::mangleCanonicalName(binopExpr->getOperator());
//            auto mangledCanonicalName = mangling::encodeOperator(binopExpr->getOperator());
            // TODO don't allocate an object for every check!
            auto tempCallExpr = std::make_shared<ast::CallExpr>(makeIdent(mangledCanonicalName),
                                                                std::vector<std::shared_ptr<ast::Expr>>{ binopExpr->getLhs(), binopExpr->getRhs() });
            tempCallExpr->setSourceLocation(binopExpr->getSourceLocation());
            return resolveTypeDesc(resolveCall(tempCallExpr, kSkipCodegen).signature.returnType);
        }
        
        case NK::LambdaExpr: {
            auto lambdaExpr = llvm::cast<ast::LambdaExpr>(expr);
            return synthesizeLambdaExpr(lambdaExpr);
        }
        
        case NK::TupleExpr: {
            auto tupleExpr = llvm::cast<ast::TupleExpr>(expr);
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
    if (ty->isReferenceTy()) {
        return false;
    }
    
    if (expr->isKnownAsTemporary) {
        return true;
    }
    
    switch (expr->getKind()) {
        case NK::Ident:
        case NK::MemberExpr:
        case NK::SubscriptExpr:
            return false;
        
        default:
            return true;
    }
}




bool IRGenerator::typeIsConstructible(Type *type) {
    return memberFunctionCallResolves(type, kInitializerMethodName, {});
}

bool IRGenerator::typeIsCopyConstructible(Type *type) {
    return memberFunctionCallResolves(type, kInitializerMethodName, { type->getReferenceTo() });
}

bool IRGenerator::typeIsDestructible(Type *type) {
    return memberFunctionCallResolves(type, kSynthesizedDeallocMethodName, {});
}





template <typename T>
Type* IRGenerator::instantiateTemplateDecl(const std::shared_ptr<T> &decl, const std::shared_ptr<ast::TemplateParamArgList> &tmplArgs) {
    LKAssert(decl->isTemplateDecl());
    
    TemplateTypeMapping tmplMapping = resolveTemplateDeclTemplateParamsFromExplicitArgs(decl, tmplArgs, false);
    
    std::shared_ptr<T> specDecl;
    
    if constexpr(std::is_same_v<T, ast::StructDecl>) {
        specDecl = ASTRewriter(tmplMapping).handleStructDecl(decl);
    } else if constexpr(std::is_same_v<T, ast::VariantDecl>) {
        specDecl = ASTRewriter(tmplMapping).handleVariantDecl(decl);
    } else {
        static_assert(util::always_false_v<T>);
    }
    
    specDecl->templateParamsDecl = nullptr;
    
    for (size_t idx = 0; idx < decl->numberOfTemplateParameters(); idx++) {
        auto &name = decl->templateParamsDecl->getParams()[idx].name->value;
        specDecl->templateInstantiationArguments.push_back(tmplMapping.at(name)->getResolvedType());
    }
    
    auto mangledName = mangling::mangleFullyResolved(specDecl);
    bool isTemporary = util::map::contains_where(tmplMapping, [](const std::string &name, const std::shared_ptr<ast::TypeDesc> &TD) -> bool {
        return TD->getResolvedType() && TD->getResolvedType()->hasFlag(Type::Flags::IsTemporary);
    });
    if (isTemporary) {
        return Type::createTemporary(mangledName, specDecl->getName(), specDecl->templateInstantiationArguments);
    }
    
    if (auto ty = nominalTypes.get(mangledName)) {
        return *ty;
    }
    
    return addToAstAndRegister(specDecl);
}









#pragma mark - Synthesized Functions

// TODO for all 3 functions below, the kSkipCodegen option is implemented really bad, mainly since we still create the ast, only to then not run kt through codegen
// TODO there's a lot of duplicate code in the functions below!


StructType* IRGenerator::synth_getStructDeclStructType(const std::shared_ptr<ast::StructDecl> &decl) {
    if (auto ty = decl->type) {
        return ty;
    } else if (decl->isInstantiatedTemplateDecl()) {
        return llvm::cast<StructType>(*nominalTypes.get(mangling::mangleFullyResolved(decl)));
    } else {
        return llvm::cast<StructType>(*nominalTypes.get(decl->name));
    }
}


bool IRGenerator::memberFunctionCallResolves(Type *targetTy, const std::string &name, const std::vector<Type *> &argTys) {
    auto expr_for_type = [](Type *type) {
        return std::make_shared<ast::RawLLVMValueExpr>(nullptr, type);
    };
    
    auto target = std::make_shared<ast::MemberExpr>(expr_for_type(targetTy), name);
    auto call = std::make_shared<ast::CallExpr>(target);
    call->arguments = util::vector::map(argTys, [&](Type *type) -> std::shared_ptr<ast::Expr> {
        return expr_for_type(type);
    });
    return canResolveCall(call);
}



llvm::Value* IRGenerator::synthesizeDefaultMemberwiseInitializer(std::shared_ptr<ast::StructDecl> structDecl, SkipCodegenOption codegenOption) {
    // TODO set the source location for all nodes generated in here!
        
    const auto &SL = structDecl->getSourceLocation();
//    StructType *ST;
//    if (auto ty = structDecl->type) {
//        ST = ty;
//    } else if (structDecl->isInstantiatedTemplateDecl()) {
//        ST = llvm::cast<StructType>(*nominalTypes.get(mangling::mangleFullyResolved(structDecl)));
//    } else {
//        ST = llvm::cast<StructType>(*nominalTypes.get(structDecl->name));
//    }
    auto ST = synth_getStructDeclStructType(structDecl);
    auto &SM = ST->getMembers();
    
    if (//!ST->hasFlag(Type::Flags::IsSynthesized) &&
        memberFunctionCallResolves(ST->getReferenceTo(), kInitializerMethodName, util::vector::map(SM, [](auto pair) { return pair.second; }))) {
        return nullptr;
    }
    
    
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
    
    addToAstAndRegister(FD);
    
    if (codegenOption == kSkipCodegen) {
        return nullptr;
    } else {
        return codegenFunctionDecl(FD); // TODO considering that addAndRegisterFunction already queues the decl for codegen, can we get rid of these calls (here and in th e2 functions below)
    }
}



llvm::Value* IRGenerator::synthesizeDefaultCopyConstructor(std::shared_ptr<ast::StructDecl> structDecl, SkipCodegenOption codegenOption) {
    auto &SL = structDecl->getSourceLocation();
    auto ST = synth_getStructDeclStructType(structDecl);
    auto &SM = ST->getMembers();
    
    if (//!ST->hasFlag(Type::Flags::IsSynthesized) &&
        memberFunctionCallResolves(ST->getReferenceTo(), kInitializerMethodName, { ST->getReferenceTo() })) {
        return nullptr;
    }
    
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
    
    addToAstAndRegister(FD);
    
    if (codegenOption == kSkipCodegen) {
        return nullptr;
    } else {
        return codegenFunctionDecl(FD);
    }
}



llvm::Value* IRGenerator::synthesizeDefaultDeallocMethod(std::shared_ptr<ast::StructDecl> structDecl, SkipCodegenOption codegenOption) {
    auto &SL = structDecl->getSourceLocation();
    auto ST = synth_getStructDeclStructType(structDecl);
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
    
    auto body = std::make_shared<ast::CompoundStmt>();
    body->setSourceLocation(SL);
    
    
//    auto callExpr = std::make_shared<ast::CallExpr>(makeIdent("printf", SL));
//    callExpr->setSourceLocation(SL);
//    callExpr->arguments = {
//        std::make_shared<ast::StringLiteral>(util::fmt::format("[{} __dealloc]\n", ST->getName()), ast::StringLiteral::StringLiteralKind::ByteString)
//    };
//    callExpr->arguments[0]->setSourceLocation(SL);
//    auto EStmt = std::make_shared<ast::ExprStmt>(callExpr);
//    EStmt->setSourceLocation(SL);
//    body->statements.push_back(EStmt);
    
    
    // Invoke a custom destructor, if defined
    if (!ST->hasFlag(Type::Flags::IsSynthesized) && memberFunctionCallResolves(ST, kDeallocMethodName, {})) {
        auto setSL = [&SL](auto node) {
            node->setSourceLocation(SL);
        };
        auto target = std::make_shared<ast::MemberExpr>(selfIdent, kDeallocMethodName);
        auto call = std::make_shared<ast::CallExpr>(target);
        auto stmt = std::make_shared<ast::ExprStmt>(call);
        body->statements.push_back(stmt);
        setSL(target);
        setSL(call);
        setSL(stmt);
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
    
    addToAstAndRegister(FD);
    
    if (codegenOption == kSkipCodegen) {
        return nullptr;
    } else {
        return codegenFunctionDecl(FD);
    }
}
