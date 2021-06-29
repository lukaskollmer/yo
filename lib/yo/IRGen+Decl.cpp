//
//  IRGen+Decl.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-03-06.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "IRGen.h"
#include "Mangling.h"
#include "lex/Diagnostics.h"
#include "util_llvm.h"
#include "util/VectorUtils.h"
#include "util/MapUtils.h"
#include "util/llvm_casting.h"


using namespace yo;
using namespace yo::irgen;
using NK = ast::Node::Kind;




void IRGenerator::registerNamedDecl(NamedDeclInfo &declInfo) {
    if (declInfo.isRegistered) {
        return;
    }
    
    switch (declInfo.decl->getKind()) {
        case NK::FunctionDecl:
            registerFunction(llvm::cast<ast::FunctionDecl>(declInfo.decl), declInfo);
            break;
        
        case NK::VariantDecl:
            registerVariantDecl(llvm::cast<ast::VariantDecl>(declInfo.decl), declInfo);
            break;
        
        case NK::StructDecl:
            registerStructDecl(llvm::cast<ast::StructDecl>(declInfo.decl), declInfo);
            break;
        
        case NK::TypealiasDecl:
            registerTypealias(llvm::cast<ast::TypealiasDecl>(declInfo.decl), declInfo);
            break;
        
        default:
            LKFatalError("ugh %s", ast::nodeKindToString(declInfo.decl->getKind()).c_str());
    }
}



void IRGenerator::registerTypealias(std::shared_ptr<ast::TypealiasDecl> decl, NamedDeclInfo &declInfo) {
    // TODO add support for typealias templates!!
    
    auto type = resolveTypeDesc(decl->type);
    nominalTypes.insert(decl->name, type);
    
    declInfo.isRegistered = true;
    declInfo.type = type;
}



VariantType* IRGenerator::registerVariantDecl(std::shared_ptr<ast::VariantDecl> decl, NamedDeclInfo &declInfo) {
    declInfo.isRegistered = true;
    
    if (decl->isTemplateDecl() && !decl->isInstantiatedTemplateDecl()) {
        LKFatalError("TODO");
        return nullptr;
    }
    
    
    auto name = mangling::mangleFullyResolved(decl);
    bool hasAssociatedData = false;
    VariantType::Elements elements;
    Type *underlyingType = nullptr;
    
    for (auto &member : decl->members) {
        TupleType *associatedData = nullptr;
        if (auto params = member.params) {
            LKAssert(params->isTuple());
            if (params->getTupleMembers().size() < 1) {
                LKFatalError("variant case associated data cannot be empty");
            }
            hasAssociatedData = true;
            associatedData = llvm::cast<TupleType>(resolveTypeDesc(member.params));
        }
        elements.push_back(std::make_pair(member.name->value, associatedData));
    }
    
    NumericalType *indexType;
    if (decl->members.size() < std::numeric_limits<uint8_t>::max()) {
        indexType = builtinTypes.yo.u8;
    } else {
        indexType = builtinTypes.yo.u64;
    }
    
    if (!hasAssociatedData) {
        underlyingType = indexType;
    
    } else {
        using ElementType = VariantType::Elements::value_type;
        
        auto largestMember = std::max_element(elements.begin(), elements.end(), [&](const ElementType &lhs, const ElementType &rhs) -> bool {
            auto lhsTy = lhs.second;
            auto rhsTy = rhs.second;
            
            if (!lhsTy && !rhsTy) {
                return false;
            } else if (lhsTy && !rhsTy) {
                return false;
            } else if (!lhsTy && rhsTy) {
                return true;
            } else {
                auto &DL = module->getDataLayout();
                return DL.getTypeStoreSize(getLLVMType(lhsTy)) < DL.getTypeStoreSize(getLLVMType(rhsTy));
            }
        });
        LKAssert(largestMember != elements.end());
        
        StructType::MembersT members = {
            std::make_pair("__index", indexType),
            std::make_pair("__data", largestMember->second)
        };
        underlyingType = StructType::create("__variant_impl", "", members, decl->getSourceLocation());
    }
    
    auto variantType = new VariantType(name, elements, underlyingType, decl->getSourceLocation());
    nominalTypes.insert(decl->name->value, variantType);
    
    for (const auto &element : variantType->getElements()) {
        if (element.second) {
            synthesizeVariantConstructor(variantType, element);
        }
    }
    
    declInfo.type = variantType;
    return variantType;
}


void IRGenerator::synthesizeVariantConstructor(VariantType *variantTy, const VariantType::Elements::value_type &elem) {
    auto &[name, data] = elem;
    
    ast::FunctionSignature sig;
    sig.returnType = ast::TypeDesc::makeResolved(variantTy);
    sig.paramTypes = { ast::TypeDesc::makeResolved(variantTy) };
    for (auto ty : data->getMembers()) {
        sig.paramTypes.push_back(ast::TypeDesc::makeResolved(ty));
    }
    attributes::FunctionAttributes attr;

    auto funcDecl = std::make_shared<ast::FunctionDecl>(ast::FunctionKind::StaticMethod, name, sig, attr);
    funcDecl->paramNames = { makeIdent("__unused") };
    for (size_t idx = 0; idx < data->memberCount(); idx++) {
        funcDecl->paramNames.push_back(makeIdent(util::fmt::format("__arg{}", idx)));
    }
    
    funcDecl->body = std::make_shared<ast::CompoundStmt>();
    auto &B = funcDecl->body->statements;
    
    auto callExpr = std::make_shared<ast::CallExpr>(makeIdent("__trap"));
    B.push_back(std::make_shared<ast::ExprStmt>(callExpr));
    addToAstAndRegister(funcDecl);
}






llvm::Function* IRGenerator::registerFunction(std::shared_ptr<ast::FunctionDecl> functionDecl, NamedDeclInfo &declInfo) {
    declInfo.isRegistered = true;
    EmptyScopeHandle ESH(*this);
    
    LKAssert(functionDecl->getParamNames().size() == functionDecl->getSignature().numberOfParameters());
    
    const auto &sig = functionDecl->getSignature();
    auto &attrs = functionDecl->getAttributes();
    bool isMain = functionDecl->isOfFunctionKind(ast::FunctionKind::GlobalFunction) && functionDecl->getName() == "main";
    
    bool hasImplicitSelfArg = functionDecl->isInstanceMethod() || functionDecl->isStaticMethod();
    
    if (attrs.extern_) {
        attrs.no_mangle = true;
    }
    
    if (isMain) {
        attrs.no_mangle = true;
        
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
    
//    if (functionDecl->isOfFunctionKind(ast::FunctionKind::StaticMethod)) {
//        util::fmt::print("canName: {}", mangling::mangleCanonicalName(functionDecl));
//        LKFatalError("");
//    }
    
    if (sig.isTemplateDecl() || attrs.intrinsic || attrs.int_isCtor) {
        // TODO detect ambiguous template declarations (ie, same name w/ same signature, etc)
        auto canonicalName = mangling::mangleCanonicalName(functionDecl);
        
        if (attrs.int_isCtor && !functions[canonicalName].empty()) {
            // Prevent multiple ambiguous (and unnecessary) overloads for compiler-generated template instantiations
//            LKFatalError("when/why do we reach here?");
            return nullptr;
        }
        
        functions[canonicalName].push_back(ResolvedCallable(sig, functionDecl, nullptr, hasImplicitSelfArg));
        return nullptr;
    }
    
    
    // Resolve parameter types and return type
    // We temporarily need to put all parameters into the local scope, since they might be used in a decltype statement
    
    LKAssert(localScope.isEmpty());
    
    std::vector<Type *> paramTypes(sig.numberOfParameters(), nullptr);
    std::vector<llvm::Type *> llvmParamTypes(sig.numberOfParameters(), nullptr);
    
    for (size_t idx = 0; idx < sig.numberOfParameters(); idx++) {
        auto type = resolveTypeDesc(sig.paramTypes[idx]);
        auto name = functionDecl->getParamNames()[idx]->value;
        paramTypes[idx] = type;
        llvmParamTypes[idx] = type->getLLVMType();
        localScope.insert(name, ValueBinding(type, nullptr, []() -> llvm::Value* {
            LKFatalError("invalid operation");
        }, [](llvm::Value *) {
            LKFatalError("invalid operation");
        }, ValueBinding::Flags::None));
    }
    
    if (functionDecl->isOfFunctionKind(ast::FunctionKind::StaticMethod)) {
        paramTypes.erase(paramTypes.begin());
        llvmParamTypes.erase(llvmParamTypes.begin());
    }
    
    auto returnType = resolveTypeDesc(sig.returnType);
    localScope.removeAll();
    
    
    auto canonicalName = mangling::mangleCanonicalName(functionDecl);
    auto resolvedName = attrs.extern_ ? canonicalName : mangleFullyResolved(functionDecl);
    
    
    if (util::map::has_key(resolvedFunctions, resolvedName)) {
        auto &otherRC = resolvedFunctions.at(resolvedName);
        const auto &otherAttrs = otherRC.funcDecl->getAttributes();
        
        // TODO is this assert really necessary/good?
        LKAssert(equal(sig, otherRC.signature) && "how can there be multiple decls w/ the same resolved name, but different signatures?");
        
        if (attrs.extern_ || otherAttrs.extern_) {
            // if one of multiple decls is extern, all must be, and they must have the same signature
            if (attrs.extern_ != otherAttrs.extern_) { // TODO replace w/ `attrs != otherAttrs` to check for full equality
                diagnostics::emitNote(otherRC.funcDecl->getSourceLocation(), "other declaration here");
                auto msg = util::fmt::format("multiple declarations of function '{}' with different attribute lists", resolvedName);
                diagnostics::emitError(functionDecl->getSourceLocation(), msg);
            }
            // TODO we should to remove the other decl from namedDeclInfos (otherwise there can be duplicates in name lookup)!
//            auto &declInfos = namedDeclInfos[canonicalName];
//            auto it = std::find_if(declInfos.begin(), declInfos.end(), [&](const NamedDeclInfo &declInfo) -> bool {
//                return declInfo.decl == functionDecl;
//            });
//            declInfos.erase(it);
            return llvm::cast<llvm::Function>(otherRC.llvmValue);
        
        } else if (attrs.int_isFwdDecl || otherAttrs.int_isFwdDecl) {
            if (attrs.int_isFwdDecl == otherAttrs.int_isFwdDecl) {
                if (functionDecl->getName() == otherRC.funcDecl->getName() && functionDecl->getName() == kSynthesizedDeallocMethodName) {
                    functionDecl->getAttributes().int_skipCodegen = true;
                    return llvm::cast<llvm::Function>(otherRC.llvmValue);
                }
                // Note: if we end up here, that indicates a bug in the compiler's function synthesis?
                LKFatalError("multiple fwd decls for the same function '%s'", resolvedName.c_str());
            }
            
            // one of the two functions is a fwd decl and the other isn't
            // -> we keep the one which is not, and discard the other one
            
            if (attrs.int_isFwdDecl) {
                // trying to register a fwd decl for an already existing function
                attrs.int_skipCodegen = true;
//                LKFatalError("");
                return llvm::cast<llvm::Function>(otherRC.llvmValue);
            } else {
                // trying to register a function for which there exists a forward decl
                // essentially, we just swap out the declarations
                ResolvedCallable RC(functionDecl, otherRC.llvmValue, hasImplicitSelfArg);
                resolvedFunctions.insert_or_assign(resolvedName, RC);
                
                std::vector<ResolvedCallable> &RCs = functions[canonicalName];
                for (size_t idx = 0; idx < RCs.size(); idx++) {
                    if (RCs[idx].funcDecl == otherRC.funcDecl) {
                        RCs[idx] = RC;
                    }
                }
                return llvm::cast<llvm::Function>(RC.llvmValue);
            }
            
        } else {
            diagnostics::emitNote(otherRC.funcDecl->getSourceLocation(), "other declaration here");
            diagnostics::emitError(functionDecl->getSourceLocation(), "multiple function declarations w/ same signature");
        }
    }
    
    
    auto FT = FunctionType::get(returnType, paramTypes, sig.isVariadic);
    auto F = llvm::Function::Create(llvm::cast<llvm::FunctionType>(getLLVMType(FT)), llvm::Function::LinkageTypes::ExternalLinkage, resolvedName, *module);
    F->setDSOLocal(!functionDecl->getAttributes().extern_);
    
    ResolvedCallable RC(functionDecl, F, hasImplicitSelfArg);
    LKAssert(!util::map::has_key(resolvedFunctions, resolvedName));
    resolvedFunctions.emplace(resolvedName, RC);
    functions[canonicalName].push_back(RC);
    
    declInfo.llvmValue = F;
    declInfo.type = FT;
    return F;
}




llvm::Value* IRGenerator::codegenFunctionDecl(std::shared_ptr<ast::FunctionDecl> functionDecl) {
    const auto &sig = functionDecl->getSignature();
    const auto &attr = functionDecl->getAttributes();
    
    LKAssert(!attr.int_isDelayed);
    LKAssert(localScope.isEmpty());
    
    if (attr.extern_ || attr.intrinsic || attr.int_isCtor || sig.isTemplateDecl()) {
        return nullptr;
    }
    
    auto resolvedName = mangleFullyResolved(functionDecl);
    
    auto F = module->getFunction(resolvedName);
    if (!F) {
        LKFatalError("Unable to find function '%s'", resolvedName.c_str());
    }
    
    if (attr.int_skipCodegen) {
        return F;
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
    
    
    if (functionDecl->getAttributes().int_isFwdDecl && !F->empty()) {
        return F;
    }
    
    if (!F->empty()) {
        F->print(llvm::outs());
        std::cout << functionDecl->description() << std::endl;
        std::cout << F->getName().str() << std::endl;
        std::cout << mangling::demangle(F->getName().str()) << std::endl;
    }
    
    
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
                                           sig.getSourceLocation().getLine(),
                                           toDISubroutineType(sig),
                                           sig.getSourceLocation().getLine(),
                                           llvm::DINode::FlagZero,
                                           llvm::DISubprogram::DISPFlags::SPFlagDefinition);
        emitDebugLocation(nullptr);
        F->setSubprogram(SP);
        debugInfo.builder.finalizeSubprogram(SP);
        debugInfo.lexicalBlocks.push_back(SP);
    }
    
    // static methods have an unused implicitly inserted first parameter which must be ignored during codegen
    size_t paramsOffset = functionDecl->isStaticMethod();
    
    std::vector<llvm::AllocaInst *> paramAllocas;
    paramAllocas.reserve(sig.numberOfParameters() - paramsOffset);
    
    for (size_t i = paramsOffset; i < sig.numberOfParameters(); i++) {
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
    
    for (size_t i = paramsOffset; i < sig.numberOfParameters(); i++) {
        auto alloca = paramAllocas.at(i - paramsOffset);
        builder.CreateStore(&F->arg_begin()[i - paramsOffset], alloca);
        
        const auto &paramTy = sig.paramTypes.at(i);
        const auto &paramNameDecl = functionDecl->getParamNames().at(i);
        if (shouldEmitDebugInfo()) {
            auto SP = debugInfo.lexicalBlocks.back();
            auto varInfo = debugInfo.builder.createParameterVariable(SP, alloca->getName(), i - paramsOffset + 1, SP->getFile(),
                                                                     paramNameDecl->getSourceLocation().getLine(),
                                                                     resolveTypeDesc(paramTy)->getLLVMDIType());
            debugInfo.builder.insertDeclare(alloca, varInfo, debugInfo.builder.createExpression(),
                                            llvm::DILocation::get(C, paramNameDecl->getSourceLocation().getLine(), paramNameDecl->getSourceLocation().getColumn(), SP), entryBB);
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
                                                          sig.getSourceLocation().getLine(), returnType->getLLVMDIType());
            debugInfo.builder.insertDeclare(retvalAlloca, D, debugInfo.builder.createExpression(),
                                            llvm::DILocation::get(C, sig.getSourceLocation().getLine(), 0, SP), entryBB);
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
    
    
    codegenCompoundStmt(functionDecl->getBody());
    
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
