//
//  IRGen+Decl.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-03-06.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "IRGen.h"
#include "Mangling.h"
#include "Diagnostics.h"
#include "util_llvm.h"


using namespace yo;
using namespace yo::irgen;
using NK = ast::Node::Kind;



void IRGenerator::registerFunction(std::shared_ptr<ast::FunctionDecl> functionDecl) {
    LKAssert(functionDecl->getParamNames().size() == functionDecl->getSignature().numberOfParameters());
    
    const auto &sig = functionDecl->getSignature();
    const bool isMain = functionDecl->isOfFunctionKind(ast::FunctionKind::GlobalFunction) && functionDecl->getName() == "main";
    
    const auto argumentOffset = argumentOffsetForFunctionKind(functionDecl->getFunctionKind());
    
    if (functionDecl->getAttributes().extern_) {
        functionDecl->getAttributes().no_mangle = true;
    }
    
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
    
    
    if (functionDecl->implTypeDesc && !functionDecl->implType) {
        // has an impl type desc, but does not have a resolved impl type -> member function of a templated type
        return;
    }
    
//    if (auto TD = functionDecl->implTypeDesc; TD && TD->isNominal() && util::map::has_key(structTemplateDecls, TD->getName())) {
//        // template
//
//    }
    
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
    
    std::vector<llvm::Type *> paramTypes(sig.numberOfParameters(), nullptr);
    
    for (size_t idx = 0; idx < sig.numberOfParameters(); idx++) {
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




llvm::Value* IRGenerator::codegenFunctionDecl(std::shared_ptr<ast::FunctionDecl> functionDecl) {
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
    
    
    std::vector<llvm::AllocaInst *> paramAllocas;
    
    for (size_t i = 0; i < sig.numberOfParameters(); i++) {
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
    
    
    for (size_t i = 0; i < sig.numberOfParameters(); i++) {
        auto alloca = paramAllocas.at(i);
        builder.CreateStore(&F->arg_begin()[i], alloca);
        
        const auto &paramTy = sig.paramTypes.at(i);
        const auto &paramNameDecl = functionDecl->getParamNames().at(i);
        if (shouldEmitDebugInfo()) {
            auto SP = debugInfo.lexicalBlocks.back();
            auto varInfo = debugInfo.builder.createParameterVariable(SP, alloca->getName(), i + 1, SP->getFile(),
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
                                            llvm::DebugLoc::get(sig.getSourceLocation().getLine(), 0, SP), entryBB);
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




llvm::Value* IRGenerator::codegenStructDecl(std::shared_ptr<ast::StructDecl> structDecl) {
    if (structDecl->isTemplateDecl()) return nullptr;
    
//    for (auto &implBlock : structDecl->implBlocks) {
//        for (auto &FD : implBlock->methods) {
//            codegen(FD);
//        }
//    }
    for (auto FD : structDecl->methods) {
        codegenFunctionDecl(FD);
    }
    
    return nullptr;
}


