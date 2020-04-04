//
//  IRGen+Stmt.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-03-06.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "IRGen.h"
#include "lex/Diagnostics.h"
#include "util_llvm.h"
#include "util/llvm_casting.h"
#include "util/VectorUtils.h"

using namespace yo;
using namespace yo::irgen;
using NK = ast::Node::Kind;


llvm::Value* IRGenerator::codegenCompoundStmt(std::shared_ptr<ast::CompoundStmt> compoundStmt) {
    const auto &stmts = compoundStmt->statements;
    
    auto marker = localScope.getMarker();
    bool didReturn = false;
        
    for (auto it = stmts.begin(); !didReturn && it != stmts.end(); it++) {
        const auto stmt = *it;
        codegenLocalStmt(stmt);
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


// TODO should assignments return something?
llvm::Value* IRGenerator::codegenAssignment(std::shared_ptr<ast::Assignment> assignment) {
    llvm::Value *llvmTargetLValue = nullptr;
    llvm::Value *llvmRhsVal = nullptr;
    auto rhsExpr = assignment->value;
    auto lhsTy = getType(assignment->target);
    auto rhsTy = getType(assignment->value);
    
    if (rhsExpr->isOfKind(NK::BinOp) && llvm::cast<ast::BinOp>(rhsExpr)->isInPlaceBinop()) {
        // <lhs> <op>= <rhs>
        // The issue here is that we have to make sure not to evaluate lhs twice
        auto binop = llvm::dyn_cast<ast::BinOp>(rhsExpr);
        //LKAssert(assignment->target == binop->getLhs()); // TODO re-enable this check!!!!!!!!!
        
        auto lhsLValue = codegenExpr(binop->getLhs(), LValue, /*insertImplicitLoadInst*/ false);
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
        llvmTargetLValue = codegenExpr(assignment->target, LValue, /*insertImplicitLoadInst*/ false);
        
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
        
//        Type *T;
//        if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(rhsExpr, lhsTy, &T)) {
//            auto msg = util::fmt::format("cannot assign to '{}' from incompatible type '{}'", lhsTy, T);
//            diagnostics::emitError(assignment->getSourceLocation(), msg);
//        }
        if (!applyImplicitConversionIfNecessary(rhsExpr, lhsTy)) {
            auto msg = util::fmt::format("cannot assign to '{}' from value of incompatible type '{}'", lhsTy, getType(rhsExpr));
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
            codegenLocalStmt(destructStmt);
        }
    }
    
    if (lhsTy->isReferenceTy() && assignment->overwriteReferences) {
        llvmRhsVal = codegenExpr(rhsExpr, LValue, /*insertImplicitLoadInst*/ false);
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






llvm::Value* IRGenerator::codegenVarDecl(std::shared_ptr<ast::VarDecl> varDecl) {
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
                                                      varDecl->getSourceLocation().getLine(),
                                                      getDIType(type));
        debugInfo.builder.insertDeclare(alloca, D,
                                        debugInfo.builder.createExpression(),
                                        llvm::DebugLoc::get(varDecl->getSourceLocation().getLine(), 0, currentFunction.llvmFunction->getSubprogram()),
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
            codegenAssignment(assignment);
        } else {
            auto V = codegenExpr(initialValueExpr, LValue);
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





llvm::Value* IRGenerator::codegenReturnStmt(std::shared_ptr<ast::ReturnStmt> returnStmt) {
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
            if (!applyImplicitConversionIfNecessary(expr, retvalTy)) {
                typeMismatch();
            }
        }
        
    handle:
        if (returnType->isReferenceTy()) {
            auto V = codegenExpr(expr, LValue);
            emitDebugLocation(returnStmt);
            builder.CreateStore(V, currentFunction.retvalAlloca);
        } else {
            auto assignment = std::make_shared<ast::Assignment>(makeIdent(kRetvalAllocaIdentifier), expr);
            assignment->setSourceLocation(returnStmt->getSourceLocation());
            assignment->shouldDestructOldValue = false;
            codegenAssignment(assignment);
        }
    } else {
        LKAssert(returnType->isVoidTy());
    }
    
    destructLocalScopeUntilMarker(currentFunction.stackTopMarker, /*removeFromLocalScope*/ false);
    
    emitDebugLocation(returnStmt);
    return builder.CreateBr(currentFunction.returnBB);
}




llvm::Value* IRGenerator::codegenIfStmt(std::shared_ptr<ast::IfStmt> ifStmt) {
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
        
        if (branch->kind == BK::Else) {
            break;
        }
        if (i > 0) {
            auto BB = branchConditionBlocks[i];
            F->getBasicBlockList().push_back(BB);
            builder.SetInsertPoint(BB);
        }
        
        auto condTy = getType(branch->condition);
        
//        if (auto BoolTy = builtinTypes.yo.Bool; condTy != BoolTy && condTy != BoolTy->getReferenceTo()) {
//            auto msg = util::fmt::format("type of expression ('{}') incompatible with expected type '{}'", condTy, BoolTy);
//            diagnostics::emitError(branch->getSourceLocation(), msg);
//        }
        
//        auto condV = codegenExpr(branch->condition);
//        if (condTy == builtinTypes.yo.Bool->getReferenceTo()) {
//            condV = builder.CreateLoad(condV);
//        }
        auto condV = codegenBoolComp(branch->condition);
        
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
        
        codegenCompoundStmt(ifStmt->branches[index]->body);
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



llvm::Value* IRGenerator::codegenWhileStmt(std::shared_ptr<ast::WhileStmt> whileStmt) {
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
    
    builder.CreateCondBr(codegenExpr(whileStmt->condition), bodyBB, mergeBB);
    
    F->getBasicBlockList().push_back(bodyBB);
    builder.SetInsertPoint(bodyBB);
    
    currentFunction.breakContDestinations.push({mergeBB, condBB});
    
    codegenCompoundStmt(whileStmt->body);
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


llvm::Value* IRGenerator::codegenForLoop(std::shared_ptr<ast::ForLoop> forLoop) {
    auto targetTy = getType(forLoop->expr);
    
    if (!memberFunctionCallResolves(targetTy, kIteratorMethodName, {})) {
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
    return codegenCompoundStmt(stmt);
}




llvm::Value* IRGenerator::codegenBreakContStmt(std::shared_ptr<ast::BreakContStmt> stmt) {
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
