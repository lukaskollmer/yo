//
//  MatchMaker.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-04-03.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#include "MatchMaker.h"
#include "IRGen.h"
#include "util/util.h"
#include "util/VectorUtils.h"
#include "util/llvm_casting.h"

#include "llvm/Support/Casting.h"

#include <string>
#include <vector>
#include <map>

using namespace yo;
using namespace yo::irgen;
using NK = ast::Node::Kind;

const std::string kWildcardIdentValue = "_";

enum class MatchMaker::PatternKind {
    Literal,
    SimpleNamedBinding,
//    Wildcard
};


MatchMaker::PatternKind MatchMaker::getPatternKind(const ast::MatchExprPattern &pattern) {
    auto &expr = pattern.expr;
    if (expr->isOfKind(NK::NumberLiteral) || expr->isOfKind(NK::StringLiteral)) {
        return PatternKind::Literal;
    }
    
    if (expr->isOfKind(NK::Ident)) {
        return PatternKind::SimpleNamedBinding;
    }

    LKFatalError("");
}


void MatchMaker::fetchPatternKinds() {
    LKAssert(patternKinds.empty());
    for (const ast::MatchExprBranch &branch : matchExpr->branches) {
        patternKinds.push_back(util::vector::map(branch.patterns, [&](const auto &expr) {
            return getPatternKind(expr);
        }));
    }
}


bool MatchMaker::validatePatterns() {
//    for (const auto &patternInfos : branchPatternInfos) {
//        LKAssert(!patternInfos.empty());
//        std::vector<PatternKind> kindsSoFar{patternInfos[0]};
//        for (size_t idx = 1; idx < patternInfos.size(); idx++) {
//            LKFatalError("TODO");
//        }
//    }
    // returns true if all patterns are valid
    // examples of invalid patterns:
    // - `1, x`         (literal and binding)
    // - `X.a, X.b(b)`  (enum cases w/ and w/out binding)
    // - `A.a(x), A.b(y)` (different names)
    // - `x, x`         (same name multiple times)
    // - `A.a(x), A.b(x)` (same name but A.a and A.b have different assoc types
    // - wildcard pattern that is not the sole pattern
    // - wildcard pattern w/ a condition, if there is no other wildcard branch w/out a condition
    
    // TODO implement
    return true;
}


llvm::Value* MatchMaker::run() {
    LKFatalError("");
    LKAssert(VK == RValue && "oof");
    LKAssert(matchExpr->branches.size() > 0);
    
    auto numBranches = matchExpr->branches.size();
    
    targetType = irgen.getType(matchExpr->target);
    util::fmt::print("[{}] targetType: '{}'", __func__, targetType);
    
    // TODO replace this w/ a more sophisticated public method, which can also be used from IRGen.getType
    auto resultType = irgen.getType(matchExpr->branches[0].expr);
    
    fetchPatternKinds();
    if (!validatePatterns()) {
        LKFatalError("");
    }
    
    if (irgen.isTemporary(matchExpr->target)) {
        auto targetExpr = matchExpr->target;
        auto ident = makeIdent(irgen.currentFunction.getTmpIdent(), targetExpr->getSourceLocation());
        auto varDecl = std::make_shared<ast::VarDecl>(ident, nullptr, targetExpr);
        targetV = irgen.codegenVarDecl(varDecl);
    } else {
        targetV = irgen.codegenExpr(matchExpr->target, LValue);
        if (!targetType->isReferenceTy()) {
            targetType = targetType->getReferenceTo(); // TODO is this a good idea? also, shouldn't this happen when we first assign targetType?
        }
    }
    auto targetVRawExpr = std::make_shared<ast::RawLLVMValueExpr>(targetV, targetType);
    targetVRawExpr->setSourceLocation(matchExpr->target->getSourceLocation());
    
    auto addBBAndSetAsInsertPoint = [&](llvm::BasicBlock *BB) {
        auto F = irgen.currentFunction.llvmFunction;
        F->insert(F->end(), BB);
        //F->getBasicBlockList().push_back(BB);
        irgen.builder.SetInsertPoint(BB);
    };
    
    std::map<llvm::BasicBlock *, llvm::Value *> phiValueMappings;
    
    // the basic block after the last branch
    auto mergeBB = llvm::BasicBlock::Create(irgen.C);
    auto nextCondBB = llvm::BasicBlock::Create(irgen.C);
    
    auto createBBs = [&](size_t count) {
        std::vector<llvm::BasicBlock *> BBs;
        BBs.reserve(count);
        for (size_t idx = 0; idx < count; idx++) {
            BBs.push_back(llvm::BasicBlock::Create(irgen.C));
        }
        return BBs;
    };
    
//    size_t totalPatternCount = 0;
//    for (const auto &branch : matchExpr->branches) {
//        totalPatternCount += branch.patterns.size();
//    }
    
    // vector of `(pattern entry BB) x branch value BB` pairs
    std::vector<std::pair<std::vector<llvm::BasicBlock *>, llvm::BasicBlock *>> valueBBMappings;
    valueBBMappings.reserve(numBranches);
    for (size_t idx = 0; idx < numBranches; idx++) {
        valueBBMappings.push_back(std::make_pair(createBBs(matchExpr->branches[idx].patterns.size()), llvm::BasicBlock::Create(irgen.C)));
    }
    
    // TODO get rid of this and just have the first condition be part of the BB containing the match expression
    irgen.builder.CreateBr(nextCondBB);
    
    for (size_t branchIdx = 0; branchIdx < numBranches; branchIdx++) {
        const auto &branch = matchExpr->branches[branchIdx];
        auto numPatterns = branch.patterns.size();
        
        for (size_t patternIdx = 0; patternIdx < numPatterns; patternIdx++) {
            const auto &pattern = branch.patterns[patternIdx];
            auto valueEntryBB = valueBBMappings[branchIdx].first[patternIdx];
            
            auto shouldBranchIntoNextValueBB = [&]() -> bool {
                if (patternIdx < numPatterns - 1) {
                    // there are more patterns in the current branch
                    return false;
                }
                if (branchIdx < numBranches - 2) {
                    // this is not the penultimate branch
                    return false;
                }
                // this assumes that the branches and patterns were validated, and that a wildcard cannot appear among other patterns in a branch
                return patternKinds[numBranches - 1][0] == PatternKind::SimpleNamedBinding
                    && !matchExpr->branches[numBranches - 1].patterns[0].hasCondition();
            };
            
            auto patternKind = patternKinds[branchIdx][patternIdx];
            switch (patternKind) {
                case PatternKind::Literal: {
                    LKAssert(!pattern.hasCondition());
                    
                    addBBAndSetAsInsertPoint(nextCondBB);
                    nextCondBB = llvm::BasicBlock::Create(irgen.C);
                    
                    // literals are implemented using the `==` operator
                    auto cmpBinop = std::make_shared<ast::BinOp>(ast::Operator::EQ, pattern.expr, targetVRawExpr);
                    cmpBinop->setSourceLocation(branch.getSourceLocation());
                    auto cond = irgen.codegenExpr(cmpBinop);
                    
                    // if the next branch is a wildcard (ie, binds to _), branch directly into its value BB
                    irgen.builder.CreateCondBr(cond, valueEntryBB, // TODO why not go into the actual value BB
                                               shouldBranchIntoNextValueBB() ? valueBBMappings[branchIdx + 1].first[0] : nextCondBB);
                    break;
                }
                case PatternKind::SimpleNamedBinding: {
                    // named binding which binds to a value
                    addBBAndSetAsInsertPoint(nextCondBB);
                    nextCondBB = llvm::BasicBlock::Create(irgen.C);
                    
                    if (pattern.hasCondition()) {
                        auto ids = insertPatternBindingsIntoLocalScope(pattern, patternKind);
                        auto cond = irgen.codegenBoolComp(pattern.cond);
                        removePatternBindingsFromLocalScope(ids);
                        irgen.builder.CreateCondBr(cond, valueEntryBB, // TODO why not go into the actual value BB
                                                   shouldBranchIntoNextValueBB() ? valueBBMappings[branchIdx + 1].first[0] : nextCondBB);
                    } else {
                        if (branchIdx == 0) {
                            // if the first branch is a wildcard, we jump directly into the value BB
                            irgen.builder.CreateBr(valueEntryBB); // TODO `valueBBMappings[branchIdx].second`?
                        } else {
                            // if the wildcard is not the first branch, there is nothing to do since the previous pattern
                            // branches into this valueBB, if the pattern didn't match
                        }
                    }
                    break;
                }
            }
        }
        
        for (size_t patternIdx = 0; patternIdx < numPatterns; patternIdx++) {
            auto valueBB = valueBBMappings[branchIdx].second;
            auto patternKind = patternKinds[branchIdx][patternIdx];
            
            switch (patternKind) {
                case PatternKind::Literal:
                    irgen.builder.CreateBr(valueBB);
                    break;
                
                case PatternKind::SimpleNamedBinding: {
                    auto ids = insertPatternBindingsIntoLocalScope(branch.patterns[patternIdx], patternKind);
                }
            }
        }
        
//        addBBAndSetAsInsertPoint(valueBB);
//        phiValueMappings[valueBB] = irgen.codegenExpr(branch.expr);
//        irgen.builder.CreateBr(mergeBB);
    }
    
    addBBAndSetAsInsertPoint(mergeBB);
    
    irgen.emitDebugLocation(matchExpr);
    auto phi = irgen.builder.CreatePHI(irgen.getLLVMType(resultType), phiValueMappings.size());
    for (auto [BB, V] : phiValueMappings) {
        phi->addIncoming(V, BB);
    }
    return phi;
}



MatchMaker::BindingIdsT MatchMaker::insertPatternBindingsIntoLocalScope(const ast::MatchExprPattern &pattern, PatternKind kind) {
    switch (kind) {
        case PatternKind::Literal:
            return {};
        
        case PatternKind::SimpleNamedBinding: {
            auto name = llvm::cast<ast::Ident>(pattern.expr)->value;
            auto binding = ValueBinding(targetType, targetV, [&]() -> llvm::Value* {
                return targetV;
            }, [](llvm::Value *){
                LKFatalError("pattern bindings are read-only");
            }, ValueBinding::Flags::CanRead);
            return { irgen.localScope.insert(name, binding) };
        }
    }
}

void MatchMaker::removePatternBindingsFromLocalScope(const BindingIdsT &ids) {
    irgen.localScope.removeAll(ids);
}

