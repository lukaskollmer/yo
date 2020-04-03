//
//  IRGen+Expr.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-03-06.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "IRGen.h"
#include "Mangling.h"
#include "TemplateSpecialization.h"
#include "lex/Diagnostics.h"
#include "util_llvm.h"
#include "util/llvm_casting.h"
#include "util/MapUtils.h"

#include <string>
#include <vector>
#include <map>
#include <set>
#include <memory>

using namespace yo;
using namespace yo::irgen;
using NK = ast::Node::Kind;


llvm::Value* IRGenerator::codegenRawLLVMValueExpr(std::shared_ptr<ast::RawLLVMValueExpr> rawExpr, ValueKind) {
    return rawExpr->value;
}


llvm::Value* IRGenerator::codegenExprStmt(std::shared_ptr<ast::ExprStmt> exprStmt) {
    auto expr = exprStmt->expr;
    auto type = getType(expr);
    auto ident = currentFunction.getTmpIdent();
    llvm::AllocaInst *alloca = nullptr;
    
    if (!type->isVoidTy()) {
        alloca = builder.CreateAlloca(type->getLLVMType());
        alloca->setName(ident);
        includeInStackDestruction(type, alloca);
    }
    
    auto value = codegenExpr(expr);
    
    if (!type->isVoidTy()) {
        builder.CreateStore(value, alloca);
    }
    return value;
}


llvm::Value* IRGenerator::codegenNumberLiteral(std::shared_ptr<ast::NumberLiteral> numberLiteral, ValueKind VK) {
    using NT = ast::NumberLiteral::NumberType;
    
    LKAssert(VK == RValue);
    emitDebugLocation(numberLiteral);
    
    // TODO add a check that `numberLiteral->value` actually fits in `numberLiteral->type`
    
    switch (numberLiteral->type) {
        case NT::Boolean: {
            return llvm::ConstantInt::get(builtinTypes.llvm.i1, numberLiteral->value);
        }
        case NT::Character: {
            //LKAssert(integerLiteralFitsInIntegralType(numberLiteral->value, builtinTypes.yo.i8));
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



llvm::Value* IRGenerator::codegenStringLiteral(std::shared_ptr<ast::StringLiteral> stringLiteral, ValueKind VK) {
    using SLK = ast::StringLiteral::StringLiteralKind;

    switch (stringLiteral->kind) {
        case SLK::ByteString:
            emitDebugLocation(stringLiteral);
            // TODO insert VK == RValue check here?
            LKAssert(VK == RValue);
            return builder.CreateGlobalStringPtr(stringLiteral->value);
        
        case SLK::NormalString: {
            if (!nominalTypes.contains("String")) {
                diagnostics::emitError(stringLiteral->getSourceLocation(), "unable to find 'String' type");
            }
            auto &loc = stringLiteral->getSourceLocation();
            auto target = makeIdent("String");
            target->setSourceLocation(loc);
            auto callExpr = std::make_shared<ast::CallExpr>(target);
            callExpr->arguments = {
                std::make_shared<ast::RawLLVMValueExpr>(builder.CreateGlobalStringPtr(stringLiteral->value), builtinTypes.yo.i8Ptr)
            };
            callExpr->setSourceLocation(loc);
            callExpr->arguments[0]->setSourceLocation(loc);
            return codegenExpr(callExpr, VK);
        }
    }
}



llvm::Value* IRGenerator::codegenIdent(std::shared_ptr<ast::Ident> ident, ValueKind returnValueKind) {
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


llvm::Value* IRGenerator::codegenCastExpr(std::shared_ptr<ast::CastExpr> castExpr, ValueKind VK) {
    LKAssert(VK == RValue && "TODO: implement?");
    
    using LLVMCastOp = llvm::Instruction::CastOps;
    constexpr auto invalidCastOp = static_cast<LLVMCastOp>(-1); // TODO is -1 a good invalid value?
    
    LLVMCastOp op = invalidCastOp;
    auto srcTy = getType(castExpr->expr);
    auto dstTy = resolveTypeDesc(castExpr->destType);
    auto &DL = module->getDataLayout();
    
    if (srcTy == dstTy) {
        return codegenExpr(castExpr->expr);
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
    return builder.CreateCast(op, codegenExpr(castExpr->expr), dstTy->getLLVMType());
}







llvm::Value* IRGenerator::codegenMemberExpr(std::shared_ptr<ast::MemberExpr> memberExpr, ValueKind returnValueKind, SkipCodegenOption skipCodegenOption, Type **outType) {
    auto skipCodegen = skipCodegenOption == kSkipCodegen;
    
    
    auto setOutType = [&](Type *type) {
        if (outType) *outType = type;
    };
    
    
    if (auto targetIdent = llvm::dyn_cast<ast::Ident>(memberExpr->target)) {
        if (localScope.contains(targetIdent->value)) {
            goto handle_as_expr;
        
        } else if (auto ty = nominalTypes.get(targetIdent->value)) {
            auto type = *ty;
            
            switch (type->getTypeId()) {
                case Type::TypeID::Variant: {
                    LKAssert(returnValueKind == RValue);
                    memberExpr->isKnownAsTemporary = true;
                    auto variantTy = llvm::cast<VariantType>(type);
                    if (!variantTy->hasElement(memberExpr->memberName)) {
                        auto msg = util::fmt::format("variant type '{}' does not contain element '{}'", variantTy, memberExpr->memberName);
                        diagnostics::emitError(memberExpr->getSourceLocation(), msg);
                    }
                    
                    if (variantTy->elementHasAssociatedData(memberExpr->memberName)) {
                        // TODO fetch the function generated for constructing this element
                        LKFatalError("");
                    } else {
                        setOutType(variantTy);
                        if (skipCodegen) {
                            return nullptr;
                        } else {
                            emitDebugLocation(memberExpr);
                            return constructVariant(variantTy, memberExpr->memberName);
                        }
                    }
                    
                    break;
                }
                case Type::TypeID::Struct:
                case Type::TypeID::Pointer:
                case Type::TypeID::Numerical:
                case Type::TypeID::Reference:
                case Type::TypeID::Void:
                case Type::TypeID::Function:
                case Type::TypeID::Tuple:
                    LKFatalError("TODO");
            }
            LKFatalError("TODO");
        
        } else {
            // targetIdent is not a type, but also not in the local scope
            auto msg = util::fmt::format("unable to resolve '{}'", targetIdent->value);
            diagnostics::emitError(targetIdent->getSourceLocation(), msg);
        }
    
    } else {
        // target is not an identifier
        goto handle_as_expr;
    }
    
// the member expr target is an expression
handle_as_expr:
    
    const auto targetTy = getType(memberExpr->target);

    StructType *structTy = nullptr;
    const bool needsLoad = targetTy->isPointerTy() || targetTy->isReferenceTy();

    structTy = getUnderlyingStruct(targetTy);

    if (!structTy) {
        auto msg = util::fmt::format("invalid member expr base type: '{}'", targetTy);
        diagnostics::emitError(memberExpr->getSourceLocation(), msg);
    }

    const auto [memberIndex, memberType] = structTy->getMember(memberExpr->memberName);
    if (!memberType) {
        auto msg = util::fmt::format("unable to find member '{}' in type '{}'", memberExpr->memberName, structTy);
        diagnostics::emitError(memberExpr->getSourceLocation(), msg);
    }
    
    setOutType(memberType);
    if (skipCodegen) {
        return nullptr;
    }
    
    llvm::Value *offsets[] = {
        llvm::ConstantInt::get(builtinTypes.llvm.i32, 0),
        llvm::ConstantInt::get(builtinTypes.llvm.i32, memberIndex)
    };
    
    auto targetV = codegenExpr(memberExpr->target, LValue, /*insertImplicitLoadInst*/ false);
    
    if (isTemporary(memberExpr->target)) {
        LKAssert(!needsLoad);
        // TODO is there a situation in which we're dealing w/ a temporary (which as such will be included in stack destruction) that also needs a load?
        // Update: come to think of it, all types that need a load (references and pointers) wouldn't participate in stack cldanup anyway:
        // - pointer: we dont destruct pointers
        // - reference: same
        
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




llvm::Value* IRGenerator::codegenSubscriptExpr(std::shared_ptr<ast::SubscriptExpr> subscript, ValueKind returnValueKind) {
    auto TT = getType(subscript->target);
    auto OT = getType(subscript->offset);
    
    if (!TT->isPointerTy()) {
        if (!typeIsSubscriptable(TT)) {
            auto msg = util::fmt::format("cannot subscript value of type '{}'", TT);
            diagnostics::emitError(subscript->target->getSourceLocation(), msg);
        }
        
        if (TT->isTupleTy() || (TT->isReferenceTy() && llvm::cast<ReferenceType>(TT)->getReferencedType()->isTupleTy())) {
            size_t index = llvm::cast<ast::NumberLiteral>(subscript->offset)->value;
            auto memberName = formatTupleMemberAtIndex(index)->value;
            auto memberExpr = std::make_shared<ast::MemberExpr>(subscript->target, memberName);
            memberExpr->setSourceLocation(subscript->getSourceLocation());
            return codegenExpr(memberExpr, returnValueKind);
        } else {
            subscript->isKnownAsTemporary = true;
            return codegenExpr(subscriptExprToCall(subscript), returnValueKind);
        }
    }
    LKAssert(TT->isPointerTy());
    
    if (TT->isPointerTy() && (!OT->isNumericalTy() || !llvm::dyn_cast<NumericalType>(OT)->isIntegerTy())) {
        auto msg = util::fmt::format("expected integral type, got '{}'", OT);
        diagnostics::emitError(subscript->offset->getSourceLocation(), msg);
    }
    
    auto target = codegenExpr(subscript->target);
    auto offset = codegenExpr(subscript->offset);
    
    emitDebugLocation(subscript);
    auto GEP = builder.CreateGEP(target, offset);
    
    switch (returnValueKind) {
        case LValue: return GEP;
        case RValue: return builder.CreateLoad(GEP);
    }
}



bool isValidUnaryOpLogicalNegType(Type *ty) {
    if (ty == Type::getBoolType() || ty->isPointerTy()) {
        return true;
    }
    
    if (auto numTy = llvm::dyn_cast<NumericalType>(ty)) {
        return numTy->isIntegerTy();
    }
    
    return false;
}


llvm::Value* IRGenerator::codegenUnaryExpr(std::shared_ptr<ast::UnaryExpr> unaryExpr, ValueKind VK) {
    LKAssert(VK == RValue && "TODO: implement");
    emitDebugLocation(unaryExpr);
    
    auto expr = unaryExpr->expr;
    
    switch (unaryExpr->op) {
        case ast::UnaryExpr::Operation::Negate:
            return builder.CreateNeg(codegenExpr(expr));
            
        case ast::UnaryExpr::Operation::BitwiseNot:
            return builder.CreateNot(codegenExpr(expr));
            
        case ast::UnaryExpr::Operation::LogicalNegation: {
            auto ty = getType(expr);
            if (!isValidUnaryOpLogicalNegType(ty)) {
                auto msg = util::fmt::format("type '{}' cannpt be used in logical negation", ty);
                diagnostics::emitError(unaryExpr->getSourceLocation(), msg);
            }
            auto V = codegenExpr(expr);
            emitDebugLocation(unaryExpr);
            return builder.CreateIsNull(V); // TODO this seems like a cop-out answer?
        }
        
        case ast::UnaryExpr::Operation::AddressOf: {
            if (isTemporary(expr)) {
                diagnostics::emitError(unaryExpr->getSourceLocation(), "can't take address of temporary");
            }
            return codegenExpr(expr, LValue);
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
    
    auto targetTy = info.targetType;
    auto patternExpr = info.patternExpr;
    
    util::fmt::print("targetTy: {}", targetTy);
    
    if (targetTy->isNumericalTy()) {
        if (auto numberLiteral = llvm::dyn_cast<ast::NumberLiteral>(patternExpr)) {
            if (valueIsTriviallyConvertible(numberLiteral, targetTy)) {
                auto cmp = std::make_shared<ast::BinOp>(ast::Operator::EQ,
                                                        std::make_shared<ast::RawLLVMValueExpr>(info.targetLLVMValue, targetTy),
                                                        numberLiteral);
                cmp->setSourceLocation(numberLiteral->getSourceLocation());
                return codegenExpr(cmp);
            }
        } else {
            LKFatalError("TODO");
//            diagnostics::emitError(PE->getSourceLocation(), util::fmt::format( "cannot match value of type '{}' against '{}'", TT, PT));
        }
    }
    
    if (auto memberExpr = llvm::dyn_cast<ast::MemberExpr>(patternExpr)) {
        auto x = NameLookup(*this).lookup(memberExpr);
        for (auto e : x) {
            util::fmt::print("LOOKUP RESULT: {}", e);
        }
        LKFatalError("");
    }
    
    
    diagnostics::emitError(patternExpr->getSourceLocation(), "not a valid match pattern");
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
llvm::Value* IRGenerator::codegenMatchExpr(std::shared_ptr<ast::MatchExpr> matchExpr, ValueKind VK) {
    LKAssert(VK == RValue && "TODO: implement");
    emitDebugLocation(matchExpr);
    
    // TODO require that match patterns cannot contain side effects? (this should go in _IsValidMatchPatternForMatchedExprType!)
    auto F = currentFunction.llvmFunction;
    auto matchedExprType = getType(matchExpr->target);
    auto resultType = getType(matchExpr->branches.front().expression);
    auto matchTargetValue = codegenExpr(matchExpr->target);
    
    
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
        
//        Type *_initialTy = nullptr;
//        if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary(branch.expression, resultType, &_initialTy)) {
//            //diagnostics::emitError(branch.expression->getSourceLocation(),
//            //                           "Invalud match branch pattern value: Type '%s' not compatible with expected type '%s'",
//            //                           _initialTy->str().c_str(), resultType->str().c_str());
//            LKFatalError("Invalid match branch result value: Type %s not compatible with expected type %s",
//                         _initialTy->str_desc().c_str(), resultType->str_desc().c_str());
//        }
        
        if (!applyImplicitConversionIfNecessary(branch.expression, resultType)) {
            LKFatalError("ugh");
        }
        
        
        F->getBasicBlockList().push_back(valueBB);
        builder.SetInsertPoint(valueBB);
        branchMappings[valueBB] = codegenExpr(branch.expression);
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




llvm::Value* IRGenerator::codegenArrayLiteralExpr(std::shared_ptr<ast::ArrayLiteralExpr> arrayLiteral, ValueKind VK) {
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
    SD->attributes.int_isSynthesized = true;
    SD->name = util::fmt::format("__tuple_{}", mangling::mangleFullyResolved(tupleTy));
    
    for (int64_t idx = 0; idx < tupleTy->memberCount(); idx++) {
        auto name = formatTupleMemberAtIndex(idx);
        auto type = ast::TypeDesc::makeResolved(tupleTy->getMembers()[idx]);
        SD->members.push_back(std::make_shared<ast::VarDecl>(name, type));
    }
    
    auto ST = withCleanSlate(*this, [this, SD] { return addAndRegisterStructDecl(SD); });
    ST->setFlag(Type::Flags::IsSynthesized);
    tupleTy->setUnderlyingStructType(ST);
    return ST;
}



llvm::Value* IRGenerator::codegenTupleExpr(std::shared_ptr<ast::TupleExpr> tupleExpr, ValueKind VK) {
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



llvm::Value* IRGenerator::codegenBinOp(std::shared_ptr<ast::BinOp> binop, ValueKind VK) {
    LKAssert(VK == RValue && "TODO: implement");
    
    if (!isValidBinopOperator(binop->getOperator())) {
        diagnostics::emitError(binop->getSourceLocation(), "not a valid binary operator");
    }
    
    auto callExpr = std::make_shared<ast::CallExpr>(makeIdent(mangling::mangleCanonicalName(binop->getOperator())),
                                                    std::vector<std::shared_ptr<ast::Expr>> { binop->getLhs(), binop->getRhs() });
    callExpr->setSourceLocation(binop->getSourceLocation());
    return codegenExpr(callExpr);
    
}





#pragma mark - lambdas


llvm::Value* IRGenerator::codegenLambdaExpr(std::shared_ptr<ast::LambdaExpr> lambdaExpr, ValueKind VK) {
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
    SD->attributes.int_isSynthesized = true;
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
    
    auto ST = withCleanSlate(*this, [&] { return addAndRegisterStructDecl(SD); });
    ST->setFlag(Type::Flags::IsSynthesized);
    lambdaExpr->_structType = ST;
    
    auto &sig = lambdaExpr->signature;
//    resolveTypeDesc(sig.returnType); // TODO is this necessary? (probably yes, bc we want the lambda to pick up the types declated in its scope?)
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
    
    addAndRegisterFunction(imp);
    
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



// TODO this should somehow return an error message explaining why the template deduction failed (eg "too many template arguments", etc)
std::optional<TemplateTypeMapping>
IRGenerator::attemptToResolveTemplateArgumentTypesForCall(const ast::FunctionSignature &signature, std::shared_ptr<ast::CallExpr> call, const std::vector<std::pair<Type *, std::shared_ptr<ast::Expr>>>& args) {
    using TDK = ast::TypeDesc::Kind;
    
    LKAssert(signature.isTemplateDecl());
    
    if (signature.numberOfParameters() != args.size()) {
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
        mapping[param.name->value] = { resolveTypeDesc(call->explicitTemplateArgs->elements[idx], false), DeductionReason::Explicit };
    }
    
    
    // Attempt to resolve template parameters from call arguments
    
    
    // retval: true -> success, false -> failure
    auto imp = [&](std::string name, Type *ty, uint64_t argIdx) -> bool {
        auto &arg = args[argIdx].second;
        auto reason = arg->isOfKind(NK::NumberLiteral) ? DR::Literal : DR::Expr;
        
        if (!util::map::has_key(mapping, name)) {
            mapping[name] = { ty, reason };
            return true;
        }
        
        auto &prevDeduction = mapping.at(name);
        
        if (prevDeduction.type == ty) {
            return true;
        
        } else if (prevDeduction.reason != reason && reason == DeductionReason::Literal) {
            // Prev wasn't a literal, but current is
            auto argExpr = llvm::dyn_cast<ast::NumberLiteral>(arg);
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
            
            case TDK::Tuple: {
                auto tupleTy = llvm::dyn_cast<TupleType>(ty);
                if (!tupleTy || tupleTy->memberCount() != typeDesc->getTupleMembers().size()) {
                    return false;
                }
                for (size_t idx = 0; idx < typeDesc->getTupleMembers().size(); idx++) {
                    if (!handle(typeDesc->getTupleMembers().at(idx), tupleTy->getMembers().at(idx), argIdx)) {
                        return false;
                    }
                }
                return true;
            }
            
            case TDK::Function:
            case TDK::Decltype:
            case TDK::Resolved:
                LKFatalError("TODO");
        }
    };
    
    
    for (size_t idx = 0; idx < args.size(); idx++) {
        auto sigTypeDesc = signature.paramTypes[idx];
        auto argTy = args[idx].first;
        
        if (llvm::isa<ReferenceType>(argTy)) {
            argTy = llvm::dyn_cast<ReferenceType>(argTy)->getReferencedType();
        }
        
        if (!handle(sigTypeDesc, argTy, idx)) {
            return std::nullopt;
        }
    }
    
    
    // Make sure all parameters were resolved, apply default values where necessary
    
    for (auto &param : signature.templateParamsDecl->getParams()) {
        if (util::map::has_key(mapping, param.name->value)) {
            continue;
        }
        
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
            mapping[param.name->value] = { resolveTypeDesc(defaultTy, false), DR::Default };
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




ResolvedCallable IRGenerator::specializeTemplateFunctionDeclForCallExpr(std::shared_ptr<ast::FunctionDecl> funcDecl, TemplateTypeMapping templateArgMapping, bool hasImplicitSelfArg, SkipCodegenOption codegenOption) {
    auto specializedDecl = TemplateSpecializer::specializeWithMapping(funcDecl, templateArgMapping);
    
//    std::vector<Type *> templateArgTypes;
//    for (const auto &param : funcDecl->getSignature().templateParamsDecl->getParams()) {
//        // TODO does this take default arg values into account?
//        templateArgTypes.push_back(resolveTypeDesc(templateArgMapping.at(param.name->value)));
//    }
//    // TODO use this assert to check whether all params (incl defaults) are taken into account here!!!!
//    LKAssert(templateArgTypes.size() == funcDecl->getSignature().templateParamsDecl->size());
//    specializedDecl->setResolvedTemplateArgTypes(templateArgTypes);
    
    
    // Avoid generating the same specialization multiple times
    // In theory, we should never end up here since the call target resolution code should prefer the already-specialized version
    // over re-instantiating the template. However, the code is not very good and cannot (yet?) do that
    
    // We need the function's types fully resolved for the `mangleFullyResolved` call below
    // TODO is withCleanSlate overkill here? we really just need the local scope to be empty so that we can insert the parameters
    // Not entirely true though, we also call resolveTypeDesc which otherwise might include local types defined only for the the scope of the current function
    withCleanSlate(*this, [&]() -> void {
        for (size_t i = 0; i < specializedDecl->getSignature().numberOfParameters(); i++) {
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
            return *decl;
        }
    }
    
    llvm::Function *llvmFunction = nullptr;
    if (codegenOption == kRunCodegen && !specializedDecl->getAttributes().intrinsic) {
        specializedDecl->getAttributes().int_isDelayed = false; // TODO what does this do?
        llvmFunction = addAndRegisterFunction(specializedDecl);
    }
    return ResolvedCallable(specializedDecl, llvmFunction, hasImplicitSelfArg);
}





bool IRGenerator::isImplicitConversionAvailable(Type *src, Type *dst) {
    LKFatalError("implement");
}





/// This function assumes that both targets match the call (ie, are valid targets)
CandidateViabilityComparisonResult FunctionCallTargetCandidate::compare(IRGenerator &irgen, const FunctionCallTargetCandidate &other, const std::vector<std::pair<Type *, std::shared_ptr<ast::Expr>>> &args) const {
    const auto &lhs = *this;
    const auto &rhs = other;
    
    LKAssert(lhs.target.hasImplicitSelfArg == rhs.target.hasImplicitSelfArg);
    
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
    
    
    for (uint32_t idx = lhs.target.hasImplicitSelfArg; idx < lhs.getSignature().numberOfParameters(); idx++) {
        auto argTy = args[idx].first;
        auto lhsTy = lhsParamTys[idx];
        auto rhsTy = rhsParamTys[idx];
        
//        util::fmt::print("[{}] act: {}, exp(L): {}, exp(R): {}", idx, argTy, lhsTy, rhsTy);
        
        if (argTy == lhsTy && argTy == rhsTy) {
            continue;
        } else if (args[idx].second->isOfKind(NK::NumberLiteral)) {
            util::fmt::print("lhs: {}", lhs.getSignature());
            util::fmt::print("rhs: {}", rhs.getSignature());
            diagnostics::emitNote(lhs.target.funcDecl->getSourceLocation(), "");
            diagnostics::emitNote(rhs.target.funcDecl->getSourceLocation(), "");
            diagnostics::emitNote(args[idx].second->getSourceLocation(), "");
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
        
        
        auto imp = [&](const FunctionCallTargetCandidate &lhs, const FunctionCallTargetCandidate &rhs) -> bool {
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
                //auto type = Type::createTemporary(mangling::mangleAsStruct(name));
                auto type = Type::createTemporary(name);
                typeDesc->setResolvedType(type);
                tmpTypes.emplace_back(type, irgen.nominalTypes.insert(name, type));
            }
            
            auto argTys = util::vector::map(specSig.paramTypes, [&irgen](auto TD) {
                return irgen.resolveTypeDesc(TD, false);
            });
            
            auto tmpCallExpr = std::make_shared<ast::CallExpr>(nullptr);
            auto args = util::vector::map(argTys, [](Type *type) -> std::pair<Type *, std::shared_ptr<ast::Expr>> {
                return std::make_pair(type, std::make_shared<ast::RawLLVMValueExpr>(nullptr, type));
            });
            
            auto deduction = irgen.attemptToResolveTemplateArgumentTypesForCall(rhs.getSignature(), tmpCallExpr, args);
            
            std::function<bool(Type *, std::set<Type *> &)> collectTempTypes;
            collectTempTypes = [&](Type *type, std::set<Type *> &found) -> bool {
                bool retval = false;
                
                if (type->hasFlag(Type::Flags::IsTemporary)) {
                    found.insert(type);
                    retval = true;
                }
                
                switch (type->getTypeId()) {
                    case Type::TypeID::Void:
                    case Type::TypeID::Numerical:
                    case Type::TypeID::Variant:
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
            
        LKFatalError("unreachable?");
    }
    
    return CandidateViabilityComparisonResult::LessViable;
}





ResolvedCallable IRGenerator::resolveCall(std::shared_ptr<ast::CallExpr> callExpr, SkipCodegenOption codegenOption) {
    std::vector<FunctionCallTargetCandidate> candidates;
    std::vector<CallTargetRejectionReason> rejections;
    ResolveCallResultStatus result;
    
    auto target = resolveCall_imp(callExpr, codegenOption, candidates, rejections, result, false);
    
    switch (result) {
        case ResolveCallResultStatus::Success:
            return *target;
        
        case ResolveCallResultStatus::NoCandidates: {
            for (const auto &rejection : rejections) {
                diagnostics::emitNote(rejection.decl->getSourceLocation(), util::fmt::format("[{}] not viable: {}", rejection.decl->signature, rejection.reason));
            }
//            std::cout << callExpr->description() << std::endl;
            diagnostics::emitError(callExpr->getSourceLocation(), util::fmt::format("unable to resolve call"));
        }
        
        case ResolveCallResultStatus::AmbiguousCandidates: {
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
            diagnostics::emitError(callExpr->getSourceLocation(), "ambiguous call");
        }
    }
}



std::optional<ResolvedCallable>
IRGenerator::resolveCall_opt(std::shared_ptr<ast::CallExpr> callExpr, SkipCodegenOption codegenOption) {
    std::vector<FunctionCallTargetCandidate> candidates;
    std::vector<CallTargetRejectionReason> rejections;
    ResolveCallResultStatus status;
    return resolveCall_imp(callExpr, kSkipCodegen, candidates, rejections, status, codegenOption);
}



// This function will only return if the call can be resolved
// TODO add a string& parameter "descriptive target name", which then can be used in error messages like "cant resolve call to {}" (eg "operator +", etc)
std::optional<ResolvedCallable>
IRGenerator::resolveCall_imp(std::shared_ptr<ast::CallExpr> callExpr, SkipCodegenOption codegenOption, std::vector<FunctionCallTargetCandidate> &candidates, std::vector<CallTargetRejectionReason> &rejections, ResolveCallResultStatus &resultStatus, bool lookingForZeroResults) {
    // TODO this function is rather long, refactor!!!
    
    const bool skipCodegen = codegenOption == kSkipCodegen;
    std::string targetName;
    
    
//    struct PotentialCallTarget {
//        //
//    };
    
    
    
    std::vector<ResolvedCallable> potentialTargets;
    std::vector<std::pair<Type *, std::shared_ptr<ast::Expr>>> allArgs;
    bool didAddImplicitSelfArgToArgsList = false;
    
    auto addImplicitFstArg = [&](std::shared_ptr<ast::Expr> expr) {
        if (didAddImplicitSelfArgToArgsList) {
            return;
        }
        LKAssert(allArgs.empty());
        allArgs.push_back(std::make_pair(getType(expr), expr));
        didAddImplicitSelfArgToArgsList = true;
    };
    
    
    auto getRC = [&](const std::shared_ptr<ast::FunctionDecl> &decl) -> ResolvedCallable& {
        // TODO this is pretty bad!!!!!
        // do we really need the functions and resolvedFunctions maps?
        auto imp = [&]() -> std::optional<std::reference_wrapper<ResolvedCallable>> {
            for (auto &[name, RCs] : functions) {
                for (auto &RC : RCs) {
                    if (RC.funcDecl.get() == decl.get()) {
                        return RC;
                    }
                }
            }
            return std::nullopt;
        };
        
        if (auto RC = imp()) {
            return RC->get();
        } else {
            registerNamedDecls(mangling::mangleCanonicalName(decl), [](const std::shared_ptr<ast::TopLevelStmt> &decl) -> bool {
                return decl->isOfKind(NK::FunctionDecl);
            });
            if (auto RC = imp()) {
                return RC->get();
            }
        }
        LKFatalError("");
    };
    
    // assuming that `expr` is a call target w/ an implicit self argument, returns that self argument
    auto getImplicitSelfArg = [](std::shared_ptr<ast::Expr> expr) -> std::shared_ptr<ast::Expr> {
        if (expr->isOfKind(NK::Ident)) {
            return expr;
        } else if (auto memberExpr = llvm::dyn_cast<ast::MemberExpr>(expr)) {
            // TODO what about a call to a member object's call operator?
            // in this case, the full member expr is the call target
            return memberExpr->target;
        } else {
            LKFatalError("");
        }
    };
    
    
//    util::fmt::print("CALL TARGET NAME LOOKUP TARGETS:");
    for (auto result : NameLookup(*this).lookup(callExpr->target)) {
//        util::fmt::print("- {}", result);
        switch (result.kind) {
            case ValueInfo::Kind::Function: {
                auto &[selfTy, funcDecl] = result.getFunctionInfo();
                
                switch (funcDecl->funcKind) {
                    case ast::FunctionKind::GlobalFunction: {
                        LKAssert(selfTy == nullptr);
                        potentialTargets.push_back(getRC(funcDecl));
                        break;
                    }
                    case ast::FunctionKind::StaticMethod:
                    case ast::FunctionKind::InstanceMethod: {
                        std::shared_ptr<ast::Expr> implicitArg;
                        if (funcDecl->isOfFunctionKind(ast::FunctionKind::InstanceMethod)) {
                            implicitArg = getImplicitSelfArg(callExpr->target); // TODO this wont work for overloaded operators ?!
                        } else {
                            implicitArg = std::make_shared<ast::RawLLVMValueExpr>(nullptr, selfTy);
                            implicitArg->setSourceLocation(callExpr->getSourceLocation());
                        }
                        addImplicitFstArg(implicitArg);
                        potentialTargets.push_back(getRC(funcDecl));
                        break;
                    }
                }
                break;
            }
            
            case ValueInfo::Kind::LocalVar:
            case ValueInfo::Kind::Property:
                LKFatalError("");
            
            case ValueInfo::Kind::TypeRef: {
                // call target resolves to a type -> ctor call?
                auto type = result.getTypeRef();
                if (auto structTy = llvm::dyn_cast<StructType>(type)) {
                    if (auto type = util::map::get_opt(namedDeclInfos, structTy->getName())) {
                        auto ctorName = mangling::mangleCanonicalName(ast::FunctionKind::StaticMethod, structTy->getName());
                        if (auto ctorCandidates = util::map::get_opt(functions, ctorName)) {
                            auto ctor = util::vector::first_where(*ctorCandidates, [](const ResolvedCallable &RC) {
                                return RC.funcDecl->getAttributes().int_isCtor;
                            });
                            if (ctor.has_value()) {
                                resultStatus = ResolveCallResultStatus::Success;
                            }
                            return ctor;
                        } else {
                            LKFatalError("unable to find compiler-generated constructor decl");
                        }
                        LKFatalError("");
                    } else {
                        LKFatalError("");
                    }
                }
                LKFatalError("");
            }
            
            case ValueInfo::Kind::TypeRefTmpl: {
                auto &decl = result.getTypeRefTmplDecl();
                
                switch (decl->getKind()) {
                    case NK::StructDecl: {
                        auto structDecl = llvm::cast<ast::StructDecl>(decl);
                        auto ctorTargetName = mangling::mangleCanonicalName(ast::FunctionKind::StaticMethod, structDecl->getName());
                        const auto &targets = functions[ctorTargetName];
                        LKAssert(targets.size() == 1);
                        auto &target = targets[0];

                        LKAssert(target.signature.isTemplateDecl() && target.funcDecl->getSignature().isTemplateDecl());
                        LKAssert(target.funcDecl->getAttributes().int_isCtor);
                        auto tmplMapping = resolveStructTemplateParametersFromExplicitTemplateArgumentList(structDecl, callExpr->explicitTemplateArgs);
                        auto specDecl = specializeTemplateFunctionDeclForCallExpr(target.funcDecl, tmplMapping, 0, kRunCodegen); // since this is a ctor, there won't be any codegen, it'll just register the function
                        resultStatus = ResolveCallResultStatus::Success;
                        return specDecl;
                    }
                    case NK::TypealiasDecl:
                    case NK::VariantDecl:
                        LKFatalError("");
                    
                    default:
                        LKFatalError("");
                }
            }
        }
    }
    
    
    const auto &possibleTargets = potentialTargets;

    // find a matching target
    
    
    if (//lookingForZeroResults &&
        possibleTargets.empty()) {
        resultStatus = ResolveCallResultStatus::NoCandidates;
        return std::nullopt;
    }
    
//    LKFatalError("");
    
    
    util::Counter<> candidateIndexCounter; // used to give each candidate a unique id (unique for the scope of this call resolution)
    
    for (auto argExpr : callExpr->arguments) {
        allArgs.emplace_back(getType(argExpr), argExpr);
    }
    
    
    for (const auto &target : possibleTargets) {
        auto &decl = target.funcDecl;
        auto &sig = decl->getSignature();
        bool isVariadicWithCLinkage = sig.isVariadic && target.funcDecl->getAttributes().extern_;
        LKAssertImplication(target.hasImplicitSelfArg, didAddImplicitSelfArgToArgsList);
        LKAssert(target.hasImplicitSelfArg == didAddImplicitSelfArgToArgsList);
        
        //auto args = std::vector(allArgs.begin() + (target.hasImplicitSelfArg ? 0 : (didAddImplicitSelfArgToArgsList)), allArgs.end());
        auto args = std::vector(target.hasImplicitSelfArg ? allArgs.begin() : (allArgs.begin() + didAddImplicitSelfArgToArgsList), allArgs.end());
        
        if (decl->getSignature().isInstantiatedTemplateDecl()) {
            continue;
        }
        
        if (!sig.isVariadic && args.size() != sig.numberOfParameters()) {
            rejections.emplace_back("argument count mismatch", decl);
            continue;
        }
        if (sig.isVariadic && (args.size() < sig.numberOfParameters() - !isVariadicWithCLinkage)) {
            rejections.emplace_back("variadic arguments count mismatch", decl);
            continue;
        }
        if (decl->getName() != kInitializerMethodName && !sig.isTemplateDecl() && callExpr->numberOfExplicitTemplateArgs() > 0) {
            // Reject a non-template target if the call expression contains explicit template arguments.
            // The sole exception here are calls to initializers, since in this case the call expression
            // is being reused by the compiler, with the original target (the ctor) replaced w/ the initializer
            if (!decl->getSignature().isInstantiatedTemplateDecl()) {
                rejections.emplace_back("cannot pass explicit template arguments to non-template target", decl);
            }
            continue;
        }
        
        // extern variadic functions are treated as having C linkage and therefore allowed any variadic arguments
        // another important distinction is that for functions w/ C linkage, the variadic parameter cannot be omitted.
        // for example, printf(*i8...) cannot be called w/ zero arguments, since that would also leave out the format string

        FunctionCallTargetCandidate candidate(candidateIndexCounter.increment(), target);
        size_t lastTypecheckedArgument = isVariadicWithCLinkage ? sig.numberOfParameters() : args.size();
        
        std::vector<decltype(nominalTypes)::ID> tempTypeIds;
        
        if (sig.isTemplateDecl()) {
            if (auto mapping = attemptToResolveTemplateArgumentTypesForCall(decl->getSignature(), callExpr, args)) {
                candidate.templateArgumentMapping = *mapping;
                for (const auto &[name, typeDesc] : *mapping) {
                    LKAssert(typeDesc->isResolved());
                    tempTypeIds.push_back(nominalTypes.insert(name, typeDesc->getResolvedType()));
                }
            } else {
                rejections.emplace_back("unable to resolve template parameter types", decl);
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
            auto [argTy, arg] = args[idx];
            Type *expectedTy = nullptr;

            if (idx < sig.numberOfParameters()) {
                expectedTy = resolveTypeDesc(sig.paramTypes[idx], false);
            } else {
                LKFatalError("is this non-C-linkage varargs?");
            }

            LKAssert(expectedTy);
            
            if (argTy == expectedTy) {
                continue;
            }
            
            if (arg->isOfKind(NK::NumberLiteral)) {
                auto numberLiteral = llvm::cast<ast::NumberLiteral>(arg);
                if (valueIsTriviallyConvertible(numberLiteral, expectedTy)) {
                    candidate.implicitConversionArgumentIndices.push_back(idx);
                    continue;
                } else {
                    auto str = util::fmt::format("argument #{} not trivially convertible from '{}' to '{}'", idx, argTy, expectedTy);
                    rejections.emplace_back(str, decl);
                    goto discard_potential_match;
                }
            }
            
            if (expectedTy->isReferenceTy()) {
                if (!argTy->isReferenceTy() && argTy->getReferenceTo() == expectedTy) {
                    // argument: `T`, expected: `&T`
                    if (!isTemporary(arg) || (idx == 0 && target.hasImplicitSelfArg)) {
                        // the argument is either:
                        // - not a temporary (meaning we can turn it into a reference)
                        // - a temporary, but this is an instance method call and we're dealing with the implicit self parameter
                        continue;
                    }
                }
                // TODO  we reach here bc we're passing an arg that cannot become an lvalue to a function expecting an lvalue reference, add that to the reason!
            } else if (!expectedTy->isReferenceTy() && argTy->isReferenceTy()) {
                if (llvm::dyn_cast<ReferenceType>(argTy)->getReferencedType() == expectedTy) {
                    continue;
                }
            }
            
            // TODO if this is an instance method call, and the target (clearly) belongs to another type, don't add it to the list of rejected targets
            // obviously this only applies if the non-matching argument is the 1st one
            rejections.emplace_back(util::fmt::format("argument #{} of type '{}' incompatible with expected type '{}'", idx, argTy, expectedTy), decl);
            goto discard_potential_match;
        }
        
        candidates.push_back(candidate);
        
    discard_potential_match:
        nominalTypes.removeAll(tempTypeIds);
    }
    
    
    std::map<FunctionCallTargetCandidate::ID, std::vector<FunctionCallTargetCandidate::ID>> ambiguousCandidates;
    
    std::sort(candidates.begin(), candidates.end(), [&](const FunctionCallTargetCandidate &lhs, const FunctionCallTargetCandidate &rhs) -> bool {
        switch (lhs.compare(*this, rhs, allArgs)) {
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
        resultStatus = ResolveCallResultStatus::NoCandidates;
        return std::nullopt;
    }
    
    
    if (candidates.size() > 1 && !ambiguousCandidates[candidates[0].id].empty()) {
        resultStatus = ResolveCallResultStatus::AmbiguousCandidates;
        return std::nullopt;
    }
    
    
    
    auto bestMatch = candidates.front();
    
    if (auto &attr = bestMatch.target.funcDecl->getAttributes();
        !skipCodegen && attr.int_isDelayed && !bestMatch.getSignature().isTemplateDecl())
    {
        attr.int_isDelayed = false;
        EmptyScopeHandle ESH(*this);
        codegenFunctionDecl(bestMatch.target.funcDecl);
    }
    
    
    if (bestMatch.getSignature().isTemplateDecl() && !bestMatch.target.llvmValue) {
        resultStatus = ResolveCallResultStatus::Success;
        return specializeTemplateFunctionDeclForCallExpr(bestMatch.target.funcDecl, bestMatch.templateArgumentMapping, bestMatch.target.hasImplicitSelfArg, codegenOption);
    }
    
    resultStatus = ResolveCallResultStatus::Success;
    return bestMatch.target;
}








bool callerCalleeSideEffectsCompatible(const std::vector<yo::attributes::SideEffect> &callerSideEffects,
                                       const std::vector<yo::attributes::SideEffect> &calleeSideEffects) {
    if (callerSideEffects.size() == 1 && callerSideEffects[0] == yo::attributes::SideEffect::Unknown) {
        return true;
    }
    
    for (const auto& sideEffect : calleeSideEffects) {
        if (!util::vector::contains(callerSideEffects, sideEffect)) {
            return false;
        }
    }
    
    return true;
}


llvm::Value* IRGenerator::codegenCallExpr(std::shared_ptr<ast::CallExpr> call, ValueKind VK) {
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
    std::map<size_t, ArgumentHandlingPolicy> argumentHandlingPolicies;
    // vector<> X(resolvedTarget.signature.numberOfParameters())
    
    for (size_t i = resolvedTarget.hasImplicitSelfArg; i < resolvedTarget.signature.numberOfParameters(); i++) {
        auto expectedType = resolveTypeDesc(resolvedTarget.signature.paramTypes[i]);
        auto expr = call->arguments[i - resolvedTarget.hasImplicitSelfArg];
        argumentHandlingPolicies[i] = ArgumentHandlingPolicy::PassByValue;
        if (!applyImplicitConversionIfNecessary(expr, expectedType)) {
            auto exprTy = getType(expr);
            if (expectedType->isReferenceTy() && !exprTy->isReferenceTy() && exprTy->getReferenceTo() == expectedType) {
                argumentHandlingPolicies[i] = ArgumentHandlingPolicy::PassByReference;
                goto cont;
            } else if (!expectedType->isReferenceTy() && exprTy->isReferenceTy() && expectedType->getReferenceTo() == exprTy) {
                argumentHandlingPolicies[i] = ArgumentHandlingPolicy::PassByValue_ExtractReference;
                goto cont;
            }
            
            auto msg = util::fmt::format("incompatible type for argument #{}. Expected '{}', got '{}'", i, expectedType, exprTy);
            diagnostics::emitError(expr->getSourceLocation(), msg);
        }
        cont:
        // TODO is modifying the arguments in-place necessarily a good idea?
        call->arguments[i - resolvedTarget.hasImplicitSelfArg] = expr;
    }
    
    if (resolvedTarget.funcDecl && resolvedTarget.funcDecl->getAttributes().intrinsic) {
        emitDebugLocation(call);
        return codegen_HandleIntrinsic(resolvedTarget.funcDecl, call);
    }
    
    llvm::Value *llvmFunction = resolvedTarget.llvmValue;
    LKAssert(llvmFunction->getType()->isPointerTy() && llvmFunction->getType()->getContainedType(0)->isFunctionTy());
    auto llvmFunctionTy = llvm::dyn_cast<llvm::FunctionType>(llvmFunction->getType()->getContainedType(0));
    auto isVariadic = llvmFunctionTy->isVarArg();
    
    LKAssert(call->arguments.size() >= llvmFunctionTy->getNumParams() - resolvedTarget.hasImplicitSelfArg - isVariadic);
    
    LKAssertImplication(resolvedTarget.funcDecl->isInstanceMethod(), resolvedTarget.hasImplicitSelfArg);
    bool hasImplicitSelfArg = resolvedTarget.funcDecl->isInstanceMethod();
    std::vector<llvm::Value *> args(hasImplicitSelfArg, nullptr);
    auto numFixedArgs = llvmFunctionTy->getNumParams() - hasImplicitSelfArg;

    
    // TODO what about just adding the implicit argument(s) to the callExpr and getting rid of the whole argumentOffset dance?
    for (uint64_t i = hasImplicitSelfArg; i < llvmFunctionTy->getNumParams(); i++) {
        auto expr = call->arguments[i - hasImplicitSelfArg];
        auto argTy = getType(expr);
        auto expectedTy = resolveTypeDesc(resolvedTarget.signature.paramTypes[i]);
        auto policy = argumentHandlingPolicies[i];
        
        if (policy == ArgumentHandlingPolicy::PassByReference) {
            args.push_back(codegenExpr(expr, LValue));
            continue;
        }
        
        if (policy == ArgumentHandlingPolicy::PassByValue && expectedTy->isReferenceTy() && argTy->isReferenceTy()) {
            args.push_back(codegenExpr(expr, RValue));
            continue;
        }
        
        bool didConstructCopy;
        auto V = constructCopyIfNecessary(argTy, expr, &didConstructCopy);
        if (policy == ArgumentHandlingPolicy::PassByValue_ExtractReference) {
            LKAssert(argTy->isReferenceTy());
            if (!didConstructCopy) {
                //  only insert a load if no copy was made. otherwise, the copy constructor already returns a non-reference object
                V = builder.CreateLoad(V);
            }
        }
        args.push_back(V);
    }
    
    if (hasImplicitSelfArg) {
        // TODO this is missing checks to make sure selfTy actually matches / is convertible to the expected argument type !?
        std::shared_ptr<ast::Expr> implicitSelfArg;
        
        if (call->target->isOfKind(NK::MemberExpr) && !resolvedTarget.funcDecl->isCallOperatorOverload()) {
            implicitSelfArg = llvm::dyn_cast<ast::MemberExpr>(call->target)->target;
        } else {
            implicitSelfArg = call->target;
        }
        
        auto selfArgTy = getType(implicitSelfArg);
        
        if (isTemporary(implicitSelfArg)) {
            // if a call target is a temporary, we need to make sure the object outlives the call
            // we do this by putting it on the stack, thus implicitly registering it for destruction once we leave the current scope
            // TODO the object should be destructed immediately after the call returns / at the end of the enclosing statement !
            auto ident = makeIdent(currentFunction.getTmpIdent(), implicitSelfArg->getSourceLocation());
            auto varDecl = std::make_shared<ast::VarDecl>(ident, nullptr, implicitSelfArg);
            args[0] = codegenVarDecl(varDecl);
        } else {
            args[0] = codegenExpr(implicitSelfArg, LValue);
        }
    }
    
    
    if (isVariadic && getResolvedFunctionWithName(llvmFunction->getName().str())->funcDecl->getAttributes().extern_) {
        // TODO extract references if possible, disallow otherwise, promote types as expected by C?
        for (auto it = call->arguments.begin() + numFixedArgs; it != call->arguments.end(); it++) {
            auto arg = *it;
            auto argTy = getType(arg);
            auto V = codegenExpr(arg, RValue);
            
            if (auto refTy = llvm::dyn_cast<ReferenceType>(argTy)) {
                V = builder.CreateLoad(V);
                argTy = refTy->getReferencedType();
            }
            
            LKAssert(argTy->isPointerTy() || argTy->isNumericalTy());
            
            if (argTy == builtinTypes.yo.f32) {
                V = builder.CreateFPCast(V, builtinTypes.llvm.Double);
            
            } else if (auto numTy = llvm::dyn_cast<NumericalType>(argTy)) {
                if (numTy->getSize() < builtinTypes.yo.i32->getSize()) {
                    V = builder.CreateIntCast(V, builtinTypes.llvm.i32, numTy->isSigned());
                }
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
            return codegenExpr(castExpr);
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
    LKAssert(call->arguments.size() == 2);
    
    Type *lhsTy = nullptr, *rhsTy = nullptr;
    
    auto lhs = call->arguments.at(0);
    auto rhs = call->arguments.at(1);
    
    if (!typecheckAndApplyTrivialNumberTypeCastsIfNecessary_binop(&lhs, &rhs, &lhsTy, &rhsTy)) { // THIS
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
    
    auto llvmOp = numTy->isFloatTy() ? getLLVMBinaryOpInstruction_Float(op) : getLLVMBinaryOpInstruction_Int(op, numTy->isSigned());
    auto lhsVal = codegenExpr(lhs);
    auto rhsVal = codegenExpr(rhs);
    emitDebugLocation(call);
    return builder.CreateBinOp(llvmOp, lhsVal, rhsVal);
}



// TODO/NOTE: this function has only a single callee, so we can probably rewrite it to better fit that single use case?
bool IRGenerator::typecheckAndApplyTrivialNumberTypeCastsIfNecessary_binop(std::shared_ptr<ast::Expr> *lhs, std::shared_ptr<ast::Expr> *rhs, Type **lhsTy_out, Type **rhsTy_out) {
    LKAssert(lhsTy_out && rhsTy_out);
    
    auto lhsTy = getType(*lhs);
    auto rhsTy = getType(*rhs);
    
//    util::fmt::print("[typecheck&cast] <lhs> of '{}' | <rhs> of '{}'", lhsTy, rhsTy);
    
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
        auto lhs = codegenExpr(lhsExpr);
        auto rhs = codegenExpr(rhsExpr);
        
        emitDebugLocation(call);
        return builder.CreateICmpEQ(lhs, rhs);
    }
    
    // Floats?
    if (lhsTy == rhsTy && lhsTy == builtinTypes.yo.f64) {
        auto lhs = codegenExpr(lhsExpr);
        auto rhs = codegenExpr(rhsExpr);
        auto pred = getMatchingLLVMCmpInstPredicateForComparisonOperator_Float(op);
        
        emitDebugLocation(call);
        return builder.CreateFCmp(pred, lhs, rhs);
    }
    
    
    if (!(lhsTy->isNumericalTy() && rhsTy->isNumericalTy())) {
        auto msg = util::fmt::format("no known comparison for types '{}' and '{}'", lhsTy, rhsTy);
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
        lhsVal = codegenExpr(lhsExpr);
        rhsVal = codegenExpr(rhsExpr);
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
        
        lhsVal = codegenExpr(lhsCast);
        rhsVal = codegenExpr(rhsCast);
        pred = getMatchingLLVMCmpInstPredicateForComparisonOperator_Int(op, numTyLhs->isSigned() || numTyRhs->isSigned());
        
    }
    
    emitDebugLocation(call);
    return builder.CreateICmp(pred, lhsVal, rhsVal);
}





llvm::Value* IRGenerator::codegen_HandleLogOpIntrinsic(Intrinsic I, std::shared_ptr<ast::CallExpr> call) {
    LKAssert(call->arguments.size() == 2);
    LKAssert(I == Intrinsic::LAnd || I == Intrinsic::LOr);
    
    auto lhs = call->arguments[0];
    auto rhs = call->arguments[1];
    
    LKAssert(getType(lhs) == builtinTypes.yo.Bool && getType(rhs) == builtinTypes.yo.Bool);
    
    auto isAnd = I == Intrinsic::LAnd;
    
    auto llvmTrueVal = llvm::ConstantInt::getTrue(builtinTypes.llvm.i1);
    auto llvmFalseVal = llvm::ConstantInt::getFalse(builtinTypes.llvm.i1);
    auto F = currentFunction.llvmFunction;
    
    auto lhsBB = builder.GetInsertBlock();
    auto rhsBB = llvm::BasicBlock::Create(C, "rhs");
    auto mergeBB = llvm::BasicBlock::Create(C, "merge");
    
    auto lhsCmp = builder.CreateICmpEQ(codegenExpr(lhs), llvmTrueVal);
    
    emitDebugLocation(call); // call's SL is the original binop's SL
    builder.CreateCondBr(lhsCmp,
                         isAnd ? rhsBB : mergeBB,
                         isAnd ? mergeBB : rhsBB);
    
    
    F->getBasicBlockList().push_back(rhsBB);
    builder.SetInsertPoint(rhsBB);
    auto rhsVal = builder.CreateICmpEQ(codegenExpr(rhs), llvmTrueVal);
    builder.CreateBr(mergeBB);
    
    F->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);
    
    emitDebugLocation(call);
    auto phi = builder.CreatePHI(builtinTypes.llvm.i1, 2);
    phi->addIncoming(isAnd ? llvmFalseVal : llvmTrueVal, lhsBB);
    phi->addIncoming(rhsVal, rhsBB);
    
    return phi;
}

