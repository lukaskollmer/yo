//
//  TemplateSpecialization.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-04-20.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "TemplateSpecialization.h"
#include "Mangling.h"
#include "parse/Attributes.h"

// Template Instantiation

// TODO:
// - make sure local declarations don't somehow shadow template parameters
// - current implementation definetely could be improved by getting rid of the dynamic casts and writing some kind of generic ast visitor / rewriter or whatever


using namespace yo;
using namespace irgen;

using NK = ast::Node::Kind;


std::shared_ptr<ast::TypeDesc> TemplateSpecializer::resolveType(std::shared_ptr<ast::TypeDesc> typeDesc) {
    if (!typeDesc) return nullptr;
    using TDK = ast::TypeDesc::Kind;
    
    const auto &loc = typeDesc->getSourceLocation();
    
    switch (typeDesc->getKind()) {
        case TDK::Resolved:
            return ast::TypeDesc::makeResolved(typeDesc->getResolvedType(), loc);
        
        case TDK::Pointer:
            return ast::TypeDesc::makePointer(resolveType(typeDesc->getPointee()), loc);
        
        case TDK::Reference:
            return ast::TypeDesc::makeReference(resolveType(typeDesc->getPointee()), loc);
        
        case TDK::Nominal: {
            if (auto ty = util::map::get_opt(templateArgumentMapping, typeDesc->getName())) {
                return std::make_shared<ast::TypeDesc>(**ty);
            }
            return std::make_shared<ast::TypeDesc>(*typeDesc);
        }
        
        case TDK::Decltype:
            return ast::TypeDesc::makeDecltype(specialize(typeDesc->getDecltypeExpr()), loc);
        
        case TDK::NominalTemplated: {
            auto resolvedTemplateArgs = util::vector::map(typeDesc->getTemplateArgs(), [this](auto &ty) { return resolveType(ty); });
            return ast::TypeDesc::makeNominalTemplated(typeDesc->getName(), resolvedTemplateArgs, loc);
        }
        
//        case TDK::Tuple: {
//            auto resolvedTys = util::vector::map(typeDesc->getTupleMembers(), [this](auto &ty) { return resolveType(ty); });
//            return ast::TypeDesc::makeTuple(resolvedTys, loc);
//        }
        
        case TDK::Tuple:
        case TDK::Function:
            LKFatalError("TODO");
        
        
    }
}


ast::FunctionSignature TemplateSpecializer::specialize(const ast::FunctionSignature &signature) {
    ast::FunctionSignature specSig = signature;
    
    for (size_t idx = 0; idx < signature.numberOfParameters(); idx++) {
        specSig.paramTypes[idx] = resolveType(signature.paramTypes[idx]);
    }
    specSig.returnType = resolveType(signature.returnType);
    specSig.setSourceLocation(signature.getSourceLocation());
    
    if (signature.numberOfTemplateParameters() > 0) {
        specSig.templateParamsDecl = std::make_shared<ast::TemplateParamDeclList>();
        specSig.templateParamsDecl->setSourceLocation(signature.templateParamsDecl->getSourceLocation());
        
        for (auto &param : signature.templateParamsDecl->getParams()) {
            if (!util::map::has_key(templateArgumentMapping, param.name->value)) {
                specSig.templateParamsDecl->addParam({ param.name, resolveType(param.defaultType) });
            }
        }
    }
    
    if (specSig.templateParamsDecl && specSig.templateParamsDecl->size() == 0) {
        specSig.templateParamsDecl = nullptr;
    }
    
    return specSig;
}


std::shared_ptr<ast::FunctionDecl> TemplateSpecializer::specialize(std::shared_ptr<ast::FunctionDecl> decl) {
    auto specializedFuncDecl = std::make_shared<ast::FunctionDecl>(decl->getFunctionKind(),
                                                                   decl->getName(),
                                                                   specialize(decl->getSignature()),
                                                                   decl->getAttributes());
    specializedFuncDecl->setSourceLocation(decl->getSourceLocation());
    specializedFuncDecl->setParamNames(decl->getParamNames()); // TODO make copies here!
    
    if (decl->getAttributes().intrinsic) {
        LKAssert(decl->getBody()->isEmpty());
        return specializedFuncDecl;
    }
    
    specializedFuncDecl->setImplType(decl->getImplType());
    specializedFuncDecl->setBody(specialize(decl->getBody()));
    return specializedFuncDecl;
}





std::shared_ptr<ast::StructDecl> TemplateSpecializer::specialize(std::shared_ptr<ast::StructDecl> SD) {
    auto specDecl = std::make_shared<ast::StructDecl>();
    specDecl->setSourceLocation(SD->getSourceLocation());
    specDecl->name = SD->getName();
    specDecl->attributes = SD->attributes;
    specDecl->members.reserve(SD->members.size());
    specDecl->methods.reserve(SD->methods.size());
    
    for (auto member : SD->members) {
        specDecl->members.push_back(specialize(member));
    }
    
    for (auto func : SD->methods) {
        specDecl->methods.push_back(specialize(func));
    }
    
    return specDecl;
}



std::shared_ptr<ast::ImplBlock> TemplateSpecializer::specialize(std::shared_ptr<ast::ImplBlock> implBlock) {
    auto decl = std::make_shared<ast::ImplBlock>(implBlock->typeDesc);
    decl->setSourceLocation(implBlock->getSourceLocation());
    decl->methods = util::vector::map(implBlock->methods, [&](auto M) { return specialize(M); });
    decl->isNominalTemplateType = implBlock->isNominalTemplateType;
    return decl;
}




#define CASE(T) case NK::T: return specialize(std::static_pointer_cast<ast::T>(node));

#define unhandled_node(node) \
{ std::cout << "[TemplateSpecializer::specialize] Unhandled Node: " << util::typeinfo::getTypename(*(node)) << std::endl; \
throw; }

std::shared_ptr<ast::LocalStmt> TemplateSpecializer::specialize(std::shared_ptr<ast::LocalStmt> node) {
    switch (node->getKind()) {
        CASE(ReturnStmt)
        CASE(Assignment)
        CASE(VarDecl)
        CASE(WhileStmt)
        CASE(IfStmt)
        CASE(ExprStmt)
        CASE(ForLoop)
        default:
            unhandled_node(node)
    }
}

std::shared_ptr<ast::Expr> TemplateSpecializer::specialize(std::shared_ptr<ast::Expr> node) {
    if (!node) return node;
    
    switch (node->getKind()) {
        // ignored nodes
        case NK::Ident:
        case NK::NumberLiteral:
        case NK::StringLiteral:
            return node;
        
        CASE(MatchExpr)
        CASE(CallExpr)
        CASE(SubscriptExpr)
        CASE(MemberExpr)
        CASE(BinOp)
        CASE(UnaryExpr)
        CASE(LambdaExpr)
        CASE(StaticDeclRefExpr)
        
        default:
            unhandled_node(node)
    }
}

#undef CASE
#undef unhandled_node

#pragma mark - Local Statements


std::shared_ptr<ast::CompoundStmt> TemplateSpecializer::specialize(std::shared_ptr<ast::CompoundStmt> stmt) {
    auto X = std::make_shared<ast::CompoundStmt>(specialize(stmt->statements));
    X->setSourceLocation(stmt->getSourceLocation());
    return X;
}


std::vector<std::shared_ptr<ast::LocalStmt>> TemplateSpecializer::specialize(std::vector<std::shared_ptr<ast::LocalStmt>> stmtList) {
    return util::vector::map(stmtList, [this](auto& stmt) { return specialize(stmt); });
}

std::shared_ptr<ast::ReturnStmt> TemplateSpecializer::specialize(std::shared_ptr<ast::ReturnStmt> returnStmt) {
    auto X = std::make_shared<ast::ReturnStmt>(specialize(returnStmt->expr));
    X->setSourceLocation(returnStmt->getSourceLocation());
    return X;
}

std::shared_ptr<ast::Assignment> TemplateSpecializer::specialize(std::shared_ptr<ast::Assignment> assignment) {
    auto X = std::make_shared<ast::Assignment>(specialize(assignment->target), specialize(assignment->value));
    X->setSourceLocation(assignment->getSourceLocation());
    return X;
}



std::shared_ptr<ast::WhileStmt> TemplateSpecializer::specialize(std::shared_ptr<ast::WhileStmt> whileStmt) {
    auto X = std::make_shared<ast::WhileStmt>(specialize(whileStmt->condition), specialize(whileStmt->body));
    X->setSourceLocation(whileStmt->getSourceLocation());
    return X;
}

std::shared_ptr<ast::IfStmt> TemplateSpecializer::specialize(std::shared_ptr<ast::IfStmt> ifStmt) {
    auto branches = util::vector::map(ifStmt->branches, [this](auto branch) -> auto {
        auto X = std::make_shared<ast::IfStmt::Branch>(branch->kind, specialize(branch->condition), specialize(branch->body));
        X->setSourceLocation(branch->getSourceLocation());
        return X;
    });
    
    auto X = std::make_shared<ast::IfStmt>(branches);
    X->setSourceLocation(ifStmt->getSourceLocation());
    return X;
}


std::shared_ptr<ast::ForLoop> TemplateSpecializer::specialize(std::shared_ptr<ast::ForLoop> forLoop) {
    auto X = std::make_shared<ast::ForLoop>(forLoop->ident, specialize(forLoop->expr), specialize(forLoop->body));
    X->capturesByReference = forLoop->capturesByReference;
    X->setSourceLocation(forLoop->getSourceLocation());
    return X;
}


std::shared_ptr<ast::ExprStmt> TemplateSpecializer::specialize(std::shared_ptr<ast::ExprStmt> exprStmt) {
    auto X = std::make_shared<ast::ExprStmt>(specialize(exprStmt->expr));
    X->setSourceLocation(exprStmt->getSourceLocation());
    return X;
}



#pragma mark - Expressions

std::shared_ptr<ast::CallExpr> TemplateSpecializer::specialize(std::shared_ptr<ast::CallExpr> call) {
    auto instantiatedCall = std::make_shared<ast::CallExpr>(*call);
    instantiatedCall->target = specialize(call->target);
    instantiatedCall->arguments = util::vector::map(call->arguments, [this](auto& expr) { return specialize(expr); });
    instantiatedCall->explicitTemplateArgs = specialize(call->explicitTemplateArgs);
    instantiatedCall->setSourceLocation(call->getSourceLocation());
    return instantiatedCall;
}


std::shared_ptr<ast::SubscriptExpr> TemplateSpecializer::specialize(std::shared_ptr<ast::SubscriptExpr> subscript) {
    auto X = std::make_shared<ast::SubscriptExpr>(specialize(subscript->target), specialize(subscript->offset));
    X->setSourceLocation(subscript->getSourceLocation());
    return X;
}

std::shared_ptr<ast::MemberExpr> TemplateSpecializer::specialize(std::shared_ptr<ast::MemberExpr> memberExpr) {
    auto X = std::make_shared<ast::MemberExpr>(specialize(memberExpr->target), memberExpr->memberName);
    X->setSourceLocation(memberExpr->getSourceLocation());
    return X;
}

std::shared_ptr<ast::StaticDeclRefExpr> TemplateSpecializer::specialize(std::shared_ptr<ast::StaticDeclRefExpr> staticDeclRefExpr) {
    auto expr = std::make_shared<ast::StaticDeclRefExpr>(*staticDeclRefExpr);
    expr->typeDesc = resolveType(expr->typeDesc);
    return expr;
}





std::shared_ptr<ast::MatchExpr> TemplateSpecializer::specialize(std::shared_ptr<ast::MatchExpr> matchExpr) {
    // TODO file a radar ?!
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-lambda-capture"
    std::vector<ast::MatchExpr::MatchExprBranch> branches = util::vector::map(matchExpr->branches, [this] (const auto& branch) {
        auto x = ast::MatchExpr::MatchExprBranch(util::vector::map(branch.patterns, [this](auto p) {return specialize(p); }), specialize(branch.expression));
        x.setSourceLocation(branch.getSourceLocation());
        return x;
    });
#pragma clang diagnostic pop
    auto X = std::make_shared<ast::MatchExpr>(specialize(matchExpr->target), branches);
    X->setSourceLocation(matchExpr->getSourceLocation());
    return X;
}


std::shared_ptr<ast::VarDecl> TemplateSpecializer::specialize(std::shared_ptr<ast::VarDecl> varDecl) {
    auto specDecl = std::make_shared<ast::VarDecl>(*varDecl);
    specDecl->type = resolveType(varDecl->type);
    specDecl->initialValue = specialize(varDecl->initialValue);
    return specDecl;
}


std::shared_ptr<ast::BinOp> TemplateSpecializer::specialize(std::shared_ptr<ast::BinOp> binop) {
    auto X = std::make_shared<ast::BinOp>(binop->getOperator(), specialize(binop->getLhs()), specialize(binop->getRhs()));
    X->setSourceLocation(binop->getSourceLocation());
    return X;
}


std::shared_ptr<ast::UnaryExpr> TemplateSpecializer::specialize(std::shared_ptr<ast::UnaryExpr> expr) {
    auto X = std::make_shared<ast::UnaryExpr>(expr->op, specialize(expr->expr));
    X->setSourceLocation(expr->getSourceLocation());
    return X;
}



std::shared_ptr<ast::LambdaExpr> TemplateSpecializer::specialize(std::shared_ptr<ast::LambdaExpr> lambdaExpr) {
    LKFatalError("TODO: implement!");
    auto specExpr = std::make_shared<ast::LambdaExpr>();
    specExpr->setSourceLocation(lambdaExpr->getSourceLocation());
//    specExpr->
    return specExpr;
}





std::shared_ptr<ast::TemplateParamDeclList> TemplateSpecializer::specialize(std::shared_ptr<ast::TemplateParamDeclList> templateDecls) {
    if (!templateDecls) return nullptr;
    LKFatalError("TODO");
}



std::shared_ptr<ast::TemplateParamArgList> TemplateSpecializer::specialize(std::shared_ptr<ast::TemplateParamArgList> argList) {
    if (!argList) return nullptr;
    
    auto spec = std::make_shared<ast::TemplateParamArgList>();
    spec->setSourceLocation(argList->getSourceLocation());
    spec->elements = util::vector::map(argList->elements, [&](const auto &elem) { return resolveType(elem); });
    return spec;
}
