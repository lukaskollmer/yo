//
//  TemplateResolution.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-04-20.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "TemplateResolution.h"
#include "Mangling.h"
#include "Attributes.h"

// Template Instantiation

// TODO:
// - make sure local declarations don't somehow shadow template parameters
// - current implementation definetely could be improved by getting rid of the dynamic casts and writing some kind of generic ast visitor / rewriter or whatever


using namespace yo;
using namespace irgen;



std::shared_ptr<ast::FunctionDecl> TemplateResolver::specializeWithTemplateMapping(std::shared_ptr<ast::FunctionDecl> decl, TemplateTypeMapping templateArgumentsMapping) {
    return TemplateResolver(templateArgumentsMapping).specialize(decl);
}


std::shared_ptr<ast::TypeDesc> TemplateResolver::resolveType(std::shared_ptr<ast::TypeDesc> typeDesc) {
    if (!typeDesc) return nullptr;
    
    switch (typeDesc->getKind()) {
        case ast::TypeDesc::Kind::Pointer:
            return ast::TypeDesc::makePointer(resolveType(typeDesc->getPointee()));
        
        case ast::TypeDesc::Kind::Nominal: {
            if (auto ty = util::map::get_opt(templateArgumentMapping, typeDesc->getName())) {
                return ty.value();
            }
            return typeDesc;
        }
        
        case ast::TypeDesc::Kind::Function:
            LKFatalError("TODO");
        
        case ast::TypeDesc::Kind::Resolved:
            return typeDesc;
    }
//    switch (typeDesc->getKind()) {
//        case ast::TypeDesc::Kind::Pointer:
//            return resol
//    }
    
    
    //if (TI->equals(TypeInfo::Unresolved)) return TI;
    
//    if (util::map::contains_key(templateArgumentMapping, TI->getName())) {
//        auto X = templateArgumentMapping.at(TI->getName());
//        return X;
//    }
//
//    if (TI->getKind() == TypeInfo::Kind::Pointer) {
//        return resolveType(TI->getPointee())->getPointerTo();
//    }
//
//    return TI;
}


std::shared_ptr<ast::FunctionDecl> TemplateResolver::specialize(std::shared_ptr<ast::FunctionDecl> decl) {
#if 0
    for (auto& [name, type] : templateArgumentsMapping) {
        std::cout << name << ": " << type->Str() << std::endl;
    }
#endif
    
    auto signature = decl->getSignature();
    
    // Substitute in signature (params & return type)
    for (auto& param : signature.parameters) {
        auto loc = param->getSourceLocation();
        param = std::make_shared<ast::VarDecl>(param->name, resolveType(param->type));
        param->setSourceLocation(loc);
    }
    signature.returnType = resolveType(signature.returnType);
    
    for (const auto& [name, type] : templateArgumentMapping) {
        if (type) {
            auto it = std::find(signature.templateArgumentNames.begin(),
                                signature.templateArgumentNames.end(), name);
            signature.templateArgumentNames.erase(it);
        } else {
            LKFatalError("Unresolved argument type in template function: %s (%s)", name.c_str(), mangling::mangleCanonicalName(decl).c_str());
        }
    }
    
    
    auto specializedFuncDecl = std::make_shared<ast::FunctionDecl>(decl->getFunctionKind(), decl->getName(), signature, decl->getAttributes());
    specializedFuncDecl->setSourceLocation(decl->getSourceLocation());
    
    if (decl->getAttributes().intrinsic) {
        LKAssert(decl->getBody().empty());
        return specializedFuncDecl;
    }
    
    specializedFuncDecl->setBody(specialize(decl->getBody()));
    return specializedFuncDecl;
}


#define HANDLE(node, T) if (auto X = std::dynamic_pointer_cast<ast::T>(node)) return specialize(X);

#define IGNORE(node, T) if (std::dynamic_pointer_cast<ast::T>(node)) return (node);

#define unhandled_node(node) \
{ std::cout << "[TemplateResolver::specialize] Unhandled Node: " << util::typeinfo::getTypename(*(node)) << std::endl; \
throw; }

std::shared_ptr<ast::LocalStmt> TemplateResolver::specialize(std::shared_ptr<ast::LocalStmt> stmt) {
    HANDLE(stmt, ReturnStmt)
    HANDLE(stmt, Assignment)
    HANDLE(stmt, VarDecl)
    HANDLE(stmt, WhileStmt)
    HANDLE(stmt, IfStmt)
    HANDLE(stmt, ExprStmt)
    unhandled_node(stmt)
}

std::shared_ptr<ast::Expr> TemplateResolver::specialize(std::shared_ptr<ast::Expr> expr) {
    if (!expr) return expr;
    IGNORE(expr, NumberLiteral)
    IGNORE(expr, Ident)
    HANDLE(expr, BinOp)
    HANDLE(expr, Comparison)
    IGNORE(expr, StringLiteral)
    HANDLE(expr, LogicalOperation)
    HANDLE(expr, MatchExpr)
    HANDLE(expr, CallExpr)
    HANDLE(expr, SubscriptExpr)
    HANDLE(expr, MemberExpr)
    unhandled_node(expr)
}

#undef HANDLE


#pragma mark - Local Statements


std::shared_ptr<ast::Composite> TemplateResolver::specialize(std::shared_ptr<ast::Composite> composite) {
    auto X = std::make_shared<ast::Composite>(specialize(composite->statements));
    X->setSourceLocation(composite->getSourceLocation());
    return X;
}


std::vector<std::shared_ptr<ast::LocalStmt>> TemplateResolver::specialize(std::vector<std::shared_ptr<ast::LocalStmt>> stmtList) {
    return util::vector::map(stmtList, [this](auto& stmt) { return specialize(stmt); });
}

std::shared_ptr<ast::ReturnStmt> TemplateResolver::specialize(std::shared_ptr<ast::ReturnStmt> returnStmt) {
    auto X = std::make_shared<ast::ReturnStmt>(specialize(returnStmt->expression));
    X->setSourceLocation(returnStmt->getSourceLocation());
    return X;
}

std::shared_ptr<ast::Assignment> TemplateResolver::specialize(std::shared_ptr<ast::Assignment> assignment) {
    auto X = std::make_shared<ast::Assignment>(specialize(assignment->target), specialize(assignment->value));
    X->setSourceLocation(assignment->getSourceLocation());
    return X;
}



std::shared_ptr<ast::WhileStmt> TemplateResolver::specialize(std::shared_ptr<ast::WhileStmt> whileStmt) {
    auto X = std::make_shared<ast::WhileStmt>(specialize(whileStmt->condition), specialize(whileStmt->body));
    X->setSourceLocation(whileStmt->getSourceLocation());
    return X;
}

std::shared_ptr<ast::IfStmt> TemplateResolver::specialize(std::shared_ptr<ast::IfStmt> ifStmt) {
    auto branches = util::vector::map(ifStmt->branches, [this](auto branch) -> auto {
        auto X = std::make_shared<ast::IfStmt::Branch>(branch->kind, specialize(branch->condition), specialize(branch->body));
        X->setSourceLocation(branch->getSourceLocation());
        return X;
    });
    
    auto X = std::make_shared<ast::IfStmt>(branches);
    X->setSourceLocation(ifStmt->getSourceLocation());
    return X;
}



std::shared_ptr<ast::ExprStmt> TemplateResolver::specialize(std::shared_ptr<ast::ExprStmt> exprStmt) {
    auto X = std::make_shared<ast::ExprStmt>(specialize(exprStmt->expr));
    X->setSourceLocation(exprStmt->getSourceLocation());
    return X;
}



#pragma mark - Expressions

std::shared_ptr<ast::CallExpr> TemplateResolver::specialize(std::shared_ptr<ast::CallExpr> call) {
    auto instantiatedCall = std::make_shared<ast::CallExpr>(*call);
    instantiatedCall->arguments = util::vector::map(call->arguments, [this](auto& expr) { return specialize(expr); });
    instantiatedCall->explicitTemplateArgumentTypes = util::vector::map(call->explicitTemplateArgumentTypes,
                                                                        [this](auto& T) { return resolveType(T); });
    instantiatedCall->setSourceLocation(call->getSourceLocation());
    return instantiatedCall;
}


std::shared_ptr<ast::SubscriptExpr> TemplateResolver::specialize(std::shared_ptr<ast::SubscriptExpr> subscript) {
    auto X = std::make_shared<ast::SubscriptExpr>(specialize(subscript->target), specialize(subscript->offset));
    X->setSourceLocation(subscript->getSourceLocation());
    return X;
}

std::shared_ptr<ast::MemberExpr> TemplateResolver::specialize(std::shared_ptr<ast::MemberExpr> memberExpr) {
    auto X = std::make_shared<ast::MemberExpr>(specialize(memberExpr->target), memberExpr->memberName);
    X->setSourceLocation(memberExpr->getSourceLocation());
    return X;
}





std::shared_ptr<ast::MatchExpr> TemplateResolver::specialize(std::shared_ptr<ast::MatchExpr> matchExpr) {
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


std::shared_ptr<ast::VarDecl> TemplateResolver::specialize(std::shared_ptr<ast::VarDecl> decl) {
    auto X = std::make_shared<ast::VarDecl>(decl->name, resolveType(decl->type), specialize(decl->initialValue));
    X->setSourceLocation(decl->getSourceLocation());
    return X;
}


std::shared_ptr<ast::Comparison> TemplateResolver::specialize(std::shared_ptr<ast::Comparison> comparison) {
    auto X = std::make_shared<ast::Comparison>(comparison->op, specialize(comparison->lhs), specialize(comparison->rhs));
    X->setSourceLocation(comparison->getSourceLocation());
    return X;
}


std::shared_ptr<ast::BinOp> TemplateResolver::specialize(std::shared_ptr<ast::BinOp> binop) {
    auto X = std::make_shared<ast::BinOp>(binop->op, specialize(binop->lhs), specialize(binop->rhs));
    X->setSourceLocation(binop->getSourceLocation());
    return X;
}


std::shared_ptr<ast::LogicalOperation> TemplateResolver::specialize(std::shared_ptr<ast::LogicalOperation> logicalOperation) {
    auto X = std::make_shared<ast::LogicalOperation>(logicalOperation->op, specialize(logicalOperation->lhs), specialize(logicalOperation->rhs));
    X->setSourceLocation(logicalOperation->getSourceLocation());
    return X;
}
