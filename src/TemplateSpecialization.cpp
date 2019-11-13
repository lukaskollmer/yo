//
//  TemplateSpecialization.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-04-20.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "TemplateSpecialization.h"
#include "Mangling.h"
#include "Attributes.h"

// Template Instantiation

// TODO:
// - make sure local declarations don't somehow shadow template parameters
// - current implementation definetely could be improved by getting rid of the dynamic casts and writing some kind of generic ast visitor / rewriter or whatever


using namespace yo;
using namespace irgen;



std::shared_ptr<ast::FunctionDecl> TemplateSpecializer::specializeWithTemplateMapping(std::shared_ptr<ast::FunctionDecl> decl, TemplateTypeMapping templateArgumentsMapping) {
    return TemplateSpecializer(templateArgumentsMapping).specialize(decl);
}


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
        
        case TDK::Function:
            LKFatalError("TODO");
        
    }
}


std::shared_ptr<ast::FunctionDecl> TemplateSpecializer::specialize(std::shared_ptr<ast::FunctionDecl> decl) {
#if 0
    for (auto& [name, type] : templateArgumentsMapping) {
        std::cout << name << ": " << type->Str() << std::endl;
    }
#endif
    
    auto signature = decl->getSignature();
    
    // Substitute in signature (params & return type)
//    for (auto &paramTy: signature.paramTypes) {
//        // TODO util::vec::map?
//        paramTy = resolveType(paramTy);
//    }
    
    signature.paramTypes = util::vector::map(decl->getSignature().paramTypes, [&](auto paramTy) {
        auto resolved = resolveType(paramTy);
        //util::fmt::print("{} -> {}", reinterpret_cast<uintptr_t>(paramTy.get()), reinterpret_cast<uintptr_t>(resolved.get()));
        return resolved;
    });
    signature.returnType = resolveType(signature.returnType);
    
//    for (const auto& [name, type] : templateArgumentMapping) {
//        if (type) {
//            auto it = std::find(signature.templateArgumentNames.begin(),
//                                signature.templateArgumentNames.end(), name);
//            signature.templateArgumentNames.erase(it);
//        } else {
//            LKFatalError("Unresolved argument type in template function: %s (%s)", name.c_str(), mangling::mangleCanonicalName(decl).c_str());
//        }
//    }
    
    // TODO instead of replacig this here, simply add the mapping to the FuncDecl and insert implicit typealiases during function codegen!
    for (const auto &templateArgTypename : signature.templateArgumentNames) {
        LKAssert(util::map::has_key(templateArgumentMapping, templateArgTypename));
    }
    signature.templateArgumentNames = {};
    
    
    auto specializedFuncDecl = std::make_shared<ast::FunctionDecl>(decl->getFunctionKind(),
                                                                   decl->getName(),
                                                                   signature,
                                                                   decl->getParamNames(),
                                                                   decl->getAttributes());
    specializedFuncDecl->setSourceLocation(decl->getSourceLocation());
    
    if (decl->getAttributes().intrinsic) {
        LKAssert(decl->getBody()->isEmpty());
        return specializedFuncDecl;
    }
    
    specializedFuncDecl->setImplType(decl->getImplType());
    specializedFuncDecl->setBody(specialize(decl->getBody()));
    return specializedFuncDecl;
}


#define HANDLE(node, T) if (auto X = std::dynamic_pointer_cast<ast::T>(node)) return specialize(X);

#define IGNORE(node, T) if (std::dynamic_pointer_cast<ast::T>(node)) return (node);

#define unhandled_node(node) \
{ std::cout << "[TemplateSpecializer::specialize] Unhandled Node: " << util::typeinfo::getTypename(*(node)) << std::endl; \
throw; }

std::shared_ptr<ast::LocalStmt> TemplateSpecializer::specialize(std::shared_ptr<ast::LocalStmt> stmt) {
    HANDLE(stmt, ReturnStmt)
    HANDLE(stmt, Assignment)
    HANDLE(stmt, VarDecl)
    HANDLE(stmt, WhileStmt)
    HANDLE(stmt, IfStmt)
    HANDLE(stmt, ExprStmt)
    unhandled_node(stmt)
}

std::shared_ptr<ast::Expr> TemplateSpecializer::specialize(std::shared_ptr<ast::Expr> expr) {
    if (!expr) return expr;
    IGNORE(expr, NumberLiteral)
    IGNORE(expr, Ident)
    IGNORE(expr, StringLiteral)
    HANDLE(expr, MatchExpr)
    HANDLE(expr, CallExpr)
    HANDLE(expr, SubscriptExpr)
    HANDLE(expr, MemberExpr)
    HANDLE(expr, BinOp)
    HANDLE(expr, UnaryExpr)
    unhandled_node(expr)
}

#undef HANDLE


#pragma mark - Local Statements


std::shared_ptr<ast::Composite> TemplateSpecializer::specialize(std::shared_ptr<ast::Composite> composite) {
    auto X = std::make_shared<ast::Composite>(specialize(composite->statements));
    X->setSourceLocation(composite->getSourceLocation());
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



std::shared_ptr<ast::ExprStmt> TemplateSpecializer::specialize(std::shared_ptr<ast::ExprStmt> exprStmt) {
    auto X = std::make_shared<ast::ExprStmt>(specialize(exprStmt->expr));
    X->setSourceLocation(exprStmt->getSourceLocation());
    return X;
}



#pragma mark - Expressions

std::shared_ptr<ast::CallExpr> TemplateSpecializer::specialize(std::shared_ptr<ast::CallExpr> call) {
    auto instantiatedCall = std::make_shared<ast::CallExpr>(*call);
    instantiatedCall->arguments = util::vector::map(call->arguments, [this](auto& expr) { return specialize(expr); });
    instantiatedCall->explicitTemplateArgumentTypes = util::vector::map(call->explicitTemplateArgumentTypes,
                                                                        [this](auto& T) { return resolveType(T); });
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


std::shared_ptr<ast::VarDecl> TemplateSpecializer::specialize(std::shared_ptr<ast::VarDecl> decl) {
    auto X = std::make_shared<ast::VarDecl>(decl->name, resolveType(decl->type), specialize(decl->initialValue));
    X->setSourceLocation(decl->getSourceLocation());
    return X;
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
