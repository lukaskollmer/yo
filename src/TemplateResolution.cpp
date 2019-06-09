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



std::shared_ptr<ast::FunctionDecl> TemplateResolver::SpecializeWithTemplateMapping(std::shared_ptr<ast::FunctionDecl> Decl, TemplateTypeMapping TemplateArgumentsMapping) {
    return TemplateResolver(TemplateArgumentsMapping).Specialize(Decl);
}


TypeInfo *TemplateResolver::ResolveType(TypeInfo *TI) {
    if (TI->Equals(TypeInfo::Unresolved)) return TI;
    
    if (util::map::contains_key(TemplateArgumentMapping, TI->getName())) {
        auto X = TemplateArgumentMapping.at(TI->getName());
        return X;
    }
    
    if (TI->getKind() == TypeInfo::Kind::Pointer) {
        return ResolveType(TI->getPointee())->getPointerTo();
    }
    
    return TI;
}


std::shared_ptr<ast::FunctionDecl> TemplateResolver::Specialize(std::shared_ptr<ast::FunctionDecl> Decl) {
#if 0
    for (auto &[Name, Type] : TemplateArgumentsMapping) {
        std::cout << Name << ": " << Type->Str() << std::endl;
    }
#endif
    
    auto SpecializedFunction = std::make_shared<ast::FunctionDecl>();
    SpecializedFunction->setSourceLocation(Decl->getSourceLocation());
    SpecializedFunction->Signature = std::make_shared<ast::FunctionSignature>(*Decl->Signature);
    SpecializedFunction->Signature->setSourceLocation(Decl->Signature->getSourceLocation());
//    SpecializedFunction->Body = std::make_shared<ast::Composite>();
//    SpecializedFunction->Body->setSourceLocation(Decl->Body->getSourceLocation());
    
    // 1. Substitute in signature (params & return type)
    
    for (auto &Param : SpecializedFunction->Signature->Parameters) {
        auto SourceLoc = Param->getSourceLocation();
        Param = std::make_shared<ast::VariableDecl>(Param->Name, ResolveType(Param->Type));
        Param->setSourceLocation(SourceLoc);
    }
    
    SpecializedFunction->Signature->ReturnType = ResolveType(SpecializedFunction->Signature->ReturnType);
    
    for (auto &[Name, Type] : TemplateArgumentMapping) {
        if (!Type->Equals(TypeInfo::Unresolved)) {
            auto It = std::find(SpecializedFunction->Signature->TemplateArgumentNames.begin(),
                                SpecializedFunction->Signature->TemplateArgumentNames.end(), Name);
            SpecializedFunction->Signature->TemplateArgumentNames.erase(It);
        } else {
            LKFatalError("Unresolved argument type in template function: %s (%s)", Name.c_str(), mangling::MangleCanonicalNameForSignature(Decl->Signature).c_str());
        }
    }
    
    //std::cout << "Before:\n" << Decl->Signature << "\nAfter:\n" << SpecializedFunction->Signature << std::endl;
    
    if (SpecializedFunction->Signature->attributes->intrinsic) {
        LKAssert(Decl->Body == nullptr);
        return SpecializedFunction;
    }
    
    SpecializedFunction->Body = Specialize(Decl->Body);
    SpecializedFunction->Body->setSourceLocation(Decl->Body->getSourceLocation());
    return SpecializedFunction;
}


#define HANDLE(node, T) if (auto X = std::dynamic_pointer_cast<ast::T>(node)) return Specialize(X);

#define IGNORE(node, T) if (std::dynamic_pointer_cast<ast::T>(node)) return (node);

#define unhandled_node(node) \
{ std::cout << "[TemplateResolver::Specialize] Unhandled Node: " << util::typeinfo::GetTypename(*(node)) << std::endl; \
throw; }

std::shared_ptr<ast::LocalStmt> TemplateResolver::Specialize(std::shared_ptr<ast::LocalStmt> Stmt) {
    HANDLE(Stmt, ReturnStmt)
    HANDLE(Stmt, Assignment)
    HANDLE(Stmt, VariableDecl)
    HANDLE(Stmt, WhileStmt)
    HANDLE(Stmt, IfStmt)
    HANDLE(Stmt, ExprStmt)
    unhandled_node(Stmt)
}

std::shared_ptr<ast::Expr> TemplateResolver::Specialize(std::shared_ptr<ast::Expr> Expr) {
    if (!Expr) return Expr;
    IGNORE(Expr, NumberLiteral)
    IGNORE(Expr, Identifier)
    HANDLE(Expr, BinaryOperation)
    HANDLE(Expr, Comparison)
    IGNORE(Expr, StringLiteral)
    HANDLE(Expr, LogicalOperation)
    HANDLE(Expr, MatchExpr)
    HANDLE(Expr, CallExpr)
    HANDLE(Expr, SubscriptExpr)
    HANDLE(Expr, MemberExpr)
    unhandled_node(Expr)
}

#undef HANDLE


#pragma mark - Local Statements


std::shared_ptr<ast::Composite> TemplateResolver::Specialize(std::shared_ptr<ast::Composite> Composite) {
    auto X = std::make_shared<ast::Composite>(util::vector::map(Composite->Statements, [this](auto S) { return Specialize(S); }));
    X->setSourceLocation(Composite->getSourceLocation());
    return X;
}

std::shared_ptr<ast::ReturnStmt> TemplateResolver::Specialize(std::shared_ptr<ast::ReturnStmt> ReturnStmt) {
    auto X = std::make_shared<ast::ReturnStmt>(Specialize(ReturnStmt->Expression));
    X->setSourceLocation(ReturnStmt->getSourceLocation());
    return X;
}

std::shared_ptr<ast::Assignment> TemplateResolver::Specialize(std::shared_ptr<ast::Assignment> Assignment) {
    auto X = std::make_shared<ast::Assignment>(Specialize(Assignment->Target), Specialize(Assignment->Value));
    X->setSourceLocation(Assignment->getSourceLocation());
    return X;
}



std::shared_ptr<ast::WhileStmt> TemplateResolver::Specialize(std::shared_ptr<ast::WhileStmt> WhileStmt) {
    return std::make_shared<ast::WhileStmt>(Specialize(WhileStmt->Condition), Specialize(WhileStmt->Body));
}

std::shared_ptr<ast::IfStmt> TemplateResolver::Specialize(std::shared_ptr<ast::IfStmt> IfStmt) {
    auto Branches = util::vector::map(IfStmt->Branches, [this](auto Branch) -> auto {
        auto X = std::make_shared<ast::IfStmt::Branch>(Branch->Kind, Specialize(Branch->Condition), Specialize(Branch->Body));
        X->setSourceLocation(Branch->getSourceLocation());
        return X;
    });
    
    auto X = std::make_shared<ast::IfStmt>(Branches);
    X->setSourceLocation(IfStmt->getSourceLocation());
    return X;
}



std::shared_ptr<ast::ExprStmt> TemplateResolver::Specialize(std::shared_ptr<ast::ExprStmt> exprStmt) {
    auto X = std::make_shared<ast::ExprStmt>(Specialize(exprStmt->expr));
    X->setSourceLocation(exprStmt->getSourceLocation());
    return X;
}



#pragma mark - Expressions

std::shared_ptr<ast::CallExpr> TemplateResolver::Specialize(std::shared_ptr<ast::CallExpr> call) {
    auto instantiatedCall = std::make_shared<ast::CallExpr>(*call);
    instantiatedCall->arguments = util::vector::map(call->arguments, [this](auto &expr) { return Specialize(expr); });
    instantiatedCall->explicitTemplateArgumentTypes = util::vector::map(call->explicitTemplateArgumentTypes,
                                                                        [this](auto &T) { return ResolveType(T); });
    instantiatedCall->setSourceLocation(call->getSourceLocation());
    return instantiatedCall;
}


std::shared_ptr<ast::SubscriptExpr> TemplateResolver::Specialize(std::shared_ptr<ast::SubscriptExpr> subscript) {
    auto X = std::make_shared<ast::SubscriptExpr>(Specialize(subscript->target), Specialize(subscript->offset));
    X->setSourceLocation(subscript->getSourceLocation());
    return X;
}

std::shared_ptr<ast::MemberExpr> TemplateResolver::Specialize(std::shared_ptr<ast::MemberExpr> memberExpr) {
    auto X = std::make_shared<ast::MemberExpr>(Specialize(memberExpr->target), memberExpr->memberName);
    X->setSourceLocation(memberExpr->getSourceLocation());
    return X;
}





std::shared_ptr<ast::MatchExpr> TemplateResolver::Specialize(std::shared_ptr<ast::MatchExpr> MatchExpr) {
    // TODO file a radar ?!
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-lambda-capture"
    auto Branches = util::vector::map(MatchExpr->Branches, [this](auto Branch) -> auto {
//        auto Patterns = util::vector::map(Branch->Patterns, [this](auto Pattern) -> auto { return Specialize(Pattern); });
//        return std::make_shared<ast::MatchExpr::MatchExprBranch>(Patterns, Specialize(Branch->Expression));
        
        auto X = std::make_shared<ast::MatchExpr::MatchExprBranch>(util::vector::map(Branch->Patterns, [this](auto P) { return Specialize(P); }),
                                                                   Specialize(Branch->Expression));
        X->setSourceLocation(Branch->getSourceLocation());
        return X;
    });
#pragma clang diagnostic pop
    auto X = std::make_shared<ast::MatchExpr>(Specialize(MatchExpr->Target), Branches);
    X->setSourceLocation(MatchExpr->getSourceLocation());
    return X;
}


std::shared_ptr<ast::VariableDecl> TemplateResolver::Specialize(std::shared_ptr<ast::VariableDecl> Decl) {
    auto X = std::make_shared<ast::VariableDecl>(Decl->Name, ResolveType(Decl->Type), Specialize(Decl->InitialValue));
    X->setSourceLocation(Decl->getSourceLocation());
    return X;
}


std::shared_ptr<ast::Comparison> TemplateResolver::Specialize(std::shared_ptr<ast::Comparison> Comparison) {
    auto X = std::make_shared<ast::Comparison>(Comparison->Op, Specialize(Comparison->LHS), Specialize(Comparison->RHS));
    X->setSourceLocation(Comparison->getSourceLocation());
    return X;
}


std::shared_ptr<ast::BinaryOperation> TemplateResolver::Specialize(std::shared_ptr<ast::BinaryOperation> Binop) {
    auto X = std::make_shared<ast::BinaryOperation>(Binop->Op, Specialize(Binop->LHS), Specialize(Binop->RHS));
    X->setSourceLocation(Binop->getSourceLocation());
    return X;
}


std::shared_ptr<ast::LogicalOperation> TemplateResolver::Specialize(std::shared_ptr<ast::LogicalOperation> LogicalOperation) {
    auto X = std::make_shared<ast::LogicalOperation>(LogicalOperation->Op, Specialize(LogicalOperation->LHS), Specialize(LogicalOperation->RHS));
    X->setSourceLocation(LogicalOperation->getSourceLocation());
    return X;
}
