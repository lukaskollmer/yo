//
//  TemplateResolution.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-04-20.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "TemplateResolution.h"
#include "Mangling.h"
#include "Annotations.h"

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
    if (TI->equals(TypeInfo::Unresolved)) return TI;
    
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
    SpecializedFunction->Annotations = Decl->Annotations;
    SpecializedFunction->Signature = std::make_shared<ast::FunctionSignature>(*Decl->Signature);
    SpecializedFunction->Body = std::make_shared<ast::Composite>();
    
    // 1. Substitute in signature (params & return type)
    
    for (auto &Param : SpecializedFunction->Signature->Parameters) {
        Param = std::make_shared<ast::VariableDecl>(Param->Name, ResolveType(Param->Type));
    }
    
    SpecializedFunction->Signature->ReturnType = ResolveType(SpecializedFunction->Signature->ReturnType);
    
    for (auto &[Name, Type] : TemplateArgumentMapping) {
        if (!Type->equals(TypeInfo::Unresolved)) {
            auto It = std::find(SpecializedFunction->Signature->TemplateArgumentNames.begin(),
                                SpecializedFunction->Signature->TemplateArgumentNames.end(), Name);
            SpecializedFunction->Signature->TemplateArgumentNames.erase(It);
        } else {
            LKFatalError("Unresolved argument type in template function: %s (%s)", Name.c_str(), mangling::MangleCanonicalNameForSignature(Decl->Signature).c_str());
        }
    }
    
    //std::cout << "Before:\n" << Decl->Signature << "\nAfter:\n" << SpecializedFunction->Signature << std::endl;
    
    if (SpecializedFunction->HasAnnotation(annotations::intrinsic)) {
        precondition(Decl->Body->Statements.empty() && "intrinsic functions must have an empty body");
        return SpecializedFunction;
    }
    
    SpecializedFunction->Body = Specialize(Decl->Body);
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
    HANDLE(Stmt, MemberAccess)
    unhandled_node(Stmt)
}

std::shared_ptr<ast::Expr> TemplateResolver::Specialize(std::shared_ptr<ast::Expr> Expr) {
    if (!Expr) return Expr;
    HANDLE(Expr, MemberAccess)
    HANDLE(Expr, FunctionCall)
    IGNORE(Expr, NumberLiteral)
    IGNORE(Expr, Identifier)
    HANDLE(Expr, BinaryOperation)
    HANDLE(Expr, Comparison)
    IGNORE(Expr, StringLiteral)
    HANDLE(Expr, LogicalOperation)
    HANDLE(Expr, MatchExpr)
    unhandled_node(Expr)
}

#undef HANDLE


#pragma mark - Local Statements


std::shared_ptr<ast::Composite> TemplateResolver::Specialize(std::shared_ptr<ast::Composite> Composite) {
    return std::make_shared<ast::Composite>(util::vector::map(Composite->Statements,
                                                              [this](auto S) -> auto {
                                                                  return Specialize(S);
                                                              }));
}

std::shared_ptr<ast::ReturnStmt> TemplateResolver::Specialize(std::shared_ptr<ast::ReturnStmt> ReturnStmt) {
    return std::make_shared<ast::ReturnStmt>(Specialize(ReturnStmt->Expression));
}

std::shared_ptr<ast::Assignment> TemplateResolver::Specialize(std::shared_ptr<ast::Assignment> Assignment) {
    return std::make_shared<ast::Assignment>(Specialize(Assignment->Target), Specialize(Assignment->Value));
}



std::shared_ptr<ast::WhileStmt> TemplateResolver::Specialize(std::shared_ptr<ast::WhileStmt> WhileStmt) {
    return std::make_shared<ast::WhileStmt>(Specialize(WhileStmt->Condition), Specialize(WhileStmt->Body));
}

std::shared_ptr<ast::IfStmt> TemplateResolver::Specialize(std::shared_ptr<ast::IfStmt> IfStmt) {
    auto Branches = util::vector::map(IfStmt->Branches, [this](auto Branch) -> auto {
        return std::make_shared<ast::IfStmt::Branch>(Branch->Kind, Specialize(Branch->Condition), Specialize(Branch->Body));
    });
    
    return std::make_shared<ast::IfStmt>(Branches);
}


#pragma mark - Expressions

std::shared_ptr<ast::MemberAccess> TemplateResolver::Specialize(std::shared_ptr<ast::MemberAccess> MemberAccess) {
    using MK = ast::MemberAccess::Member::MemberKind;
    
    std::vector<std::shared_ptr<ast::MemberAccess::Member>> SpecializedMembers;
    
    for (auto &Member : MemberAccess->Members) {
        std::shared_ptr<ast::MemberAccess::Member> NewMember = nullptr;
        switch (Member->Kind) {
            case MK::Initial_Identifier:
            case MK::MemberAttributeRead:
                NewMember = Member;
                break;
            
            case MK::Initial_FunctionCall:
                NewMember = std::make_shared<ast::MemberAccess::Member>(MK::Initial_FunctionCall, Specialize(Member->Data.Call));
                break;
            
            case MK::Initial_StaticCall:
                LKFatalError("TODO");
            
            case MK::OffsetRead:
                NewMember = std::make_shared<ast::MemberAccess::Member>(Member->Kind, Specialize(Member->Data.Offset));
                break;
            
            case MK::MemberFunctionCall:
                LKFatalError("TODO");
        }
        
        precondition(NewMember);
        SpecializedMembers.push_back(NewMember);
    }
    return std::make_shared<ast::MemberAccess>(SpecializedMembers);
}



std::shared_ptr<ast::FunctionCall> TemplateResolver::Specialize(std::shared_ptr<ast::FunctionCall> Call) {
    auto InstantiatedCall = std::make_shared<ast::FunctionCall>(*Call);
    InstantiatedCall->Arguments = util::vector::map(Call->Arguments, [this](auto &Expr) -> auto {
        return Specialize(Expr);
    });
    InstantiatedCall->ExplicitTemplateArgumentTypes = util::vector::map(Call->ExplicitTemplateArgumentTypes, [this](auto &T) -> auto {
        return ResolveType(T);
    });
    
    return InstantiatedCall;
}


std::shared_ptr<ast::MatchExpr> TemplateResolver::Specialize(std::shared_ptr<ast::MatchExpr> MatchExpr) {
    // TODO file a radar ?!
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-lambda-capture"
    auto Branches = util::vector::map(MatchExpr->Branches, [this](auto Branch) -> auto {
        auto Patterns = util::vector::map(Branch->Patterns, [this](auto Pattern) -> auto { return Specialize(Pattern); });
        return std::make_shared<ast::MatchExpr::MatchExprBranch>(Patterns, Specialize(Branch->Expression));
    });
#pragma clang diagnostic pop
    return std::make_shared<ast::MatchExpr>(Specialize(MatchExpr->Target), Branches);
}


std::shared_ptr<ast::VariableDecl> TemplateResolver::Specialize(std::shared_ptr<ast::VariableDecl> Decl) {
    return std::make_shared<ast::VariableDecl>(Decl->Name, ResolveType(Decl->Type), Specialize(Decl->InitialValue));
}



std::shared_ptr<ast::Comparison> TemplateResolver::Specialize(std::shared_ptr<ast::Comparison> Comparison) {
    return std::make_shared<ast::Comparison>(Comparison->Op, Specialize(Comparison->LHS), Specialize(Comparison->RHS));
}


std::shared_ptr<ast::BinaryOperation> TemplateResolver::Specialize(std::shared_ptr<ast::BinaryOperation> Binop) {
    return std::make_shared<ast::BinaryOperation>(Binop->Op, Specialize(Binop->LHS), Specialize(Binop->RHS));
}


std::shared_ptr<ast::LogicalOperation> TemplateResolver::Specialize(std::shared_ptr<ast::LogicalOperation> LogicalOperation) {
    return std::make_shared<ast::LogicalOperation>(LogicalOperation->Op, Specialize(LogicalOperation->LHS), Specialize(LogicalOperation->RHS));
}
