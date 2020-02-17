//
//  TemplateSpecialization.h
//  yo
//
//  Created by Lukas Kollmer on 2019-04-20.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <memory>
#include <map>
#include <string>
#include <optional>

#include "AST.h"
#include "IRGen.h"


NS_START(yo::irgen)


class TemplateSpecializer {
public:
    using TmplParamMapping = std::map<std::string, std::shared_ptr<ast::TypeDesc>>;
    const TmplParamMapping templateArgumentMapping;
    
    template <typename T>
    static std::shared_ptr<T> specializeWithMapping(std::shared_ptr<T> node, TmplParamMapping M) {
        return TemplateSpecializer(M).specialize(node);
    }
    
    
    explicit TemplateSpecializer(TmplParamMapping templateArgumentMapping) : templateArgumentMapping(templateArgumentMapping) {}
    
    
    std::shared_ptr<ast::TypeDesc> resolveType(std::shared_ptr<ast::TypeDesc>);
    
    std::shared_ptr<ast::FunctionDecl> specialize(std::shared_ptr<ast::FunctionDecl>);
    std::shared_ptr<ast::StructDecl> specialize(std::shared_ptr<ast::StructDecl>);
    std::shared_ptr<ast::ImplBlock> specialize(std::shared_ptr<ast::ImplBlock>);
    
    std::shared_ptr<ast::LocalStmt> specialize(std::shared_ptr<ast::LocalStmt>);
    std::shared_ptr<ast::Expr> specialize(std::shared_ptr<ast::Expr>);
    
    std::shared_ptr<ast::VarDecl> specialize(std::shared_ptr<ast::VarDecl>);
    std::shared_ptr<ast::Assignment> specialize(std::shared_ptr<ast::Assignment>);
    std::shared_ptr<ast::ReturnStmt> specialize(std::shared_ptr<ast::ReturnStmt>);
    std::shared_ptr<ast::WhileStmt> specialize(std::shared_ptr<ast::WhileStmt>);
    std::shared_ptr<ast::IfStmt> specialize(std::shared_ptr<ast::IfStmt>);
    std::shared_ptr<ast::ForLoop> specialize(std::shared_ptr<ast::ForLoop>);
    std::vector<std::shared_ptr<ast::LocalStmt>> specialize(std::vector<std::shared_ptr<ast::LocalStmt>>);
    std::shared_ptr<ast::CompoundStmt> specialize(std::shared_ptr<ast::CompoundStmt>);
    std::shared_ptr<ast::ExprStmt> specialize(std::shared_ptr<ast::ExprStmt>);
    std::shared_ptr<ast::MatchExpr> specialize(std::shared_ptr<ast::MatchExpr>);
    
    std::shared_ptr<ast::CallExpr> specialize(std::shared_ptr<ast::CallExpr>);
    std::shared_ptr<ast::SubscriptExpr> specialize(std::shared_ptr<ast::SubscriptExpr>);
    std::shared_ptr<ast::MemberExpr> specialize(std::shared_ptr<ast::MemberExpr>);
    std::shared_ptr<ast::BinOp> specialize(std::shared_ptr<ast::BinOp>);
    std::shared_ptr<ast::UnaryExpr> specialize(std::shared_ptr<ast::UnaryExpr>);
    std::shared_ptr<ast::LambdaExpr> specialize(std::shared_ptr<ast::LambdaExpr>);
    std::shared_ptr<ast::StaticDeclRefExpr> specialize(std::shared_ptr<ast::StaticDeclRefExpr>);
    
    std::shared_ptr<ast::TemplateParamArgList> specialize(std::shared_ptr<ast::TemplateParamArgList>);
    std::shared_ptr<ast::TemplateParamDeclList> specialize(std::shared_ptr<ast::TemplateParamDeclList>);
};

NS_END
