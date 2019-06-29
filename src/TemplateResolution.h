//
//  TemplateResolution.h
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
#include "TypeInfo.h"


NS_START(yo::irgen)

class TemplateResolver {
    using TemplateTypeMapping = std::map<std::string, TypeInfo *>;
    const TemplateTypeMapping templateArgumentMapping;
    
    explicit TemplateResolver(TemplateTypeMapping templateArgumentMapping) : templateArgumentMapping(templateArgumentMapping) {}
    
    TypeInfo *resolveType(TypeInfo *TI);
    
    std::shared_ptr<ast::FunctionDecl> specialize(std::shared_ptr<ast::FunctionDecl>);
    
    std::shared_ptr<ast::LocalStmt> specialize(std::shared_ptr<ast::LocalStmt>);
    std::shared_ptr<ast::Expr> specialize(std::shared_ptr<ast::Expr>);
    
    std::shared_ptr<ast::VariableDecl> specialize(std::shared_ptr<ast::VariableDecl>);
    std::shared_ptr<ast::Assignment> specialize(std::shared_ptr<ast::Assignment>);
    std::shared_ptr<ast::ReturnStmt> specialize(std::shared_ptr<ast::ReturnStmt>);
    std::shared_ptr<ast::WhileStmt> specialize(std::shared_ptr<ast::WhileStmt>);
    std::shared_ptr<ast::IfStmt> specialize(std::shared_ptr<ast::IfStmt>);
    std::shared_ptr<ast::Composite> specialize(std::shared_ptr<ast::Composite>);
    std::shared_ptr<ast::ExprStmt> specialize(std::shared_ptr<ast::ExprStmt>);
    std::shared_ptr<ast::MatchExpr> specialize(std::shared_ptr<ast::MatchExpr>);
    
    std::shared_ptr<ast::CallExpr> specialize(std::shared_ptr<ast::CallExpr>);
    std::shared_ptr<ast::SubscriptExpr> specialize(std::shared_ptr<ast::SubscriptExpr>);
    std::shared_ptr<ast::MemberExpr> specialize(std::shared_ptr<ast::MemberExpr>);
    std::shared_ptr<ast::Comparison> specialize(std::shared_ptr<ast::Comparison>);
    std::shared_ptr<ast::BinaryOperation> specialize(std::shared_ptr<ast::BinaryOperation>);
    std::shared_ptr<ast::LogicalOperation> specialize(std::shared_ptr<ast::LogicalOperation>);
    
public:
    static std::shared_ptr<ast::FunctionDecl> specializeWithTemplateMapping(std::shared_ptr<ast::FunctionDecl>, TemplateTypeMapping);
};

NS_END
