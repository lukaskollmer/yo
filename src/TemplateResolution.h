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
    const TemplateTypeMapping TemplateArgumentMapping;
    
    explicit TemplateResolver(TemplateTypeMapping TemplateArgumentMapping) : TemplateArgumentMapping(TemplateArgumentMapping) {}
    
    TypeInfo *ResolveType(TypeInfo *TI);
    
    std::shared_ptr<ast::FunctionDecl> Specialize(std::shared_ptr<ast::FunctionDecl>);
    
    std::shared_ptr<ast::LocalStmt> Specialize(std::shared_ptr<ast::LocalStmt>);
    std::shared_ptr<ast::Expr> Specialize(std::shared_ptr<ast::Expr>);
    
    std::shared_ptr<ast::VariableDecl> Specialize(std::shared_ptr<ast::VariableDecl>);
    std::shared_ptr<ast::Assignment> Specialize(std::shared_ptr<ast::Assignment>);
    std::shared_ptr<ast::ReturnStmt> Specialize(std::shared_ptr<ast::ReturnStmt>);
    std::shared_ptr<ast::WhileStmt> Specialize(std::shared_ptr<ast::WhileStmt>);
    std::shared_ptr<ast::IfStmt> Specialize(std::shared_ptr<ast::IfStmt>);
    std::shared_ptr<ast::Composite> Specialize(std::shared_ptr<ast::Composite>);
    std::shared_ptr<ast::NEW_ExprStmt> Specialize(std::shared_ptr<ast::NEW_ExprStmt>);
    std::shared_ptr<ast::MatchExpr> Specialize(std::shared_ptr<ast::MatchExpr>);
    
    std::shared_ptr<ast::CallExpr> Specialize(std::shared_ptr<ast::CallExpr>);
    std::shared_ptr<ast::SubscriptExpr> Specialize(std::shared_ptr<ast::SubscriptExpr>);
    std::shared_ptr<ast::MemberExpr> Specialize(std::shared_ptr<ast::MemberExpr>);
    std::shared_ptr<ast::Comparison> Specialize(std::shared_ptr<ast::Comparison>);
    std::shared_ptr<ast::BinaryOperation> Specialize(std::shared_ptr<ast::BinaryOperation>);
    std::shared_ptr<ast::LogicalOperation> Specialize(std::shared_ptr<ast::LogicalOperation>);
    
public:
    static std::shared_ptr<ast::FunctionDecl> SpecializeWithTemplateMapping(std::shared_ptr<ast::FunctionDecl>, TemplateTypeMapping);
};

NS_END
