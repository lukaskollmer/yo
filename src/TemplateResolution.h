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
//#include "TypeInfo.h"
#include "IRGen.h"


NS_START(yo::irgen)

class TemplateResolver {
public:
    using TemplateTypeMapping = std::map<std::string, std::shared_ptr<ast::TypeDesc>>;
    
    static std::shared_ptr<ast::FunctionDecl> specializeWithTemplateMapping(std::shared_ptr<ast::FunctionDecl>, TemplateTypeMapping);
    
private:
    const TemplateTypeMapping templateArgumentMapping;
    //const IRGenerator &irgen;
    
    explicit TemplateResolver(/*const IRGenerator &irgen,*/ TemplateTypeMapping templateArgumentMapping) : /*irgen(irgen),*/ templateArgumentMapping(templateArgumentMapping) {}
    
    //TypeInfo *resolveType(TypeInfo *TI);
    std::shared_ptr<ast::TypeDesc> resolveType(std::shared_ptr<ast::TypeDesc>);
    
    std::shared_ptr<ast::FunctionDecl> specialize(std::shared_ptr<ast::FunctionDecl>);
    
    std::shared_ptr<ast::LocalStmt> specialize(std::shared_ptr<ast::LocalStmt>);
    std::shared_ptr<ast::Expr> specialize(std::shared_ptr<ast::Expr>);
    
    std::shared_ptr<ast::VarDecl> specialize(std::shared_ptr<ast::VarDecl>);
    std::shared_ptr<ast::Assignment> specialize(std::shared_ptr<ast::Assignment>);
    std::shared_ptr<ast::ReturnStmt> specialize(std::shared_ptr<ast::ReturnStmt>);
    std::shared_ptr<ast::WhileStmt> specialize(std::shared_ptr<ast::WhileStmt>);
    std::shared_ptr<ast::IfStmt> specialize(std::shared_ptr<ast::IfStmt>);
    std::vector<std::shared_ptr<ast::LocalStmt>> specialize(std::vector<std::shared_ptr<ast::LocalStmt>>);
    std::shared_ptr<ast::Composite> specialize(std::shared_ptr<ast::Composite>);
    std::shared_ptr<ast::ExprStmt> specialize(std::shared_ptr<ast::ExprStmt>);
    std::shared_ptr<ast::MatchExpr> specialize(std::shared_ptr<ast::MatchExpr>);
    
    std::shared_ptr<ast::CallExpr> specialize(std::shared_ptr<ast::CallExpr>);
    std::shared_ptr<ast::SubscriptExpr> specialize(std::shared_ptr<ast::SubscriptExpr>);
    std::shared_ptr<ast::MemberExpr> specialize(std::shared_ptr<ast::MemberExpr>);
    std::shared_ptr<ast::BinOp> specialize(std::shared_ptr<ast::BinOp>);
    std::shared_ptr<ast::UnaryExpr> specialize(std::shared_ptr<ast::UnaryExpr>);
};

NS_END
