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


NS_START(irgen)

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
    std::shared_ptr<ast::Composite> Specialize(std::shared_ptr<ast::Composite>);
    std::shared_ptr<ast::MemberAccess> Specialize(std::shared_ptr<ast::MemberAccess>);
    std::shared_ptr<ast::FunctionCall> Specialize(std::shared_ptr<ast::FunctionCall>);
    std::shared_ptr<ast::BinaryOperation> Specialize(std::shared_ptr<ast::BinaryOperation>);
    std::shared_ptr<ast::Comparison> Specialize(std::shared_ptr<ast::Comparison>);
    
public:
    static std::shared_ptr<ast::FunctionDecl> SpecializeWithTemplateMapping(std::shared_ptr<ast::FunctionDecl>, TemplateTypeMapping);
};

NS_END