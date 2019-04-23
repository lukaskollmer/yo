//
//  Parser.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-03.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <memory>
#include <vector>
#include <initializer_list>
#include <optional>

#include "TypeInfo.h"
#include "Token.h"
#include "util.h"
#include "Lexer.h"
#include "AST.h"



// Operator precedence groups, in increasing order
enum class PrecedenceGroup : uint8_t {
    Initial = 0,
    
    LogicalDisjunction,
    LogicalConjunction,
    Comparison,
    Casting,
    Addition,
    Multiplication,
    Bitshift,
};





class Parser {
public:
    Parser() {}
    ast::AST Parse(std::string &FilePath);
    
private:
    TokenList Tokens;
    uint64_t Position;
    std::vector<std::string> ImportedFiles;
    
    void ResolveImport();
    std::string ResolveImportPathRelativeToBaseDirectory(const std::string &ModuleName, const std::string &BaseDirectory);
    
    Token &CurrentToken() { return *Tokens[Position]; }
    Token &NextToken() { return *Tokens[++Position]; }
    
    Token::TokenKind CurrentTokenKind() {
        return CurrentToken().Kind;
    }
    Token &Peek(uint64_t Offset = 1) {
        return *Tokens[Position + Offset];
    }
    Token::TokenKind &PeekKind(uint64_t Offset = 1) {
        return Peek(Offset).Kind;
    }
    void Consume(uint64_t Count = 1) { Position += Count; }
    
    
    
    std::shared_ptr<ast::TopLevelStmt> ParseTopLevelStmt();
    
    std::vector<std::string> ParseAnnotations();
    std::shared_ptr<ast::FunctionSignature> ParseFunctionSignature(bool IsExternal);
    
    std::shared_ptr<ast::FunctionDecl> ParseFunctionDecl();
    std::shared_ptr<ast::ExternFunctionDecl> ParseExternFunctionDecl();
    std::shared_ptr<ast::StructDecl> ParseStructDecl();
    std::shared_ptr<ast::ImplBlock> ParseImplBlock();
    std::shared_ptr<ast::TypealiasDecl> ParseTypealias();
    
    std::vector<std::shared_ptr<ast::VariableDecl>> ParseParameterList();
    
    TypeInfo *ParseType();
    
    std::shared_ptr<ast::Composite> ParseComposite();
    
    std::shared_ptr<ast::LocalStmt> ParseLocalStmt();
    std::shared_ptr<ast::ReturnStmt> ParseReturnStmt();
    std::shared_ptr<ast::VariableDecl> ParseVariableDecl();
    
    std::shared_ptr<ast::IfStmt> ParseIfStmt();
    std::shared_ptr<ast::WhileStmt> ParseWhileStmt();
    
    std::shared_ptr<ast::Expr> ParseExpression(PrecedenceGroup CurrentPrecedenceGroup = PrecedenceGroup::Initial);
    std::vector<std::shared_ptr<ast::Expr>> ParseExpressionList(Token::TokenKind Delimiter);
    std::shared_ptr<ast::Identifier> ParseIdentifier();
    
    std::shared_ptr<ast::Expr> ParseMemberAccess();
    
    std::shared_ptr<ast::NumberLiteral> ParseNumberLiteral();
    std::shared_ptr<ast::StringLiteral> ParseStringLiteral();
    
    // Why do these return optional?
    // Binop and Comparison operators can have the same initial token (ie, << and <, or & and &&)
    std::optional<ast::BinaryOperation::Operation> ParseBinopOperator();
    std::optional<ast::Comparison::Operation> ParseComparisonOperator();
    std::optional<ast::LogicalOperation::Operation> ParseLogicalOperationOperator();
};
