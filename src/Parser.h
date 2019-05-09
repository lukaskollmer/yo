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
#include "Attributes.h"


NS_START(yo::parser)


// Operator precedence groups, in increasing order
enum class PrecedenceGroup : uint8_t {
    Initial = 0,
    FunctionPipeline,
    
    LogicalDisjunction,
    LogicalConjunction,
    Comparison,
    Casting,
    Addition,
    Multiplication,
    Bitshift,
    
    PrefixOperator
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
    
    std::vector<yo::attributes::Attribute> ParseAttributes();
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
    std::shared_ptr<ast::ForLoop> ParseForLoop();
    
    std::shared_ptr<ast::Expr> ParseExpression(PrecedenceGroup CurrentPrecedenceGroup = PrecedenceGroup::Initial);
    
    
    // Parses a CallExpr
    // Precondition: The current token most be either a less than sign or opening parentheses
    // If the current token is a less than sign and ParseCallExpr fails to parse a list of type expressions, it returns nullptr, with the parser's position reset to the less than sign
    std::shared_ptr<ast::CallExpr> ParseCallExpr(std::shared_ptr<ast::Expr> target);
    
    
    std::vector<std::shared_ptr<ast::Expr>> ParseExpressionList(Token::TokenKind Delimiter);
    std::shared_ptr<ast::Identifier> ParseIdentifier();
    
    std::shared_ptr<ast::MatchExpr> ParseMatchExpr();
    
    std::shared_ptr<ast::NumberLiteral> ParseNumberLiteral();
    std::shared_ptr<ast::StringLiteral> ParseStringLiteral();
    std::shared_ptr<ast::UnaryExpr> ParseUnaryExpr();
    
    // Why do these return optional?
    // Binop and Comparison operators can have the same initial token (ie, << and <, or & and &&)
    std::optional<ast::BinaryOperation::Operation> ParseBinopOperator();
    std::optional<ast::Comparison::Operation> ParseComparisonOperator();
    std::optional<ast::LogicalOperation::Operation> ParseLogicalOperationOperator();
};

NS_END
