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
    ast::AST parse(std::string &filepath);
    
    void setCustomStdlibRoot(const std::string &path) {
        useCustomStdlibRoot = true;
        customStdlibRoot = path;
    }
    
private:
    TokenList tokens;
    uint64_t position;
    std::vector<std::string> importedFiles;
    
    // TODO replace these two w/ a single std::optional?
    bool useCustomStdlibRoot = false;
    std::string customStdlibRoot;
    
    void resolveImport();
    std::string resolveImportPathRelativeToBaseDirectory(const std::string &moduleName, const std::string &baseDirectory);
    
    Token &currentToken() { return *tokens[position]; }
    Token &nextToken() { return *tokens[++position]; }
    
    Token::TokenKind currentTokenKind() {
        return currentToken().kind;
    }
    Token &peek(uint64_t offset = 1) {
        return *tokens[position + offset];
    }
    Token::TokenKind &peekKind(uint64_t offset = 1) {
        return peek(offset).kind;
    }
    void consume(uint64_t count = 1) { position += count; }
    
    TokenSourceLocation &getCurrentSourceLocation() {
        return currentToken().sourceLocation;
    }
    
    
    
    std::shared_ptr<ast::TopLevelStmt> parseTopLevelStmt();
    
    std::vector<yo::attributes::Attribute> parseAttributes();
    std::shared_ptr<ast::FunctionSignature> parseFunctionSignature(std::shared_ptr<attributes::FunctionAttributes>);
    
    std::shared_ptr<ast::FunctionDecl> parseFunctionDecl(std::shared_ptr<attributes::FunctionAttributes>);
    std::shared_ptr<ast::ImplBlock> parseImplBlock();
    std::shared_ptr<ast::StructDecl> parseStructDecl(std::shared_ptr<attributes::StructAttributes>);
    std::shared_ptr<ast::TypealiasDecl> parseTypealias();
    
    void parseFunctionParameterList(std::shared_ptr<ast::FunctionSignature> &signature);
    
    std::vector<std::shared_ptr<ast::VariableDecl>> parseStructPropertyDeclList();
    
    TypeInfo *parseType();
    
    std::shared_ptr<ast::Composite> parseComposite();
    
    std::shared_ptr<ast::LocalStmt> parseLocalStmt();
    std::shared_ptr<ast::ReturnStmt> parseReturnStmt();
    std::shared_ptr<ast::VariableDecl> parseVariableDecl();
    
    std::shared_ptr<ast::IfStmt> parseIfStmt();
    std::shared_ptr<ast::WhileStmt> parseWhileStmt();
    std::shared_ptr<ast::ForLoop> parseForLoop();
    
    std::shared_ptr<ast::Expr> parseExpression(PrecedenceGroup currentPrecedenceGroup = PrecedenceGroup::Initial);
    
    
    // Parses a CallExpr
    // Precondition: The current token most be either a less than sign or opening parentheses
    // If the current token is a less than sign and ParseCallExpr fails to parse a list of type expressions, it returns nullptr, with the parser's position reset to the less than sign
    std::shared_ptr<ast::CallExpr> parseCallExpr(std::shared_ptr<ast::Expr> target);
    
    
    std::vector<std::shared_ptr<ast::Expr>> parseExpressionList(Token::TokenKind delimiter);
    std::shared_ptr<ast::Identifier> parseIdentifier();
    
    std::shared_ptr<ast::MatchExpr> parseMatchExpr();
    
    std::shared_ptr<ast::NumberLiteral> parseNumberLiteral();
    std::shared_ptr<ast::StringLiteral> parseStringLiteral();
    std::shared_ptr<ast::UnaryExpr> parseUnaryExpr();
    
    // Why do these return optional?
    // Binop and Comparison operators can have the same initial token (ie, << and <, or & and &&)
    std::optional<ast::BinaryOperation::Operation> parseBinopOperator();
    std::optional<ast::Comparison::Operation> parseComparisonOperator();
    std::optional<ast::LogicalOperation::Operation> parseLogicalOperationOperator();
};

NS_END
