//
//  Parser.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-03.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"
#include "Token.h"
#include "Lexer.h"
#include "AST.h"
#include "Attributes.h"

#include <memory>
#include <vector>
#include <initializer_list>
#include <optional>


NS_START(yo::parser)


// Operator precedence groups, in increasing order
enum class PrecedenceGroup : uint8_t {
    Initial = 0,
    FunctionPipeline,
    
    Assignment,
    
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
    ast::AST parse(const std::string &filepath);
    
    void setCustomStdlibRoot(const std::string &path) {
        customStdlibRoot = path;
    }
    
private:
    std::vector<Token> tokens;
    uint64_t position;
    std::vector<std::string> importedFiles;
    std::optional<std::string> customStdlibRoot;
    
    void resolveImport();
    std::string resolveImportPathRelativeToBaseDirectory(const TokenSourceLocation&, const std::string &moduleName, const std::string &baseDirectory);
    
    const Token& currentToken() { return tokens[position]; }
    
    Token::TokenKind currentTokenKind() {
        return currentToken().getKind();
    }
    const Token& peek(uint64_t offset = 1) {
        return tokens[position + offset];
    }
    Token::TokenKind peekKind(uint64_t offset = 1) {
        return peek(offset).getKind();
    }
    void consume(uint64_t count = 1) { position += count; }
    
    const TokenSourceLocation& getCurrentSourceLocation() {
        return currentToken().getSourceLocation();
    }
    
    
    // Error messages
    
    [[noreturn]]
    void unhandledToken(const Token &);
    void assertTk(Token::TokenKind expected);
    void assertTkAndConsume(Token::TokenKind expected) {
        assertTk(expected);
        consume();
    }
    
    
    // Parsing
    
    std::shared_ptr<ast::TopLevelStmt> parseTopLevelStmt();
    
    std::vector<yo::attributes::Attribute> parseAttributes();
    
    std::shared_ptr<ast::FunctionDecl> parseFunctionDecl(attributes::FunctionAttributes);
    std::shared_ptr<ast::ImplBlock> parseImplBlock();
    std::shared_ptr<ast::StructDecl> parseStructDecl(attributes::StructAttributes);
    std::shared_ptr<ast::TypealiasDecl> parseTypealias();
    
    void parseFunctionParameterList(ast::FunctionSignature&, std::vector<std::shared_ptr<ast::Ident>>&);
    
    std::vector<std::shared_ptr<ast::VarDecl>> parseStructPropertyDeclList();
    
    std::shared_ptr<ast::TypeDesc> parseType();
    
    std::shared_ptr<ast::Composite> parseComposite();
    
    std::shared_ptr<ast::LocalStmt> parseLocalStmt();
    std::shared_ptr<ast::ReturnStmt> parseReturnStmt();
    std::shared_ptr<ast::VarDecl> parseVariableDecl();
    
    std::shared_ptr<ast::IfStmt> parseIfStmt();
    std::shared_ptr<ast::WhileStmt> parseWhileStmt();
    std::shared_ptr<ast::ForLoop> parseForLoop();
    std::shared_ptr<ast::BreakContStmt> parseBreakOrContinueStmt();
    
    std::shared_ptr<ast::Expr> parseExpression(PrecedenceGroup currentPrecedenceGroup = PrecedenceGroup::Initial);
    
    std::optional<ast::Operator> parseOperator();
    
    
    // Parses a CallExpr
    // Precondition: The current token most be either a less than sign or opening parentheses
    // If the current token is a less than sign and ParseCallExpr fails to parse a list of type expressions, it returns nullptr, with the parser's position reset to the less than sign
    std::shared_ptr<ast::CallExpr> parseCallExpr(std::shared_ptr<ast::Expr> target);
    
    
    std::vector<std::shared_ptr<ast::Expr>> parseExpressionList(Token::TokenKind delimiter);
    
    std::string parseIdentAsString();
    std::shared_ptr<ast::Ident> parseIdent();
    
    std::shared_ptr<ast::MatchExpr> parseMatchExpr();
    
    std::shared_ptr<ast::NumberLiteral> parseNumberLiteral();
    std::shared_ptr<ast::StringLiteral> parseStringLiteral();
    std::shared_ptr<ast::UnaryExpr> parseUnaryExpr();
};

NS_END
