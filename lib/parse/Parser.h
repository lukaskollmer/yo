//
//  Parser.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-03.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util/util.h"
//#include "Token.h"
#include "lex/Lexer.h"
#include "AST.h"
#include "Attributes.h"

#include <memory>
#include <vector>
#include <initializer_list>
#include <optional>


namespace yo::parser {


// Operator precedence groups, in increasing order
enum class PrecedenceGroup : uint8_t {
    Initial = 0,
    FunctionPipeline,
    
    Assignment,
    
    LogicalDisjunction,
    LogicalConjunction,
    Comparison,
    Casting,
    RangeFormation,
    Addition,
    Multiplication,
    Bitshift,
    
    PrefixOperator,
    FunctionCall
};





class Parser {
public:
    Parser() {}
    ast::AST parse(const std::string &filepath);
    
    void setCustomStdlibRoot(const std::string &path) {
        customStdlibRoot = path;
    }
    
private:
    std::vector<lex::Token> tokens;
    int64_t position;
    std::vector<std::string> importedFiles;
    std::optional<std::string> customStdlibRoot;
    
    void resolveImport();
    std::string resolveImportPathRelativeToBaseDirectory(const lex::SourceLocation&, const std::string &moduleName, const std::string &baseDirectory);
    
    const lex::Token& currentToken() { return tokens[position]; }
    
    lex::TokenKind currentTokenKind() {
        return currentToken().getKind();
    }
    const lex::Token& peek(int64_t offset = 1) {
        return tokens[position + offset];
    }
    lex::TokenKind peekKind(int64_t offset = 1) {
        return peek(offset).getKind();
    }
    void consume(int64_t count = 1) { position += count; }
    
    const lex::SourceLocation& getCurrentSourceLocation() {
        return currentToken().getSourceLocation();
    }
    
    const lex::SourceLocation& getSourceLocation(int64_t offset = 0) {
        return tokens[position + offset].getSourceLocation();
    }
    
    
    // Error messages
    
    [[noreturn]]
    void unhandledToken(const lex::Token &);
    
    void assertTk(lex::TokenKind expected);
    
    void assertTkAndConsume(lex::TokenKind expected) {
        assertTk(expected);
        consume();
    }
    
    
    // Parsing
    
    std::shared_ptr<ast::TopLevelStmt> parseTopLevelStmt();
    
    std::vector<yo::attributes::Attribute> parseAttributes();
    
    std::shared_ptr<ast::FunctionDecl> parseFunctionDecl(attributes::FunctionAttributes);
    std::shared_ptr<ast::ImplBlock> parseImplBlock();
    std::shared_ptr<ast::TemplateParamDeclList> parseTemplateParamDeclList();
    std::shared_ptr<ast::StructDecl> parseStructDecl(attributes::StructAttributes);
    std::shared_ptr<ast::TypealiasDecl> parseTypealias();
    std::shared_ptr<ast::VariantDecl> parseVariantDecl();
    
    void parseFunctionSignatureAndParamNames(ast::FunctionSignature&, std::vector<std::shared_ptr<ast::Ident>>&);
    
    std::vector<std::shared_ptr<ast::VarDecl>> parseStructPropertyDeclList();
    
    std::shared_ptr<ast::TypeDesc> parseType();
    
    std::shared_ptr<ast::CompoundStmt> parseCompoundStmt();
    
    std::shared_ptr<ast::LocalStmt> parseLocalStmt();
    std::shared_ptr<ast::ReturnStmt> parseReturnStmt();
    std::shared_ptr<ast::VarDecl> parseVariableDecl();
    
    std::shared_ptr<ast::IfStmt> parseIfStmt();
    std::shared_ptr<ast::WhileStmt> parseWhileStmt();
    std::shared_ptr<ast::ForLoop> parseForLoop();
    std::shared_ptr<ast::BreakContStmt> parseBreakOrContinueStmt();
    
    std::shared_ptr<ast::Expr> parseExpression(PrecedenceGroup currentPrecedenceGroup = PrecedenceGroup::Initial);
    
    /// includeFunctionDeclOperators controls whether operators that usually take arguments, like `[]` or `()` should be treated as valid operators, or not
    std::optional<ast::Operator> parseOperator(bool includeFunctionDeclOperators = false);
    
    
    // Parses a CallExpr
    // Precondition: The current token most be either a less than sign or opening parentheses
    // If the current token is a less than sign and ParseCallExpr fails to parse a list of type expressions, it returns nullptr, with the parser's position reset to the less than sign
    std::shared_ptr<ast::CallExpr> parseCallExpr(std::shared_ptr<ast::Expr> target);
    
    /// Parse an explicit template argument list
    /// For example everything between the target name and the first parentheses in `foo<i8, i8, i8>(<args>)`
    std::shared_ptr<ast::TemplateParamArgList> parseTemplateArgumentList();
    
    
    std::shared_ptr<ast::TupleExpr> parseTupleExpr();
    std::vector<std::shared_ptr<ast::Expr>> parseExpressionList(lex::TokenKind delimiter);
    
    std::shared_ptr<ast::ArrayLiteralExpr> parseArrayLiteral();
    
    std::string parseIdentAsString();
    std::shared_ptr<ast::Ident> parseIdent();
    
    std::shared_ptr<ast::MatchExpr> parseMatchExpr();
    ast::MatchExprPattern parseMatchExprPattern();
    
    std::shared_ptr<ast::LambdaExpr> parseLambdaExpr();
    
    std::shared_ptr<ast::NumberLiteral> parseNumberLiteral();
    std::shared_ptr<ast::StringLiteral> parseStringLiteral();
    std::shared_ptr<ast::UnaryExpr> parseUnaryExpr();
};

}
