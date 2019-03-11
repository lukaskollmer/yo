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
    ast::AST Parse(TokenList Tokens);
    
private:
    TokenList Tokens;
    uint64_t Position;
    
    inline Token &CurrentToken() { return *Tokens[Position]; }
    inline Token &NextToken() { return *Tokens[++Position]; }
    
    inline Token::TokenKind CurrentTokenKind() {
        return CurrentToken().getKind();
    }
    inline const Token &Peek(uint64_t Offset = 1) {
        return *Tokens[Position + Offset];
    }
    inline const Token::TokenKind &PeekKind(uint64_t Offset = 1) {
        return Peek(Offset).getKind();
    }
    inline void Consume(uint64_t Count = 1) { Position += Count; }
    
    void assert_current_token(Token::TokenKind Expected) {
        if (CurrentToken().getKind() != Expected) {
            std::cout << "token assert failed. expected: " << Expected << " got: " << CurrentTokenKind() << std::endl;
            throw "FUCK";
        }
    }
    
    void assert_current_token_and_consume(Token::TokenKind Expected) {
        if (CurrentToken().getKind() != Expected) {
            std::cout << "token assert failed. expected: " << Expected << " got: " << CurrentTokenKind() << std::endl;
            throw "FUCK";
        } else {
            Consume();
        }
    }
    
    
    void assert_current_token_either(std::vector<Token::TokenKind> TokenKinds) {
        auto Current = CurrentTokenKind();
        for (auto &TK : TokenKinds) {
            if (TK == Current) return;
        }
        throw;
        //std::cout << "[assert_current_]" << std::endl;
    }
    
    std::shared_ptr<ast::TopLevelStmt> ParseTopLevelStmt();
    
    void ParseFunctionSignatureInto(std::shared_ptr<ast::FunctionSignature> S);
    
    std::shared_ptr<ast::FunctionDecl> ParseFunctionDecl();
    std::shared_ptr<ast::ExternFunctionDecl> ParseExternFunctionDecl();
    std::shared_ptr<ast::StructDecl> ParseStructDecl();
    std::shared_ptr<ast::ImplBlock> ParseImplBlock();
    
    std::vector<std::shared_ptr<ast::VariableDecl>> ParseParameterList();
    
    TypeInfo *ParseType();
    
    std::shared_ptr<ast::Composite> ParseComposite();
    
    std::shared_ptr<ast::LocalStmt> ParseLocalStmt();
    std::shared_ptr<ast::ReturnStmt> ParseReturnStmt();
    std::shared_ptr<ast::VariableDecl> ParseVariableDecl();
    
    std::shared_ptr<ast::IfStmt> ParseIfStmt();
    
    std::shared_ptr<ast::Expr> ParseExpression(PrecedenceGroup CurrentPrecedenceGroup = PrecedenceGroup::Initial);
    std::vector<std::shared_ptr<ast::Expr>> ParseExpressionList(Token::TokenKind Delimiter);
    std::shared_ptr<ast::Identifier> ParseIdentifier();
    
    std::shared_ptr<ast::Expr> ParseMemberAccess();
    
    std::shared_ptr<ast::NumberLiteral> ParseNumberLiteral();
    
    // Why do these return optional?
    // Binop and Comparison operators can have the same initial token (ie, << and <, or & and &&)
    std::optional<ast::BinaryOperation::Operation> ParseBinopOperator();
    std::optional<ast::Comparison::Operation> ParseComparisonOperator();
    std::optional<ast::LogicalOperation::Operation> ParseLogicalOperationOperator();
};
