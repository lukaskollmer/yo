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

#include "TypeInfo.h"
#include "Token.h"
#include "util.h"
#include "Lexer.h"
#include "AST.h"

class Parser {
public:
    Parser() {}
    ast::AST Parse(TokenList Tokens);
    
private:
    TokenList Tokens;
    uint64_t Position;
    
    inline Token &CurrentToken() { return *Tokens[Position]; }
    inline Token &NextToken() { return *Tokens[++Position]; }
    
    inline Token::TokenKind CurrentTokenKind() { return CurrentToken().getKind(); }
    
    inline Token &Peek(uint64_t Offset = 1) { return *Tokens[Position + Offset]; }
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
    
    std::shared_ptr<ast::TopLevelStmt> ParseTopLevelStmt();
    
    void ParseFunctionSignatureInto(std::shared_ptr<ast::FunctionSignature> S);
    
    std::shared_ptr<ast::FunctionDecl> ParseFunctionDecl();
    std::shared_ptr<ast::ExternFunctionDecl> ParseExternFunctionDecl();
    
    std::vector<std::shared_ptr<ast::VariableDecl>> ParseParameterList();
    
    TypeInfo *ParseType();
    
    std::shared_ptr<ast::Composite> ParseComposite();
    
    std::shared_ptr<ast::LocalStmt> ParseLocalStmt();
    std::shared_ptr<ast::ReturnStmt> ParseReturnStmt();
    
    std::shared_ptr<ast::Expr> ParseExpression();
    std::shared_ptr<ast::Identifier> ParseIdentifier();
    
    
    std::shared_ptr<ast::NumberLiteral> ParseNumberLiteral();
    
    
    //std::vector<std::shared_ptr<ast::VariableDeclAST>> ParseVariableDeclList();
    
};
