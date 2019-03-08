//
//  Parser.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-03.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Parser.h"

#include <map>

using namespace ast;


using TK = Token::TokenKind;


// How does the parser work?
//
// Position always points to the current token.
// For example, if we parse an identifier, after returning from `ParseIdentifier`, Position would point to the token after that identifier

AST Parser::Parse(TokenList Tokens) {
    this->Tokens = Tokens;
    this->Position = 0;
    
    AST Ast;
    while (Position < Tokens.size() && CurrentToken().getKind() != TK::EOF_) {
        std::cout << CurrentToken() << std::endl;
        Ast.push_back(ParseTopLevelStmt());
    }
    
    return Ast;
}

#define unhandled_token(T) { std::cout << "Unhandled Token: " << T << std::endl; throw; }


std::shared_ptr<TopLevelStmt> Parser::ParseTopLevelStmt() {
    switch (CurrentToken().getKind()) {
        case TK::Fn: return ParseFunctionDecl();
        case TK::Extern: return ParseExternFunctionDecl();
        default: unhandled_token(CurrentToken());
    }
}



void Parser::ParseFunctionSignatureInto(std::shared_ptr<FunctionSignature> S) {
    assert_current_token_and_consume(TK::Fn);
    
    S->Name = ParseIdentifier()->Value;
    assert_current_token_and_consume(TK::OpeningParens);
    
    S->Parameters = ParseParameterList();
    assert_current_token_and_consume(TK::ClosingParens);
    assert_current_token_and_consume(TK::Colon);
    
    S->ReturnType = ParseType();
}



std::shared_ptr<ExternFunctionDecl> Parser::ParseExternFunctionDecl() {
    assert_current_token_and_consume(TK::Extern);
    
    auto EFD = std::make_shared<ExternFunctionDecl>();
    ParseFunctionSignatureInto(EFD);
    
    assert_current_token_and_consume(TK::Semicolon);
    
    return EFD;
}

std::shared_ptr<FunctionDecl> Parser::ParseFunctionDecl() {
    auto FD = std::make_shared<FunctionDecl>();
    
    ParseFunctionSignatureInto(FD);
    assert_current_token(TK::OpeningCurlyBraces);
    
    FD->Body = ParseComposite();
    
    return FD;
}



std::vector<std::shared_ptr<VariableDecl>> Parser::ParseParameterList() {
    std::vector<std::shared_ptr<VariableDecl>> Parameters;
    
    while (CurrentTokenKind() == TK::Identifier) {
        auto Ident = ParseIdentifier();
        assert_current_token_and_consume(TK::Colon);
        
        auto Type = ParseType();
        Parameters.push_back(std::make_shared<VariableDecl>(Ident, Type));
        
        if (CurrentTokenKind() == TK::Comma) {
            Consume();
        } else {
            break;
        }
    }
    
    return Parameters;
}


TypeInfo *Parser::ParseType() {
    if (CurrentTokenKind() == TK::Identifier) {
        auto Name = ParseIdentifier()->Value;
        // TODO Is the `?:` operator standard c++ ? is it guaranteed that rhs will only be evaluated if lhs was nil?
        return TypeInfo::GetBuiltinWithName(Name) ?: TypeInfo::MakeComplex(Name);
    }
    if (CurrentTokenKind() == TK::Asterisk) {
        Consume();
        return TypeInfo::MakePointer(ParseType());
    }
    throw 0;
}





std::shared_ptr<Composite> Parser::ParseComposite() {
    assert_current_token_and_consume(TK::OpeningCurlyBraces);
    
    auto C = std::make_shared<Composite>();
    
    while (CurrentTokenKind() != TK::ClosingCurlyBraces) {
        C->Statements.push_back(ParseLocalStmt());
    }
    
    assert_current_token_and_consume(TK::ClosingCurlyBraces);
    return C;
}






std::shared_ptr<LocalStmt> Parser::ParseLocalStmt() {
    if (CurrentTokenKind() == TK::Return) {
        return ParseReturnStmt();
    }
    
    if (CurrentTokenKind() == TK::Identifier && PeekKind(1) == TK::OpeningParens) {
        // Function Call
        auto Target = ParseIdentifier();
        assert_current_token_and_consume(TK::OpeningParens);
        
        auto Arguments = ParseExpressionList(TK::ClosingParens);
        assert_current_token_and_consume(TK::ClosingParens);
        
        if (CurrentTokenKind() == TK::Semicolon) {
            Consume();
            return std::make_shared<FunctionCall>(Target, Arguments, true);
        }
    }
    
    unhandled_token(CurrentToken())
}



std::shared_ptr<ReturnStmt> Parser::ParseReturnStmt() {
    assert_current_token_and_consume(TK::Return);
    
    auto Expr = ParseExpression();
    assert_current_token_and_consume(TK::Semicolon);
    return std::make_shared<ReturnStmt>(Expr);
}





std::shared_ptr<Identifier> Parser::ParseIdentifier() {
    assert_current_token(TK::Identifier);
    auto R = std::make_shared<Identifier>(CurrentToken().getData().s);
    Consume();
    return R;
}



static std::vector<Token::TokenKind> BinopOperators = {
    // TODO?
};



// Tokens that, if they appear on their own, mark the end of an expression
static std::vector<TK> ExpressionDelimitingTokens = {
    TK::ClosingParens
};


bool IsExprDelimitingToken(TK Token) {
    return util::vector::contains(ExpressionDelimitingTokens, Token);
}

std::shared_ptr<Expr> Parser::ParseExpression() {
    std::shared_ptr<Expr> E;
    
    if (!E) {
        E = ParseNumberLiteral();
    }
    
    if (IsExprDelimitingToken(CurrentTokenKind())) {
        return E;
    }
    
    if (CurrentTokenKind() == TK::Semicolon) {
        return E;
    }
    

    unhandled_token(CurrentToken())
}


// Parses a (potentially empty) list of expressions separated by commas, until Delimiter is reached
// The delimiter is not consumed
std::vector<std::shared_ptr<Expr>> Parser::ParseExpressionList(Token::TokenKind Delimiter) {
    if (CurrentTokenKind() == Delimiter) return {};
    
    std::vector<std::shared_ptr<Expr>> Expressions;
    
    do {
        Expressions.push_back(ParseExpression());
        assert_current_token_either({TK::Comma, Delimiter});
        if (CurrentTokenKind() == TK::Comma) Consume();
    } while (CurrentTokenKind() != Delimiter);
    
    assert_current_token(Delimiter);
    return Expressions;
}



std::shared_ptr<NumberLiteral> Parser::ParseNumberLiteral() {
    if (CurrentTokenKind() != TK::IntegerLiteral) {
        return nullptr;
    }
    
    auto Value = CurrentToken().getData().i;
    Consume();
    
    return std::make_shared<NumberLiteral>(Value);
}





