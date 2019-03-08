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


#define save_pos(name) auto name = Position;
#define restore_pos(name) Position = name;

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
    if (CurrentTokenKind() != TK::Identifier) return nullptr;
    auto R = std::make_shared<Identifier>(CurrentToken().getData().s);
    Consume();
    return R;
}


class TokenSet {
    std::vector<TK> Tokens;

public:
    TokenSet(std::initializer_list<TK> Tokens) : Tokens(Tokens) {}
    
    bool Contains(TK Token) {
        return util::vector::contains(Tokens, Token);
    }
};


template <typename T>
class MappedTokenSet {
    std::map<TK, T> Mapping;
    
public:
    MappedTokenSet(std::initializer_list<std::pair<TK, T>> Mapping) {
        for (auto &Pair : Mapping) {
            this->Mapping.insert(Pair);
        }
    }
    
    bool Contains(TK Token) {
        return Mapping.find(Token) != Mapping.end();
    }
    
    T &operator [](TK Token) {
        return Mapping.at(Token);
    }
};


static TokenSet BinopOperatorTokens = {
    TK::Plus, TK::Minus, TK::Asterisk, TK::ForwardSlash, TK::PercentageSign
};




// The initial tokens of all binop operators
static TokenSet BinopOperatorStartTokens = {
    TK::Plus, TK::Minus, TK::Asterisk, TK::ForwardSlash, TK::PercentageSign,
    TK::Ampersand, TK::Pipe, TK::Circumflex, TK::LessThanSign, TK::GreaterSign
};




static MappedTokenSet<ast::BinaryOperation::Operation> SingleTokenBinopOperatorTokenMapping = {
    { TK::Plus,           BinaryOperation::Operation::Add },
    { TK::Minus,          BinaryOperation::Operation::Sub },
    { TK::Asterisk,       BinaryOperation::Operation::Mul },
    { TK::ForwardSlash,   BinaryOperation::Operation::Div },
    { TK::PercentageSign, BinaryOperation::Operation::Mod },
    { TK::Ampersand,      BinaryOperation::Operation::And },
    { TK::Pipe,           BinaryOperation::Operation::Or  },
    { TK::Circumflex,     BinaryOperation::Operation::Xor }
};


ast::BinaryOperation::Operation Parser::ParseBinopOperator() {
    auto T = CurrentTokenKind();
    if (!BinopOperatorStartTokens.Contains(T)) throw;
    
    if (SingleTokenBinopOperatorTokenMapping.Contains(T)) {
        Consume();
        return SingleTokenBinopOperatorTokenMapping[T];
    }
    
    if (T == TK::LessThanSign && PeekKind() == TK::LessThanSign) {
        Consume(2);
        return BinaryOperation::Operation::Shl;
    }
    
    if (T == TK::GreaterSign && PeekKind() == TK::GreaterSign) {
        Consume(2);
        return BinaryOperation::Operation::Shr;
    }
    
    // should never reach here
    throw;
}





PrecedenceGroup GetOperatorPrecedenceGroup(BinaryOperation::Operation Op) {
    switch (Op) {
    case BinaryOperation::Operation::Add:
    case BinaryOperation::Operation::Sub:
    case BinaryOperation::Operation::Or:
    case BinaryOperation::Operation::Xor:
        return PrecedenceGroup::Addition;
    case BinaryOperation::Operation::Mul:
    case BinaryOperation::Operation::Div:
    case BinaryOperation::Operation::Mod:
    case BinaryOperation::Operation::And:
        return PrecedenceGroup::Multiplication;
    case BinaryOperation::Operation::Shl:
    case BinaryOperation::Operation::Shr:
        return PrecedenceGroup::Bitshift;
    }
    
    throw;
}




// Tokens that, if they appear on their own, mark the end of an expression
static TokenSet ExpressionDelimitingTokens = {
    TK::ClosingParens, TK::Semicolon, TK::Comma,
};


std::shared_ptr<Expr> Parser::ParseExpression(std::shared_ptr<Expr> Context, PrecedenceGroup PrecedenceGroupConstraint) {
    std::shared_ptr<Expr> E;
    
    if (CurrentTokenKind() == TK::OpeningParens) {
        Consume();
        E = ParseExpression();
        assert_current_token_and_consume(TK::ClosingParens);
    }
    
    if (!E) {
        E = ParseNumberLiteral();
    }
    if (!E) {
        E = ParseIdentifier();
    }
    
    if (auto Ident = std::dynamic_pointer_cast<Identifier>(E) && CurrentTokenKind() == TK::OpeningParens) {
        // Function call
        Consume();
        auto Arguments = ParseExpressionList(TK::ClosingParens);
        assert_current_token_and_consume(TK::ClosingParens);
        E = std::make_shared<FunctionCall>(E, Arguments, false);
    }
    
    while (BinopOperatorStartTokens.Contains(CurrentTokenKind())) {
        save_pos(fallback)
        auto Op = ParseBinopOperator();
        auto Op_Precedence = GetOperatorPrecedenceGroup(Op);
        
        if (Op_Precedence > PrecedenceGroupConstraint) {
            auto RHS = ParseExpression(nullptr, Op_Precedence);
            E = std::make_shared<BinaryOperation>(Op, E, RHS);
        } else {
            restore_pos(fallback)
            return E;
        }
    }
    
    if (ExpressionDelimitingTokens.Contains(CurrentTokenKind())) {
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





