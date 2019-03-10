//
//  Token.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <string>
#include <iostream>
#include "util.h"
// TODO rename Token -> SourceToken, Kind -> Token?

class Token;
using TokenList = std::vector<std::shared_ptr<Token>>;

class Token {
public:
    enum class TokenKind {
        Unknown,
        EOF_,
        
        // Tokens w/ associated data
        Identifier,
        IntegerLiteral,
        
        OpeningParens,
        ClosingParens,
        OpeningCurlyBraces,
        ClosingCurlyBraces,
        OpeningSquareBrackets,
        ClosingSquareBrackets,
        Period,
        Comma, Colon, Semicolon, EqualsSign, ExclamationMark,
        
        Asterisk, Plus, Minus, ForwardSlash, PercentageSign,
        Ampersand, Pipe, Circumflex, LessThanSign, GreaterSign,
        
        // Keywords
        Fn, Extern,
        Let, Return,
        If, Else,
        As,
    };
    
    union TokenData {
        int64_t i;
        std::string s;
        
        TokenData() : i(0) {}
        ~TokenData() {}
    };
    
private:
    // TODO destructors in the union need to be called manually!!!
    
    TokenKind Kind;
    TokenData Data;
    Range SourceLocation;
    
public:
    Token(TokenKind Kind) : Kind(Kind) {}
    ~Token() {
        if (Kind == TokenKind::Identifier) {
            Data.s.~basic_string();
        }
    }
    
    static std::shared_ptr<Token> WithKind(TokenKind Kind) {
        return std::make_shared<Token>(Kind);
    }
    
    static std::shared_ptr<Token> Identifier(std::string Name) {
        auto T = WithKind(TokenKind::Identifier);
        T->Data.s = Name;
        return T;
    }
    
    static std::shared_ptr<Token> IntegerLiteral(int64_t Value) {
        auto T = WithKind(TokenKind::IntegerLiteral);
        T->Data.i = Value;
        return T;
    }
    
    auto &getKind() const { return Kind; }
    auto &getData() const { return Data; }
    auto &getSourceLocation() const { return SourceLocation; }
    
    void setSourceLocation(Range SourceLocation) {
        this->SourceLocation = SourceLocation;
    }
    
};

std::ostream &operator<<(std::ostream &OS, const Token &T);
std::ostream &operator<<(std::ostream &OS, const Token::TokenKind &TK);


