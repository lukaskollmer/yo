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
#include <memory>
// TODO rename Token -> SourceToken, Kind -> Token?

class Token;
using TokenList = std::vector<std::shared_ptr<Token>>;

struct TokenSourceLocation {
    std::string Filename;
    uint64_t Line;
    uint64_t Column;
    uint64_t Length;
    
    TokenSourceLocation() {}
    
    TokenSourceLocation(std::string Filename, uint64_t Line, uint64_t Column, uint64_t Length) : Filename(Filename), Line(Line), Column(Column), Length(Length) {}
};


class Token {
public:
    enum class TokenKind {
        Unknown,
        EOF_,
        
        // Tokens w/ associated data
        Identifier,
        StringLiteral,
        ByteStringLiteral,
        CharLiteral,
        IntegerLiteral,
        DoubleLiteral,
        BoolLiteral,
        
        // Punctuation
        OpeningParens,
        ClosingParens,
        OpeningCurlyBraces,
        ClosingCurlyBraces,
        OpeningSquareBrackets,
        ClosingSquareBrackets,
        Period, Hashtag,
        Comma, Colon, Semicolon, EqualsSign, ExclamationMark,
        
        Asterisk, Plus, Minus, ForwardSlash, PercentageSign,
        Ampersand, Pipe, Circumflex, LessThanSign, GreaterSign,
        
        // Keywords
        Use,
        Struct, Impl,
        Fn, Extern,
        Let, Return,
        If, Else, While,
        As, Using
    };
    
    union TokenData {
        char C;
        double D;
        int64_t I;
        std::string *S;
        
        TokenData() : I(0) {}
        ~TokenData() {}
        
        static_assert(sizeof(I) == sizeof(S), "");
    };
    
    TokenKind Kind;
    TokenData Data;
    TokenSourceLocation SourceLocation;
    
    
    Token(TokenKind Kind) : Kind(Kind) {}
    ~Token() {
        if (Kind == TokenKind::Identifier
            || Kind == TokenKind::StringLiteral
            || Kind == TokenKind::ByteStringLiteral) {
            delete Data.S;
        }
    }
    
    static std::shared_ptr<Token> WithKind(TokenKind Kind) {
        return std::make_shared<Token>(Kind);
    }
    
    static std::shared_ptr<Token> Identifier(std::string Name) {
        auto T = WithKind(TokenKind::Identifier);
        T->Data.S = new std::string(Name);
        return T;
    }
    
    static std::shared_ptr<Token> IntegerLiteral(int64_t Value) {
        auto T = WithKind(TokenKind::IntegerLiteral);
        T->Data.I = Value;
        return T;
    }
    
    static std::shared_ptr<Token> StringLiteral(std::string Value) {
        auto T = WithKind(TokenKind::StringLiteral);
        T->Data.S = new std::string(Value);
        return T;
    }
    
    static std::shared_ptr<Token> CharLiteral(char Value) {
        auto T = WithKind(TokenKind::CharLiteral);
        T->Data.C = Value;
        return T;
    }
    
    static std::shared_ptr<Token> BoolLiteral(bool Value) {
        auto T = WithKind(TokenKind::BoolLiteral);
        T->Data.C = Value;
        return T;
    }
};

std::ostream &operator<<(std::ostream &OS, Token &T);
std::ostream &operator<<(std::ostream &OS, Token::TokenKind TK);
