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
        Struct, Impl,
        Fn, Extern,
        Let, Return,
        If, Else,
        As,
    };
    
    union TokenData {
        char C;
        double D;
        int64_t I;
        std::string S;
        
        // TODO: it would be interesting to know how many tokens actually use the S field
        // (identifiers and string literals). if it's a minority, it _might_ make sense to turn the S field
        // into a pointer, which would slightly decrease `sizeof(TokenData)` (24 -> 8 bytes)
        // Not sure whether this actually matters, but it might start adding up eventually when parsing large files?
        
        TokenData() : I(0) {}
        ~TokenData() {}
    };
    
    TokenKind Kind;
    TokenData Data;
    TokenSourceLocation SourceLocation;
    
    
    Token(TokenKind Kind) : Kind(Kind) {}
    ~Token() {
        if (Kind == TokenKind::Identifier
            || Kind == TokenKind::StringLiteral
            || Kind == TokenKind::ByteStringLiteral) {
            Data.S.~basic_string();
        }
    }
    
    static std::shared_ptr<Token> WithKind(TokenKind Kind) {
        return std::make_shared<Token>(Kind);
    }
    
    static std::shared_ptr<Token> Identifier(std::string Name) {
        auto T = WithKind(TokenKind::Identifier);
        T->Data.S = Name;
        return T;
    }
    
    static std::shared_ptr<Token> IntegerLiteral(int64_t Value) {
        auto T = WithKind(TokenKind::IntegerLiteral);
        T->Data.I = Value;
        return T;
    }
    
    static std::shared_ptr<Token> StringLiteral(std::string Value) {
        auto T = WithKind(TokenKind::StringLiteral);
        T->Data.S = Value;
        return T;
    }
    
    static std::shared_ptr<Token> CharLiteral(char Value) {
        auto T = WithKind(TokenKind::CharLiteral);
        T->Data.C = Value;
        return T;
    }
};

std::ostream &operator<<(std::ostream &OS, Token &T);
std::ostream &operator<<(std::ostream &OS, Token::TokenKind TK);


