//
//  Token.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"

#include <string>
#include <iostream>
#include <memory>
#include <variant>

// TODO rename Token -> SourceToken, Kind -> Token?

NS_START(yo::parser)

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
        Period, Hashtag, Tilde,
        Comma, Colon, Semicolon, EqualsSign, ExclamationMark,
        
        Asterisk, Plus, Minus, ForwardSlash, PercentageSign,
        Ampersand, Pipe, Circumflex, LessThanSign, GreaterSign,
        
        // Keywords
        Use,
        Fn, Struct, Impl,
        Let, Return,
        If, Else, While, For, In, Match,
        As, Using
    };
    
    std::variant<
        bool,
        char,
        double,
        uint64_t,
        std::string
    > Data;
    
    TokenKind Kind;
    TokenSourceLocation SourceLocation;
    
    
    Token(TokenKind Kind) : Kind(Kind) {}
    
    static std::shared_ptr<Token> WithKind(TokenKind Kind) {
        return std::make_shared<Token>(Kind);
    }
    
    static std::shared_ptr<Token> Identifier(std::string Name) {
        auto T = WithKind(TokenKind::Identifier);
        T->Data = Name;
        return T;
    }
    
    static std::shared_ptr<Token> IntegerLiteral(uint64_t Value) {
        auto T = WithKind(TokenKind::IntegerLiteral);
        T->Data = Value;
        return T;
    }
    
    static std::shared_ptr<Token> StringLiteral(std::string Value) {
        auto T = WithKind(TokenKind::StringLiteral);
        T->Data = Value;
        return T;
    }
    
    static std::shared_ptr<Token> CharLiteral(char Value) {
        auto T = WithKind(TokenKind::CharLiteral);
        T->Data = Value;
        return T;
    }
    
    static std::shared_ptr<Token> BoolLiteral(bool Value) {
        auto T = WithKind(TokenKind::BoolLiteral);
        T->Data = Value;
        return T;
    }
};

std::ostream &operator<<(std::ostream &OS, Token &T);
std::ostream &operator<<(std::ostream &OS, Token::TokenKind TK);


NS_END
