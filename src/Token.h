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
    std::string filepath;
    uint64_t line;
    uint64_t column;
    uint64_t length;
    
    TokenSourceLocation() {}
    TokenSourceLocation(const std::string &filepath, uint64_t line, uint64_t column, uint64_t length) : filepath(filepath), line(line), column(column), length(length) {}
};

inline std::ostream& operator<<(std::ostream &OS, const TokenSourceLocation &SL) {
    return OS << SL.filepath << ":" << SL.line << ":" << SL.column << ":" << SL.length;
}


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
        Comma, Colon, Semicolon, EqualsSign,
        ExclamationMark, QuestionMark,
        
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
    > data;
    
    TokenKind kind;
    TokenSourceLocation sourceLocation;
    
    
    Token(TokenKind kind) : kind(kind) {}
    
    static std::shared_ptr<Token> WithKind(TokenKind kind) {
        return std::make_shared<Token>(kind);
    }
    
    static std::shared_ptr<Token> Identifier(std::string name) {
        auto T = WithKind(TokenKind::Identifier);
        T->data = name;
        return T;
    }
    
    static std::shared_ptr<Token> IntegerLiteral(uint64_t value) {
        auto T = WithKind(TokenKind::IntegerLiteral);
        T->data = value;
        return T;
    }
    
    static std::shared_ptr<Token> StringLiteral(std::string value) {
        auto T = WithKind(TokenKind::StringLiteral);
        T->data = value;
        return T;
    }
    
    static std::shared_ptr<Token> CharLiteral(char value) {
        auto T = WithKind(TokenKind::CharLiteral);
        T->data = value;
        return T;
    }
    
    static std::shared_ptr<Token> BoolLiteral(bool value) {
        auto T = WithKind(TokenKind::BoolLiteral);
        T->data = value;
        return T;
    }
};

std::ostream &operator<<(std::ostream &OS, Token &T);
std::ostream &operator<<(std::ostream &OS, Token::TokenKind TK);


NS_END
