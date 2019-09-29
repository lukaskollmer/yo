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



NS_START(yo::parser)

struct TokenSourceLocation {
    std::string filepath;
    uint64_t line;
    uint64_t column;
    uint64_t length;
    
    TokenSourceLocation() : filepath{}, line(0), column(0), length(0) {}
    TokenSourceLocation(const std::string &filepath, uint64_t line, uint64_t column, uint64_t length) : filepath(filepath), line(line), column(column), length(length) {}
    
    
    bool empty() const {
        return filepath.empty() && line == 0 && column == 0 && length == 0;
    }
};

inline std::ostream& operator<<(std::ostream &OS, const TokenSourceLocation &SL) {
    return OS << SL.filepath << ":" << SL.line << ":" << SL.column;
}


class Token {
public:
    enum class TokenKind {
        Unknown,
        EOF_,
        
        // Tokens w/ associated data
        Ident,
        StringLiteral, // TODO rename to NormalStringLiteral or StdStringLiteral?
        ByteStringLiteral,
        CharLiteral,
        IntegerLiteral,
        DoubleLiteral,
        BoolLiteral,
        Whitespace,
        LineComment, BlockComment,
        
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
        As
    };
    
private:
    std::variant<
        bool,
        char,
        double,
        uint64_t,
        std::string
    > data;
    
    std::string sourceText;
    TokenKind kind;
    TokenSourceLocation sourceLocation;
    
public:
    Token() : kind(TokenKind::Unknown) {}
    explicit Token(std::string sourceText, TokenKind kind) : sourceText(sourceText), kind(kind) {}
    
    template <typename T>
    Token(std::string sourceText, TokenKind kind, T data) : data(data), sourceText(sourceText), kind(kind) {}
    
    
    const std::string& getSourceText() const { return sourceText; }
    
    TokenKind getKind() const { return kind; }
    void setKind(TokenKind kind) { this->kind = kind; }
    
    const TokenSourceLocation& getSourceLocation() const { return sourceLocation; }
    void setSourceLocation(TokenSourceLocation sourceLoc) { sourceLocation = sourceLoc; }
    
    template <typename T>
    void setData(T value) { data = value; }
    
    template <typename T>
    T getData() const { return std::get<T>(data); }
};

std::ostream &operator<<(std::ostream &OS, const Token &T);
std::ostream &operator<<(std::ostream &OS, const Token::TokenKind TK);


NS_END
