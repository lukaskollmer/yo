//
//  Token.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "TokenKind.h"
#include "SourceLocation.h"

#include <string>
#include <variant>

namespace yo {
namespace lex {


class Token {
    TokenKind kind;
    std::string sourceText;
    SourceLocation sourceLoc;
    std::variant<
        bool,
        char,
        double,
        uint64_t,
        std::string
    > data;
    
public:
    Token() : kind(TokenKind::Unknown) {}
    Token(TokenKind kind, std::string sourceText) : kind(kind), sourceText(sourceText) {}
    
    template <typename T>
    Token(TokenKind kind, std::string sourceText, const T &data) : kind(kind), sourceText(sourceText), data(data) {}
    
    
    TokenKind getKind() const {
        return kind;
    }
    
    const SourceLocation& getSourceLocation() const {
        return sourceLoc;
    }
    void setSourceLocation(SourceLocation loc) {
        sourceLoc = loc; // TODO set this via the constructor?
    }
    
    const std::string& getSourceText() const {
        return sourceText;
    }
    
    template <typename T>
    const T& getData() const {
        return std::get<T>(data);
    }
    
    template <typename T>
    void setData(const T &newValue) {
        data = newValue;
    }
    
};



} // ns lex
} // ns yo
