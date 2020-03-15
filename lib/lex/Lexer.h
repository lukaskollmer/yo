//
//  Lexer.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "SourceLocation.h"
#include "Token.h"

#include <string>
#include <vector>

namespace yo {
namespace lex {


class Lexer {
    std::string_view sourceText;
    const std::string &filepath;
    
    uint64_t offset = 0;
    uint64_t line = 0;
    uint64_t lineStart = 0;
    std::vector<Token> tokens;
    
public:
    bool shouldPreserveFullInput = false;
    
    Lexer(std::string_view sourceText, const std::string &filepath) : sourceText(sourceText), filepath(filepath) {}
    
    std::vector<Token> lex();
    
private:
    void consume(uint64_t count = 1) {
        offset += count;
    }
    
    void handleNewline(bool ignorePreserveFullInput = false) {
        if (shouldPreserveFullInput && !ignorePreserveFullInput) {
            addToken(TokenKind::Whitespace, "\n"); // TODO what is this used for?
        }
        line += 1;
        lineStart = offset + 1;
        consume();
    }
    
    
    void lexLineComment();
    void lexBlockComment();
    void lexIdent();
    void lexNumberLiteral();
    void lexCharLiteral();
    void lexStringLiteral(bool isByteString, bool isRawString);
    
    SourceLocation getSourceLoc(int64_t offsetBy, uint64_t length) const;
    
    Token& addToken(TokenKind tokenKind, const std::string& tokenSourceText);
    
    char readEscapedChar();
};



} // ns lex
} // ns yo
