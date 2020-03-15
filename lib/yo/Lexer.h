//
//  Lexer.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"
#include "Token.h"

#include <string>
#include <vector>
#include <memory>


NS_START(yo::lex)

class Lexer {
public:
    // set `preserveFullInput` to true to preserve whitespace and comments
    std::vector<Token> lex(std::string_view sourceText, const std::string& filepath, bool preserveFullInput = false);
    
private:
    std::vector<Token> tokens;
    
    uint64_t offset;
    uint64_t line;
    uint64_t lineStart;
    std::string filepath;
    
    uint64_t columnRelativeToCurrentLine() {
        return offset - lineStart;
    }
    
    SourceLocation currentSourceLocation(int64_t offsetBy, uint64_t length);
    Token& handleRawToken(const std::string& tokenSourceText, TokenKind tokenKind = TokenKind::Unknown);
};

NS_END
