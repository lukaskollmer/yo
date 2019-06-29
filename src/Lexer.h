//
//  Lexer.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <string>
#include <vector>
#include <memory>

#include "Token.h"

#include "util.h"

NS_START(yo::parser)

class Lexer {
public:
    TokenList lex(std::string_view string, std::string &filename);
    
private:
    TokenList tokens;
    
    uint64_t offset;
    uint64_t line;
    uint64_t lineStart;
    std::string filename;
    
    uint64_t columnRelativeToCurrentLine() {
        return offset - lineStart;
    }
    
    Token *handleRawToken(const std::string &rawToken, Token::TokenKind tokenKind = Token::TokenKind::Unknown);
};

NS_END
