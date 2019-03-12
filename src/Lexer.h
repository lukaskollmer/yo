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


class Lexer {
public:
    TokenList Lex(std::string &String, std::string &Filename);
    
private:
    TokenList Tokens;
    
    uint64_t Offset;
    uint64_t Line;
    uint64_t LineStart;
    std::string Filename;
    
    uint64_t ColumnRelatoveToCurrentLine() {
        return Offset - LineStart;
    }
    
    Token *HandleRawToken(const std::string &RawToken, Token::TokenKind TokenKind = Token::TokenKind::Unknown);
};
