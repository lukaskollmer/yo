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
    LKUInteger LexLine(std::string &Line, LKUInteger LineStartPosition);
    TokenList Lex(std::string &String);
    
    void HandleRawToken(const std::string &RawToken, LKUInteger EndLocation, Token::TokenKind TokenKind = Token::TokenKind::Unknown);
    
    static std::string RemovingComments(std::string Input);
    
private:
    TokenList Tokens;
};
