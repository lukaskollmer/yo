//
//  Token.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//


#include "Token.h"
#include <map>

static std::map<Token::TokenKind, std::string> TokenStringMappings = {
#define ENTRY(x) { Token::TokenKind::x, #x },
    
    ENTRY(Unknown)
    ENTRY(EOF_)
    ENTRY(Identifier)
    ENTRY(IntegerLiteral)
    
    ENTRY(OpeningParens)
    ENTRY(ClosingParens)
    ENTRY(OpeningCurlyBraces)
    ENTRY(ClosingCurlyBraces)
    ENTRY(Colon)
    ENTRY(Comma)
    ENTRY(Semicolon)
    
    ENTRY(Return)
    ENTRY(Fn)
    
#undef ENTRY
};


std::ostream &operator<<(std::ostream &OS, const Token &T) {
    OS << "<Token " << TokenStringMappings.at(T.getKind());
    switch (T.getKind()) {
        case Token::TokenKind::Identifier:
            OS << " '" << T.getData().s << "'";
            break;
        case Token::TokenKind::IntegerLiteral:
            OS << " " << T.getData().i << "";
            break;
        default: break;
    }
    OS << ">";
    return OS;
}



std::ostream &operator<<(std::ostream &OS, const Token::TokenKind &TK) {
    return OS << TokenStringMappings.at(TK);
}
