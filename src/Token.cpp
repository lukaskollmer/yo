//
//  Token.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//


#include "Token.h"
#include <map>

std::string TokenKindToString(Token::TokenKind Kind) {
#define CASE(x) case Token::TokenKind::x: return #x;
    switch (Kind) {
        CASE(Unknown)
        CASE(EOF_)
        CASE(Identifier)
        CASE(IntegerLiteral)
        CASE(OpeningParens)
        CASE(ClosingParens)
        CASE(OpeningCurlyBraces)
        CASE(ClosingCurlyBraces)
        CASE(OpeningSquareBrackets)
        CASE(ClosingSquareBrackets)
        CASE(Comma)
        CASE(Colon)
        CASE(Semicolon)
        CASE(Asterisk)
        CASE(Plus)
        CASE(Minus)
        CASE(ForwardSlash)
        CASE(PercentageSign)
        CASE(Return)
        CASE(Fn)
        CASE(Extern)
        CASE(EqualsSign)
        CASE(Circumflex)
        CASE(LessThanSign)
        CASE(GreaterSign)
        CASE(Let)
        CASE(Ampersand)
        CASE(Pipe)
        CASE(ExclamationMark)
        CASE(If)
        CASE(Else)
        CASE(As)
        CASE(Period)
        CASE(Struct)
    }
#undef CASE
}


std::ostream &operator<<(std::ostream &OS, const Token &T) {
    OS << "<Token " << TokenKindToString(T.getKind());
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
    return OS << TokenKindToString(TK);
}
