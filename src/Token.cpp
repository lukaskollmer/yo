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
        CASE(Impl)
        CASE(StringLiteral)
        CASE(CharLiteral)
        CASE(DoubleLiteral)
        CASE(ByteStringLiteral)
        CASE(Hashtag)
        CASE(Use)
        CASE(While)
        CASE(Using)
        CASE(BoolLiteral)
        CASE(Tilde)
        CASE(For)
        CASE(In)
    }
#undef CASE
}


std::string escape_char(char C) {
    switch (C) {
        case '\n': return "\\n";
        case '\t': return "\\t";
        default: return std::string(1, C);
    }
}

std::ostream &operator<<(std::ostream &OS, Token &T) {
    OS << "<Token " << TokenKindToString(T.Kind);
    switch (T.Kind) {
        case Token::TokenKind::Identifier:
        case Token::TokenKind::StringLiteral:
        case Token::TokenKind::ByteStringLiteral:
            OS << " '" << T.Data.S << "'";
            break;
        case Token::TokenKind::IntegerLiteral:
            OS << " " << T.Data.I;
            break;
        case Token::TokenKind::CharLiteral:
            OS << " '" << escape_char(T.Data.C) << "'";
            break;
        case Token::TokenKind::DoubleLiteral:
            OS << " " << T.Data.D;
            break;
        default: break;
    }
    
    return OS << ">";
}



std::ostream &operator<<(std::ostream &OS, Token::TokenKind TK) {
    return OS << TokenKindToString(TK);
}
