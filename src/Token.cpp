//
//  Token.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Token.h"
#include <map>

using namespace yo::parser;

std::string TokenKindToString(Token::TokenKind kind) {
#define CASE(x) case Token::TokenKind::x: return #x;
    switch (kind) {
        CASE(Unknown)
        CASE(EOF_)
        CASE(Ident)
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
        CASE(BoolLiteral)
        CASE(Tilde)
        CASE(For)
        CASE(In)
        CASE(Match)
        CASE(QuestionMark)
        CASE(Whitespace)
        CASE(LineComment)
        CASE(BlockComment)
    }
#undef CASE
}


std::string escape_char(char c) {
    switch (c) {
        case '\n': return "\\n";
        case '\t': return "\\t";
        default: return std::string(1, c);
    }
}


std::ostream &yo::parser::operator<<(std::ostream &OS, const Token::TokenKind TK) {
    return OS << TokenKindToString(TK);
}


std::ostream& yo::parser::operator<<(std::ostream &OS, const Token &T) {
    OS << "<Token " << TokenKindToString(T.getKind());
    switch (T.getKind()) {
        case Token::TokenKind::Ident:
        case Token::TokenKind::StringLiteral:
        case Token::TokenKind::ByteStringLiteral:
        case Token::TokenKind::Whitespace:
            OS << " '" << T.getData<std::string>() << "'";
            break;
        case Token::TokenKind::IntegerLiteral:
            OS << " " << T.getData<uint64_t>();
            break;
        case Token::TokenKind::CharLiteral:
            OS << " '" << escape_char(T.getData<char>()) << "'";
            break;
        case Token::TokenKind::DoubleLiteral:
            OS << " " << T.getData<double>();
            break;
        default: break;
    }
    
    return OS << ">";
}
