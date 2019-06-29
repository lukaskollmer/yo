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
        CASE(Match)
        CASE(QuestionMark)
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

std::ostream& yo::parser::operator<<(std::ostream &OS, Token &T) {
    OS << "<Token " << TokenKindToString(T.kind);
    switch (T.kind) {
        case Token::TokenKind::Identifier:
        case Token::TokenKind::StringLiteral:
        case Token::TokenKind::ByteStringLiteral:
            OS << " '" << std::get<std::string>(T.data) << "'";
            break;
        case Token::TokenKind::IntegerLiteral:
            OS << " " << std::get<uint64_t>(T.data);
            break;
        case Token::TokenKind::CharLiteral:
            OS << " '" << escape_char(std::get<char>(T.data)) << "'";
            break;
        case Token::TokenKind::DoubleLiteral:
            OS << " " << std::get<double>(T.data);
            break;
        default: break;
    }
    
    return OS << ">";
}



std::ostream &yo::parser::operator<<(std::ostream &OS, Token::TokenKind TK) {
    return OS << TokenKindToString(TK);
}
