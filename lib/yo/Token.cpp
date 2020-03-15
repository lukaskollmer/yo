//
//  Token.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Token.h"
#include <map>

using namespace yo::lex;

std::string TokenKindToString(TokenKind kind) {
#define CASE(X) case TokenKind::X: return #X;
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
        CASE(OpeningAngledBracket)
        CASE(ClosingAngledBracket)
        CASE(Let)
        CASE(Ampersand)
        CASE(Pipe)
        CASE(ExclamationMark)
        CASE(If)
        CASE(Else)
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
        CASE(Decltype)
        CASE(Break)
        CASE(Continue)
        CASE(Variant)
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


std::ostream &yo::lex::operator<<(std::ostream &OS, const TokenKind TK) {
    return OS << TokenKindToString(TK);
}


std::ostream& yo::lex::operator<<(std::ostream &OS, const Token &T) {
    OS << "<Token " << TokenKindToString(T.getKind());
    switch (T.getKind()) {
        case TokenKind::Ident:
        case TokenKind::StringLiteral:
        case TokenKind::ByteStringLiteral:
        case TokenKind::Whitespace:
            OS << " '" << T.getData<std::string>() << "'";
            break;
        case TokenKind::IntegerLiteral:
            OS << " " << T.getData<uint64_t>();
            break;
        case TokenKind::CharLiteral:
            OS << " '" << escape_char(T.getData<char>()) << "'";
            break;
        case TokenKind::DoubleLiteral:
            OS << " " << T.getData<double>();
            break;
        default:
            break;
    }
    
    return OS << ">";
}
