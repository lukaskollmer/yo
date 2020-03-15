//
//  TokenKind.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-03-15.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#include "TokenKind.h"


#define CASE(X) case TokenKind::X: OS << #X; break;

std::ostream& yo::lex::operator<<(std::ostream &OS, TokenKind kind) {
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
    return OS;
}

#undef CASE
