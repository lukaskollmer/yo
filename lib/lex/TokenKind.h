//
//  TokenKind.h
//  yo
//
//  Created by Lukas Kollmer on 2020-03-15.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <iostream>

namespace yo::lex {

enum class TokenKind;

std::ostream& operator<<(std::ostream&, TokenKind);

enum class TokenKind {
    Unknown,
    EOF_,
    
    // Tokens w/ associated data
    Ident,
    StringLiteral, // TODO rename to NormalStringLiteral or StdStringLiteral?
    ByteStringLiteral,
    CharLiteral,
    IntegerLiteral,
    DoubleLiteral,
    BoolLiteral,
    Whitespace,
    LineComment, BlockComment,
    
    // Punctuation
    OpeningParens,
    ClosingParens,
    OpeningCurlyBraces,
    ClosingCurlyBraces,
    OpeningSquareBrackets,
    ClosingSquareBrackets,
    OpeningAngledBracket,
    ClosingAngledBracket,
    Period, Hashtag, Tilde,
    Comma, Colon, Semicolon, EqualsSign,
    ExclamationMark, QuestionMark,
    
    Asterisk, Plus, Minus, ForwardSlash, PercentageSign,
    Ampersand, Pipe, Circumflex,
    
    // Keywords
    Use,
    Fn, Struct, Impl, Variant,
    Let, Return,
    If, Else, While, For, In, Match,
    Break, Continue,
    Decltype
};



} // ns yo::lex
