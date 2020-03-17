//
//  Lexer.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Lexer.h"
#include "Diagnostics.h"
#include "util/util.h"

#include <map>

using namespace yo;
using namespace yo::lex;


static inline constexpr char DOUBLE_QUOTE = '"';

inline bool charIsInInclusiveRange(char c, char start, char end) {
    return c >= start && c <= end;
}

inline bool charIsWhitespace(char c) {
    return c == ' '; // TODO there are other kinds of whitespace
}

inline bool charIsNewline(char c) {
    return c == '\n'; // TODO what about other kinds of newlines?
}

inline bool isBinaryDigitChar(char c) {
    return c == '0' || c == '1';
}

inline bool isOctalDigitChar(char c) {
    return charIsInInclusiveRange(c, '0', '7');
}

inline bool isDecimalDigitChar(char c) {
    return charIsInInclusiveRange(c, '0', '9');
}

inline bool isHexDigitChar(char c) {
    return charIsInInclusiveRange(c, '0', '9') || charIsInInclusiveRange(c, 'a', 'f');
}

inline bool isLetterChar(char c) {
    return charIsInInclusiveRange(c, 'a', 'z') || charIsInInclusiveRange(c, 'A', 'Z');
}

inline bool isIdentStartChar(char c) {
    return c == '_' || isLetterChar(c);
}

inline bool isIdentChar(char c) {
    return isIdentStartChar(c) || isDecimalDigitChar(c);
}

inline bool isSingleCharToken(char c) {
    static std::string_view singleCharTokens = ".,+-*/;:=<>%!&^#|~(){}[]@";
    return singleCharTokens.find(c) != std::string_view::npos;
}



// TODO no idea if/how this would work, but it'd be cool to have a compile-time assertion that the mapping below is complete
static const std::map<std::string, TokenKind> tokenKindMappings = {
    { "(" , TokenKind::OpeningParens },
    { ")" , TokenKind::ClosingParens },
    { "{" , TokenKind::OpeningCurlyBraces },
    { "}" , TokenKind::ClosingCurlyBraces },
    { "[" , TokenKind::OpeningSquareBrackets },
    { "]" , TokenKind::ClosingSquareBrackets },
    
    { "," , TokenKind::Comma },
    { "!" , TokenKind::ExclamationMark },
    { "?" , TokenKind::QuestionMark },
    { ":" , TokenKind::Colon },
    { ";" , TokenKind::Semicolon },
    { "=" , TokenKind::EqualsSign },
    { "." , TokenKind::Period },
    { "#" , TokenKind::Hashtag },
    
    { "*" , TokenKind::Asterisk },
    { "+" , TokenKind::Plus },
    { "-" , TokenKind::Minus },
    { "/" , TokenKind::ForwardSlash },
    { "%" , TokenKind::PercentageSign },
    { "~" , TokenKind::Tilde },
    
    { "&" , TokenKind::Ampersand },
    { "|" , TokenKind::Pipe },
    { "^" , TokenKind::Circumflex },
    
    { "<" , TokenKind::OpeningAngledBracket },
    { ">" , TokenKind::ClosingAngledBracket },
    
    // Keywords
    { "fn",       TokenKind::Fn       },
    { "return",   TokenKind::Return   },
    { "let",      TokenKind::Let      },
    { "if",       TokenKind::If       },
    { "else",     TokenKind::Else     },
    { "struct",   TokenKind::Struct   },
    { "variant",  TokenKind::Variant  },
    { "impl",     TokenKind::Impl     },
    { "use",      TokenKind::Use      },
    { "while",    TokenKind::While    },
    { "for",      TokenKind::For      },
    { "in",       TokenKind::In       },
    { "match",    TokenKind::Match    },
    { "decltype", TokenKind::Decltype },
    { "break",    TokenKind::Break    },
    { "continue", TokenKind::Continue },
};


// this might seems stupid, but it does in fact save 2 string comparisons per identifier parsed
uint8_t isBoolLiteral(std::string_view str) {
    if (str == "false") return 1;
    else if (str == "true") return 2;
    else return 0;
}


SourceLocation Lexer::getSourceLoc(int64_t offsetBy, uint64_t length) const {
    int64_t column = static_cast<int64_t>(offset + 2 - lineStart - length) + offsetBy;
    LKAssert(column > 0);
    return SourceLocation(filepath, line + 1, column, length);
}


Token& Lexer::addToken(TokenKind tokenKind, const std::string &tokenSourceText) {
    Token token;
    
    if (tokenKind != TokenKind::Unknown) {
        token = Token(tokenKind, tokenSourceText);
    }
    else if (auto it = tokenKindMappings.find(tokenSourceText); it != tokenKindMappings.end()) {
        token = Token(it->second, tokenSourceText);
    }
    else if (isIdentStartChar(tokenSourceText[0]) && util::string::allCharsMatch(tokenSourceText.substr(1), isIdentChar)) {
        if (auto val = isBoolLiteral(tokenSourceText); val != 0) {
            token = Token(TokenKind::BoolLiteral, tokenSourceText);
            token.setData<bool>(val - 1);
        } else {
            token = Token(TokenKind::Ident, tokenSourceText);
            token.setData(tokenSourceText);
        }
    }
    else {
        LKFatalError("didn't initialize token for '%s'", tokenSourceText.c_str());
    }
    
    token.setSourceLocation(getSourceLoc(0, tokenSourceText.length()));
    tokens.push_back(token);
    return tokens.back();
}



std::vector<Token> Lexer::lex() {
    if (!tokens.empty()) {
        LKFatalError("don;t reuse a lexer instance");
    }
    
    offset = 0;
    uint64_t prevOffset = UINT64_MAX;
    
    
    while (offset < sourceText.size()) {
        if (prevOffset == offset) {
            std::cout << getSourceLoc(0, 1) << std::endl;
            LKFatalError("'%c'", sourceText[offset]);
        } else {
            prevOffset = offset;
        }
        
        auto c = sourceText[offset];
        
        if (charIsWhitespace(c)) {
            consume();
            continue;
        
        } else if (charIsNewline(c)) {
            handleNewline();
            continue;
        
        } else if (c == '/' && sourceText[offset + 1] == '/') {
            lexLineComment();
            continue;
        
        } else if (c == '/' && sourceText[offset + 1] == '*') {
            lexBlockComment();
            continue;
        
        } else if (c == '\'') {
            lexCharLiteral();
            continue;
        
        } else if (c == '"') {
            lexStringLiteral(/*isByteString*/ false, /*isRawString*/ false);
            continue;
        
        } else if (isIdentStartChar(c)) {
            if (c == 'b') {
                if (sourceText[offset + 1] == '"') {
                    consume();
                    lexStringLiteral(/*isByteString*/ true, /*isRawString*/ false);
                    continue;
                } else if (sourceText[offset + 1] == 'r' && sourceText[offset + 2] == '"') {
                    consume(2);
                    lexStringLiteral(/*isByteString*/ true, /*isRawString*/ true);
                    continue;
                }
            }
            lexIdent();
            continue;
        
        } else if (isSingleCharToken(c)) {
            std::string str(1, c);
            addToken(TokenKind::Unknown, str);
            offset++;
            continue;
        
        } else if (isIdentStartChar(c)) {
            lexIdent();
            continue;
        
        } else if (isDecimalDigitChar(c)) {
            lexNumberLiteral();
            continue;
        
        } else if (c == '.') {
            LKFatalError("TODO");
            
        
        } else {
//            LKFatalError("char not handled");
        }
    }
    
    addToken(TokenKind::EOF_, "");
    return tokens;
}



void Lexer::lexLineComment() {
    // Start of line comment
    uint64_t pos_prev = offset;
    while (sourceText[++offset] != '\n');
    
    if (shouldPreserveFullInput) {
        auto commentText = std::string(sourceText.substr(pos_prev, offset - pos_prev));
        addToken(TokenKind::LineComment, commentText);
    }
    
    handleNewline();
}


void Lexer::lexBlockComment() {
    // start of comment block
    // Note that we deliberately don't check whether a comment's end is within a string literal
    uint64_t startPos = offset;
    offset += 2;
    while (!(sourceText[offset++] == '*' && sourceText[offset] == '/')) {
        if (sourceText[offset] == '\n') {
            handleNewline(true);
        }
    }
    
    if (shouldPreserveFullInput) {
        auto commentSourceText = std::string(sourceText.substr(startPos, offset - startPos + 1));
        addToken(TokenKind::BlockComment, commentSourceText);
    }
    
    offset++;
}

// Offset after returning is the character after the end of the escaped character
// For example, if we're parsing `'\123x`, the offset after returhing would "point" to the x
char Lexer::readEscapedChar() {
    LKAssert(sourceText[offset] == '\\');
    if (sourceText[offset + 1] == '\\') {
        // Escaped backslash
        offset++;
        return '\\';
    }
    
    auto x = sourceText[++offset];
    // Offset at this point is now the character after the backslash
    if (x == 'x' || isOctalDigitChar(x)) { // Hex or octal value
        // TODO why does this allow octal values? what are the rules here?
        auto isHex = x == 'x';
        std::string rawValue;
        
        if (isHex) {
            rawValue.push_back(sourceText[++offset]);
            rawValue.push_back(sourceText[++offset]);
            offset++;
        } else {
            while (isOctalDigitChar(sourceText[offset]) && rawValue.length() < 3) {
                rawValue.push_back(sourceText[offset++]);
            }
        }
        
        std::size_t length;
        char value;
        try {
            value = std::stoi(rawValue, &length, isHex ? 16 : 8);
        } catch (...) {
            throw;
        }
        
        LKAssert(length == rawValue.length());
        return value;
    }
    
    switch (x) {
        case 'n': offset++; return '\n';
        case 't': offset++; return '\t';
        default: break;
    }
    LKFatalError("Unable to parse escaped character");
}


void Lexer::lexIdent() {
    std::string ident;
    char c = sourceText[offset];
    do {
        ident.push_back(c);
        c = sourceText[++offset];
    } while (isIdentChar(c));
    offset--;
    addToken(TokenKind::Unknown, ident);
    offset++; // TODO is decrementing and re-incrementing the offset really necessary?
}


void Lexer::lexCharLiteral() {
    uint64_t initialOffset = offset;
    char c = sourceText[++offset];
    char content;
    
    if (c == '\'') {
        throw; // Empty char literal
    } else if (c == '\\') {
        content = readEscapedChar();
    } else {
        content = c;
        offset++;
    }
    LKAssert(sourceText[offset] == '\'');
    offset++;
    
    auto rawSourceText = std::string(sourceText.substr(initialOffset, offset - initialOffset + 1));
    //handleRawToken(rawSourceText, TokenKind::CharLiteral).setData(content);
    auto &token = addToken(TokenKind::CharLiteral, rawSourceText);
    token.setData(content);
}


void Lexer::lexStringLiteral(bool isByteString, bool isRawString) {
    uint64_t startOffset = offset - (isRawString + isByteString);
    
    std::string content;
    offset++;
    // Offset is at first char after opening quotes
    if (isRawString) {
        for (auto c = sourceText[offset]; c != DOUBLE_QUOTE; c = sourceText[++offset]) {
            content.push_back(c);
            if (c == '\n') handleNewline(true);
        }
    } else {
        for (auto c = sourceText[offset]; c != DOUBLE_QUOTE; c = sourceText[offset]) {
            if (c == '\\') {
                content.push_back(readEscapedChar());
            } else {
                if (c == '\n') handleNewline(true);
                content.push_back(c);
                offset++;
            }
        }
    }
    LKAssert(sourceText[offset] == DOUBLE_QUOTE);
    offset++;
    
    auto rawSourceText = std::string(sourceText.substr(startOffset, offset - startOffset + 1));
    auto &token = addToken(isByteString ? TokenKind::ByteStringLiteral : TokenKind::StringLiteral, rawSourceText);
    token.setData(content);
}




void Lexer::lexNumberLiteral() {
    uint8_t base = 10;
    std::string rawValue;
    auto next = sourceText[offset + 1];
    bool isFloat = false;
    
    if (sourceText[offset] == '0') { // binary/octal/decimal/hex ?
        if (next == 'b') {
            offset += 2;
            base = 2;
        } else if (next == 'o') {
            offset += 2;
            base = 8;
        } else if (next == 'x') {
            offset += 2;
            base = 16;
        } else {
            // A single 0, not followed by another numeric digit
            LKAssert(!isHexDigitChar(next));
        }
    }
    
    while (true) {
        // TODO allow non-base-10 floating point literals?
        // ie: 0b101.11 = 5.75
        // also support some of these: https://en.cppreference.com/w/cpp/language/floating_literal ?
        rawValue.push_back(sourceText[offset]);
        
        auto nextChar = sourceText[++offset];
        // The hex digits charset contains all other digits as well, so it makes most sense to use that.
        // Invalid characters will be detected by `std::{stoll|stod}` below
        
        if (isFloat && isDecimalDigitChar(nextChar)) {
            // if isFloat is true, the next digits that still belong to the number can only be base-10
            continue;
        } else if (!isFloat && isHexDigitChar(nextChar)) {
            continue;
        }
        
        // All of the below are valid code!
        // 1.7
        // 1.x
        // 1.7.x
        if (nextChar == '.') {
            if (!isDecimalDigitChar(sourceText[offset + 1])) { // next next char
                break;
            }
            if (isFloat || base != 10) {
                // parsing a float and there's a second period or parsing something that cannot become a float and there's a period
                break;
            }
            isFloat = true;
        } else {
            break; // end of literal?
        }
    }
    
    
    double value_f64;
    uint64_t value_i64;
    std::size_t length;
    
    try {
        if (isFloat) {
            value_f64 = std::stod(rawValue, &length);
        } else {
            value_i64 = std::stoll(rawValue, &length, base);
        }
    } catch (const std::exception &e) { // TODO can we do this w/out an the exception handling?
        std::cout << e.what() << std::endl;
        throw;
    }
    
    offset--; // unavoidable :/
    
    if (length != rawValue.length()) {
        auto loc = getSourceLoc(length, rawValue.length());
        diagnostics::emitError(loc, "Invalid character in number literal");
    }
    
    auto &token = addToken(isFloat ? TokenKind::DoubleLiteral : TokenKind::IntegerLiteral, rawValue);
    if (isFloat) {
        token.setData(value_f64);
    } else {
        token.setData(value_i64);
    }
    offset++; // TODO again, do we really need the decrement above?
}

