//
//  Lexer.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include <iostream>
#include <sstream>
#include <map>

#include "Lexer.h"
#include "util.h"


using namespace yo;
using namespace yo::parser;

using TK = Token::TokenKind;

static constexpr char DOUBLE_QUOTE = '"';


inline bool charIsInInclusiveRange(char c, char start, char end) {
    return c >= start && c <= end;
}


inline bool isWhitespaceChar(char c) {
    return c == ' '; // TODO there are other kinds of whitespace
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
static std::map<std::string, Token::TokenKind> tokenKindMappings = {
    { "(" , TK::OpeningParens },
    { ")" , TK::ClosingParens },
    { "{" , TK::OpeningCurlyBraces },
    { "}" , TK::ClosingCurlyBraces },
    { "[" , TK::OpeningSquareBrackets },
    { "]" , TK::ClosingSquareBrackets },
    
    { "," , TK::Comma },
    { "!" , TK::ExclamationMark },
    { "?" , TK::QuestionMark },
    { ":" , TK::Colon },
    { ";" , TK::Semicolon },
    { "=" , TK::EqualsSign },
    { "." , TK::Period },
    { "#" , TK::Hashtag },
    
    { "*" , TK::Asterisk },
    { "+" , TK::Plus },
    { "-" , TK::Minus },
    { "/" , TK::ForwardSlash },
    { "%" , TK::PercentageSign },
    { "~" , TK::Tilde },
    
    { "&" , TK::Ampersand },
    { "|" , TK::Pipe },
    { "^" , TK::Circumflex },
    
    { "<" , TK::LessThanSign },
    { ">" , TK::GreaterSign },
    
    // Keywords
    { "fn",     TK::Fn     },
    { "return", TK::Return },
    { "let",    TK::Let    },
    { "if",     TK::If     },
    { "else",   TK::Else   },
    { "as",     TK::As     },
    { "struct", TK::Struct },
    { "impl",   TK::Impl   },
    { "use",    TK::Use    },
    { "while",  TK::While  },
    { "for",    TK::For    },
    { "in",     TK::In     },
    { "match",  TK::Match  },
};


std::vector<Token> Lexer::lex(std::string_view sourceText, const std::string &filepath, bool preserveFullInput) {
    // Reset everything
    tokens = {};
    line = 0;
    lineStart = 0;
    this->filepath = filepath;
    
    
    auto length = sourceText.length();
    
    for (offset = 0; offset < length; offset++) {
        auto c = sourceText[offset];
        
        //if (whitespaceCharacters.contains(c)) {
        if (isWhitespaceChar(c)) {
            if (preserveFullInput) {
                handleRawToken(std::string(1, c), TK::Whitespace);
            }
            continue;
        }
        
        // Q: Why is this a lambda?
        // A: We also need to adjust the current source position if we're in multiline strings/comments
        auto handleNewline = [&](bool ignorePreserveFullInput = false) {
            LKAssert(sourceText[offset] == '\n');
            if (preserveFullInput && !ignorePreserveFullInput) {
                handleRawToken("\n", TK::Whitespace);
            }
            line++;
            lineStart = offset + 1;
        };
        
        if (c == '\n') {
            // TODO this would be a good place to insert semicolons?
            handleNewline();
            continue;
        }
        
        if (c == '/' && sourceText[offset + 1] == '/') {
            // Start of line comment
            uint64_t pos_prev = offset;
            while (sourceText[++offset] != '\n');
            
            if (preserveFullInput) {
                auto commentText = std::string(sourceText.substr(pos_prev, offset - pos_prev));
                handleRawToken(commentText, TK::LineComment);
            }
            
            handleNewline();
            continue;
        }
        
        if (c == '/' && sourceText[offset + 1] == '*') {
            // start of comment block
            // Note that we deliberately don't check whether a comment's end is within a string literal
            uint64_t startPos = offset;
            offset += 2;
            while (sourceText[offset++] != '*' && sourceText[offset] != '/') {
                if (sourceText[offset] == '\n') handleNewline(true);
            }
            
            if (preserveFullInput) {
                auto commentSourceText = std::string(sourceText.substr(startPos, offset - startPos + 1));
                handleRawToken(commentSourceText, TK::BlockComment);
            }
            
            continue;
        }
        
        // Offset after returning is the character after the end of the escaped character
        // For example, if we're parsing `'\123x`, the Offset after returhing would "point" to the x
        auto parseEscapedCharacter = [&] () -> char {
            LKAssert(sourceText[offset] == '\\');
            if (sourceText[offset + 1] == '\\') {
                // Escaped backslash
                offset++;
                return '\\';
            }
            
            auto x = sourceText[++offset];
            // Offset at this point is now the character after the backslash
            if (x == 'x' || isOctalDigitChar(x)) { // Hex or octal value
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
        };
        

        if (c == '\'') { // Char literal
            uint64_t initialOffset = offset;
            c = sourceText[++offset];
            char content;
            
            if (c == '\'') {
                throw; // Empty char literal
            } else if (c == '\\') {
                content = parseEscapedCharacter();
            } else {
                content = c;
                offset++;
            }
            LKAssert(sourceText[offset] == '\'');
            
            auto rawSourceText = std::string(sourceText.substr(initialOffset, offset - initialOffset + 1));
            handleRawToken(rawSourceText, TK::CharLiteral).setData(content);
            continue;
        }
        
        
        bool isByteStringLiteral = false;
        bool isRawStringLiteral = false;
        
        if (c == 'b' && sourceText[offset + 1 + (sourceText[offset + 1] == 'r')] == DOUBLE_QUOTE) {
            isByteStringLiteral = true;
            offset++;
        }
        
        if (sourceText[offset] == 'r' && sourceText[offset + 1] == DOUBLE_QUOTE) { // Raw (unescaped) string literal
            isRawStringLiteral = true;
            offset++;
        }
        
        if (sourceText[offset] == DOUBLE_QUOTE) { // String literal
            uint64_t startOffset = offset - (isRawStringLiteral + isByteStringLiteral);
            
            std::string content;
            offset++;
            // Offset is at first char after opening quotes
            if (isRawStringLiteral) {
                for (auto c = sourceText[offset]; c != DOUBLE_QUOTE; c = sourceText[++offset]) {
                    content.push_back(c);
                    if (c == '\n') handleNewline(true);
                }
            } else {
                for (auto c = sourceText[offset]; c != DOUBLE_QUOTE; c = sourceText[offset]) {
                    if (c == '\\') {
                        content.push_back(parseEscapedCharacter());
                    } else {
                        if (c == '\n') handleNewline(true);
                        content.push_back(c);
                        offset++;
                    }
                }
            }
            LKAssert(sourceText[offset] == DOUBLE_QUOTE);
            
            auto rawSourceText = std::string(sourceText.substr(startOffset, offset - startOffset + 1));
            handleRawToken(rawSourceText, isByteStringLiteral ? TK::ByteStringLiteral : TK::StringLiteral).setData(content);
            continue;
        }
        
        // If either of these is true, the string should've been handled above
        LKAssert(!isRawStringLiteral && !isByteStringLiteral);
        
        if (isSingleCharToken(c)) {
            std::string x(1, c);
            handleRawToken(x);
            continue;
        }
        
        if (isIdentStartChar(c)) {
            std::string ident;
            do {
                ident.push_back(c);
                c = sourceText[++offset];
            } while (isIdentChar(c));
            handleRawToken(ident);
            offset--;
            continue;
        }
        
        if (isDecimalDigitChar(c)) { // Number literal
            uint8_t base = 10;
            std::string rawValue;
            auto next = sourceText[offset + 1];
            
            if (c == '0') { // binary/octal/decimal/hex ?
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
            
            // The hex digits charset contains all other digits as well, so it makes most sense to use that.
            // Invalid characters will be detected by `std::stoll` below
            
            do {
                rawValue.push_back(sourceText[offset]);
            } while (isHexDigitChar(sourceText[++offset]));
            
            
            uint64_t value;
            std::size_t length;
            
            try {
                value = std::stoll(rawValue, &length, base);
            } catch (const std::exception &e) {
                std::cout << e.what() << std::endl;
                throw;
            }
            
            // TODO turn this into a nice error message
            LKAssert(length == rawValue.length()); // The string contained illegal characters (eg: 0b110012)
            
            handleRawToken(rawValue, TK::IntegerLiteral).setData(value);
            offset--; // unavoidable :/
            continue;
        }
        LKFatalError("unhandled character: '%c'", sourceText[offset]);
    }
    
    handleRawToken("", TK::EOF_);
    return tokens;
}


// this might seems stupid, but it does in fact save 2 string comparisons per identifier parsed
uint8_t isBoolLiteral(std::string_view str) {
    if (str == "false") return 1;
    else if (str == "true") return 2;
    else return 0;
}


Token& Lexer::handleRawToken(const std::string &tokenSourceText, Token::TokenKind tokenKind) {
    Token token;
    
    if (tokenKind != TK::Unknown) {
        token = Token(tokenSourceText, tokenKind);
    }
    else if (auto it = tokenKindMappings.find(tokenSourceText); it != tokenKindMappings.end()) {
        token = Token(tokenSourceText, it->second);
    }
    else if (isIdentStartChar(tokenSourceText[0]) && util::string::allCharsMatch(tokenSourceText.substr(1), isIdentChar)) {
        if (auto val = isBoolLiteral(tokenSourceText); val != 0) {
            token = Token(tokenSourceText, TK::BoolLiteral);
            token.setData<bool>(val - 1);
        } else {
            token = Token(tokenSourceText, TK::Ident);
            token.setData(tokenSourceText);
        }
    }
    else {
        LKLog("didn't initialize token for '%s'", tokenSourceText.c_str());
        exit(EXIT_FAILURE);
    }
    
    auto column = columnRelativeToCurrentLine();
    token.setSourceLocation(TokenSourceLocation(filepath, line + 1, column + 1, offset - column));
    
    tokens.push_back(token);
    return tokens.back();
}

