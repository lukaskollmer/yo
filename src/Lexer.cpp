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
#include "CharacterSet.h"
//#include "util.h"


using namespace yo;
using namespace yo::parser;

using TK = Token::TokenKind;

static constexpr char DOUBLE_QUOTE = '"';

static CharacterSet letters({{'a', 'z'}, {'A', 'Z'}});
static CharacterSet ignoredCharacters(" "); // TODO make this "all whitespace" instead
static CharacterSet binaryDigits("01");
static CharacterSet octalDigits("01234567");
static CharacterSet decimalDigits("0123456789");
static CharacterSet hexadecimalDigits("0123456789abcdef");
static CharacterSet identifierStartCharacters = letters.joined(CharacterSet("_"));
static CharacterSet identifierCharacters = identifierStartCharacters.joined(decimalDigits);
static CharacterSet singleCharTokens(".,+-*/;:=<>%!&^#|~(){}[]@");



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
    { "using",  TK::Using  },
    { "for",    TK::For    },
    { "in",     TK::In     },
    { "match",  TK::Match  },
};


TokenList Lexer::lex(std::string_view string, std::string &filename) {
    // Reset everything
    tokens = {};
    line = 0;
    lineStart = 0;
    this->filename = filename;
    
    
    auto length = string.length();
    
    for (offset = 0; offset < length; offset++) {
        auto c = string[offset];
        
        if (ignoredCharacters.contains(c)) {
            continue;
        }
        
        // Q: Why is this a lambda?
        // A: We also need to adjust the current source position if we're in multiline strings/comments
        auto handleNewline = [&]() {
            line++;
            lineStart = offset + 1;
        };
        
        if (c == '\n') {
            // TODO this would be a good place to insert semicolons?
            handleNewline();
            continue;
        }
        
        if (c == '/' && string[offset + 1] == '/') {
            // Start of line comment
            while (string[++offset] != '\n');
            handleNewline();
            continue;
        }
        
        if (c == '/' && string[offset + 1] == '*') {
            // start of comment block
            // Note that we deliberately don't check whether a comment's end is within a string literal
            offset += 2;
            while (string[offset++] != '*' && string[offset] != '/') {
                if (string[offset] == '\n') handleNewline();
            }
            continue;
        }
        
        // Offset after returning is the character after the end of the escaped character
        // For example, if we're parsing `'\123x`, the Offset after returhing would "point" to the x
        auto parseEscapedCharacter = [&] () -> char {
            LKAssert(string[offset] == '\\');
            if (string[offset + 1] == '\\') {
                // Escaped backslash
                offset++;
                return '\\';
            }
            
            auto x = string[++offset];
            // Offset at this point is now the character after the backslash
            if (x == 'x' || octalDigits.contains(x)) { // Hex or octal value
                auto isHex = x == 'x';
                std::string rawValue;
                
                if (isHex) {
                    rawValue.push_back(string[++offset]);
                    rawValue.push_back(string[++offset]);
                    offset++;
                } else {
                    while (octalDigits.contains(string[offset]) && rawValue.length() < 3) {
                        rawValue.push_back(string[offset++]);
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
            c = string[++offset];
            char content;
            
            if (c == '\'') {
                throw; // Empty char literal
            } else if (c == '\\') {
                content = parseEscapedCharacter();
            } else {
                content = c;
                offset++;
            }
            
            LKAssert(string[offset] == '\'');
            auto token = handleRawToken("", TK::CharLiteral);
            token->data = content;
            continue;
        }
        
        
        bool isByteStringLiteral = false;
        bool isRawStringLiteral = false;
        
        if (c == 'b' && string[offset + 1 + (string[offset + 1] == 'r')] == DOUBLE_QUOTE) {
            isByteStringLiteral = true;
            offset++;
        }
        
        if (string[offset] == 'r' && string[offset + 1] == DOUBLE_QUOTE) { // Raw (unescaped) string literal
            isRawStringLiteral = true;
            offset++;
        }
        
        if (string[offset] == DOUBLE_QUOTE) { // String literal
            std::string content;
            offset++;
            // Offset is at first char after opening quotes
            if (isRawStringLiteral) {
                for (auto c = string[offset]; c != DOUBLE_QUOTE; c = string[++offset]) {
                    content.push_back(c);
                    if (c == '\n') handleNewline();
                }
            } else {
                for (auto c = string[offset]; c != DOUBLE_QUOTE; c = string[offset]) {
                    if (c == '\\') {
                        content.push_back(parseEscapedCharacter());
                    } else {
                        if (c == '\n') handleNewline();
                        content.push_back(c);
                        offset++;
                    }
                }
            }
            LKAssert(string[offset++] == DOUBLE_QUOTE);
            
            auto token = handleRawToken("", isByteStringLiteral ? TK::ByteStringLiteral : TK::StringLiteral);
            token->data = content;
            offset--; // unavoidable :/
            continue;
        }
        
        // If either of these is true, the string should've been handled above
        LKAssert(!isRawStringLiteral && !isByteStringLiteral);
        
        if (singleCharTokens.contains(c)) {
            std::string x(1, c);
            handleRawToken(x);
            continue;
        }
        
        if (identifierStartCharacters.contains(c)) {
            std::string ident;
            do {
                ident.push_back(c);
                c = string[++offset];
            } while (identifierCharacters.contains(c));
            handleRawToken(ident);
            offset--;
            continue;
        }
        
        if (decimalDigits.contains(c)) { // Number literal
            uint8_t base = 10;
            std::string rawValue;
            auto next = string[offset + 1];
            
            if (c == '0') { // binary/octal/decimal/hex ?
                if (next == 'b') {
                    offset += 2;
                    base = 2;
                } else if (next == 'x') {
                    offset += 2;
                    base = 16;
                } else if (octalDigits.contains(next)) {
                    base = 8;
                } else {
                    // A single 0, not followed by another numeric digit
                    LKAssert(!hexadecimalDigits.contains(next));
                }
            }
            
            // The hex digits charset contains all other digits as well, so it makes most sense to use that.
            // Invalid characters will be detected by `std::stoll` below
            
            do {
                rawValue.push_back(string[offset]);
            } while (hexadecimalDigits.contains(string[++offset]));
            
            
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
            
            auto token = handleRawToken("", TK::IntegerLiteral);
            token->data = value;
            offset--; // unavoidable :/
            continue;
        }
        LKFatalError("unhandled character: '%c'", string[offset]);
    }
    
    handleRawToken("", TK::EOF_);
    return tokens;
}


// this might seems stupid, but it does in fact save 2 string comparisons per identifier parsed
uint8_t _isBoolLiteral(std::string_view str) {
    if (str == "false") return 1;
    else if (str == "true") return 2;
    else return 0;
}


Token *Lexer::handleRawToken(const std::string &rawToken, Token::TokenKind tokenKind) {
    std::shared_ptr<Token> token;
    
    if (tokenKind != TK::Unknown) {
        token = Token::WithKind(tokenKind);
    }
    
    else if (tokenKindMappings.find(rawToken) != tokenKindMappings.end()) { // TODO turn this into a `if (auto x; x== ...)`
        token = Token::WithKind(tokenKindMappings[rawToken]);
    }
    
    else if (identifierStartCharacters.contains(rawToken.at(0)) && identifierCharacters.containsAllCharactersInString(rawToken.substr(1))) {
        if (auto val = _isBoolLiteral(rawToken); val != 0) {
            token = Token::BoolLiteral(val - 1);
        } else {
            token = Token::Identifier(rawToken);
        }
    }
    
    
    if (!token) {
        LKLog("didn't initialize token for '%s'", rawToken.c_str());
        exit(EXIT_FAILURE);
    }
    
    auto column = columnRelativeToCurrentLine();
    token->sourceLocation = TokenSourceLocation(filename, line + 1, column + 1, offset - column);
    
    tokens.push_back(token);
    return token.get();
}

