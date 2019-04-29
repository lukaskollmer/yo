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


using TK = Token::TokenKind;

static constexpr char DOUBLE_QUOTE = '"';

static CharacterSet Letters({{'a', 'z'}, {'A', 'Z'}});
static CharacterSet IgnoredCharacters(" "); // TODO make this "all whitespace" instead
static CharacterSet Delimiters(" .,+-*/;:=<>!&^#|~(){}[]@\n");
static CharacterSet BinaryDigits("01");
static CharacterSet OctalDigits("01234567");
static CharacterSet DecimalDigits("0123456789");
static CharacterSet HexadecimalDigits("0123456789abcdef");
static CharacterSet IdentifierStartCharacters = Letters.Joined(CharacterSet("_"));
static CharacterSet IdentifierCharacters = IdentifierStartCharacters.Joined(DecimalDigits);
static CharacterSet SingleCharTokens(".,+-*/;:=<>!&^#|~(){}[]@");



// TODO no idea if/how this would work, but it'd be cool to have a compile-time assertion that the mapping below is complete
static std::map<std::string, Token::TokenKind> TokenKindMappings = {
    { "(" , TK::OpeningParens },
    { ")" , TK::ClosingParens },
    { "{" , TK::OpeningCurlyBraces },
    { "}" , TK::ClosingCurlyBraces },
    { "[" , TK::OpeningSquareBrackets },
    { "]" , TK::ClosingSquareBrackets },
    
    { "," , TK::Comma },
    { "!" , TK::ExclamationMark },
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
    { "extern", TK::Extern },
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


TokenList Lexer::Lex(std::string_view String, std::string &Filename) {
    // Reset everything
    Tokens = {};
    Line = 0;
    LineStart = 0;
    this->Filename = Filename;
    
    
    auto Length = String.length();
    
    for (Offset = 0; Offset < Length; Offset++) {
        auto C = String[Offset];
        
        if (IgnoredCharacters.Contains(C)) {
            continue;
        }
        
        // Q: Why is this a lambda?
        // A: We also need to adjust the current source position if we're in multiline strings/comments
        auto HandleNewline = [&]() {
            Line++;
            LineStart = Offset + 1;
        };
        
        if (C == '\n') {
            // TODO this would be a good place to insert semicolons?
            HandleNewline();
            continue;
        }
        
        if (C == '/' && String[Offset + 1] == '/') {
            // Start of line comment
            while (String[++Offset] != '\n');
            HandleNewline();
            continue;
        }
        
        if (C == '/' && String[Offset + 1] == '*') {
            // start of comment block
            // Note that we deliberately don't check whether a comment's end is within a string literal
            Offset += 2;
            while (String[Offset++] != '*' && String[Offset] != '/') {
                if (String[Offset] == '\n') HandleNewline();
            }
            continue;
        }
        
        // Offset after returning is the character after the end of the escaped character
        // For example, if we're parsing `'\123x`, the Offset after returhing would "point" to the x
        auto ParseEscapedCharacter = [&] () -> char {
            precondition(String[Offset] == '\\');
            if (String[Offset + 1] == '\\') {
                // Escaped backslash
                Offset++;
                return '\\';
            }
            
            auto X = String[++Offset];
            // Offset at this point is now the character after the backslash
            if (X == 'x' || OctalDigits.Contains(X)) { // Hex or octal value
                auto IsHex = X == 'x';
                std::string RawValue;
                
                if (IsHex) {
                    RawValue.push_back(String[++Offset]);
                    RawValue.push_back(String[++Offset]);
                    Offset++;
                } else {
                    while (OctalDigits.Contains(String[Offset]) && RawValue.length() < 3) {
                        RawValue.push_back(String[Offset++]);
                    }
                }
                
                std::size_t Length;
                char Value;
                try {
                    Value = std::stoi(RawValue, &Length, IsHex ? 16 : 8);
                } catch (...) {
                    throw;
                }
                
                precondition(Length == RawValue.length());
                return Value;
            }
            
            switch (X) {
                case 'n': Offset++; return '\n';
                case 't': Offset++; return '\t';
                default: break;
            }
            LKFatalError("Unable to parse escaped character");
        };
        

        if (C == '\'') { // Char literal
            C = String[++Offset];
            char content;
            
            if (C == '\'') {
                throw; // Empty char literal
            } else if (C == '\\') {
                content = ParseEscapedCharacter();
            } else {
                content = C;
                Offset++;
            }
            
            precondition(String[Offset] == '\'');
            auto T = HandleRawToken("", TK::CharLiteral);
            T->Data.C = content;
            continue;
        }
        
        
        bool IsByteStringLiteral = false;
        bool IsRawStringLiteral = false;
        
        if (C == 'b' && String[Offset + 1 + (String[Offset + 1] == 'r')] == DOUBLE_QUOTE) {
            IsByteStringLiteral = true;
            Offset++;
        }
        
        if (String[Offset] == 'r' && String[Offset + 1] == DOUBLE_QUOTE) { // Raw (unescaped) string literal
            IsRawStringLiteral = true;
            Offset++;
        }
        
        if (String[Offset] == DOUBLE_QUOTE) { // String literal
            std::string Content;
            Offset++;
            // Offset is at first char after opening quotes
            if (IsRawStringLiteral) {
                for (auto C = String[Offset]; C != DOUBLE_QUOTE; C = String[++Offset]) {
                    Content.push_back(C);
                    if (C == '\n') HandleNewline();
                }
            } else {
                for (auto C = String[Offset]; C != DOUBLE_QUOTE; C = String[Offset]) {
                    if (C == '\\') {
                        Content.push_back(ParseEscapedCharacter());
                    } else {
                        if (C == '\n') HandleNewline();
                        Content.push_back(C);
                        Offset++;
                    }
                }
            }
            precondition(String[Offset++] == DOUBLE_QUOTE);
            
            auto T = HandleRawToken("", IsByteStringLiteral ? TK::ByteStringLiteral : TK::StringLiteral);
            precondition(T->Data.S == nullptr);
            T->Data.S = new std::string(Content);
            Offset--; // unavoidable :/
            continue;
        }
        
        // If either of these is true, the string should've been handled above
        precondition(!IsRawStringLiteral && !IsByteStringLiteral);
        
        if (SingleCharTokens.Contains(C)) {
            std::string X(1, C);
            HandleRawToken(X);
            continue;
        }
        
        if (IdentifierStartCharacters.Contains(C)) {
            std::string Ident;
            do {
                Ident.push_back(C);
                C = String[++Offset];
            } while (IdentifierCharacters.Contains(C));
            HandleRawToken(Ident);
            Offset--;
            continue;
        }
        
        if (DecimalDigits.Contains(C)) { // Number literal
            uint8_t Base = 10;
            std::string RawValue;
            auto Next = String[Offset + 1];
            
            if (C == '0') { // binary/octal/decimal/hex ?
                if (Next == 'b') {
                    Offset += 2;
                    Base = 2;
                } else if (Next == 'x') {
                    Offset += 2;
                    Base = 16;
                } else if (OctalDigits.Contains(Next)) {
                    Base = 8;
                } else {
                    // A single 0, not followed by another numeric digit
                    precondition(!HexadecimalDigits.Contains(Next));
                }
            }
            
            // The hex digits charset contains all other digits as well, so it makes most sense to use that.
            // Invalid characters will be detected by `std::stoll` below
            
            do {
                RawValue.push_back(String[Offset]);
            } while (HexadecimalDigits.Contains(String[++Offset]));
            
            
            uint64_t Value;
            std::size_t Length;
            
            try {
                Value = std::stoll(RawValue, &Length, Base);
            } catch (const std::exception &e) {
                std::cout << e.what() << std::endl;
                throw;
            }
            
            // TODO turn this into a nice error message
            precondition(Length == RawValue.length()); // The string contained illegal characters (eg: 0b110012)
            
            auto T = HandleRawToken("", TK::IntegerLiteral);
            T->Data.I = Value;
            Offset--; // unavoidable :/
            continue;
        }
        LKFatalError("unhandled character: '%c'", String[Offset]);
    }
    
    HandleRawToken("", TK::EOF_);
    return Tokens;
}


// this might seems stupid, but it does in fact save 2 string comparisons per identifier parsed
uint8_t _isBoolLiteral(std::string_view str) {
    if (str == "false") return 1;
    else if (str == "true") return 2;
    else return 0;
}


Token *Lexer::HandleRawToken(const std::string &RawToken, Token::TokenKind TokenKind) {
    std::shared_ptr<Token> Token;
    
    if (TokenKind != TK::Unknown) {
        Token = Token::WithKind(TokenKind);
    }
    
    else if (TokenKindMappings.find(RawToken) != TokenKindMappings.end()) {
        Token = Token::WithKind(TokenKindMappings[RawToken]);
    }
    
    else if (IdentifierStartCharacters.Contains(RawToken.at(0)) && IdentifierCharacters.ContainsAllCharactersInString(RawToken.substr(1))) {
        if (auto Val = _isBoolLiteral(RawToken); Val != 0) {
            Token = Token::BoolLiteral(Val - 1);
        } else {
            Token = Token::Identifier(RawToken);
        }
    }
    
    
    if (!Token) {
        LKLog("didn't initialize token for '%s'", RawToken.c_str());
        exit(EXIT_FAILURE);
    }
    
    auto Column = ColumnRelatoveToCurrentLine();
    Token->SourceLocation = TokenSourceLocation(Filename, Line + 1, Column + 1, Offset - Column);
    
    Tokens.push_back(Token);
    return Token.get();
}

