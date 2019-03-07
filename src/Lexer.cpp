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


static CharacterSet Letters({{'a', 'z'}, {'A', 'Z'}});


static CharacterSet IgnoredCharacters(" "); // TODO make this "all whitespace" instead
static CharacterSet Delimiters(" .,+-*/;:=<>!&^#|~(){}[]@\n");
static CharacterSet DecimalDigits("0123456789");
static CharacterSet IdentifierStartCharacters = Letters.Joined(CharacterSet("_"));
static CharacterSet IdentifierCharacters = IdentifierStartCharacters.Joined(DecimalDigits);



// TODO now idea if/how this would work, but it'd be cool to have a compile-time assertion that the mapping below is complete
static std::map<std::string, Token::TokenKind> TokenKindMappings = {
    { "(" , Token::TokenKind::OpeningParens },
    { ")" , Token::TokenKind::ClosingParens },
    { "{" , Token::TokenKind::OpeningCurlyBraces },
    { "}" , Token::TokenKind::ClosingCurlyBraces },
    { "," , Token::TokenKind::Comma },
    { ":" , Token::TokenKind::Colon },
    { ";" , Token::TokenKind::Semicolon },
    { "*" , Token::TokenKind::Asterisk },
    
    // Keywords
    { "fn",     Token::TokenKind::Fn     },
    { "return", Token::TokenKind::Return },
};




bool IsWhitespace(char &character) {
    return character == ' '; // TODO add support for other kinds of whitespace
}



std::string RemovingCommentsInLine(std::string &line) {
    auto length = line.length();
    if (length == 0) return "";
    
    bool didOnlyEncounterWhitespaceSoFar = true;
    bool inStringLiteral = false;
    bool escapeNextCharacter = false;
    int inlineCommentStartIndex = -1;
    
    auto isParsingInlineComment = [&inlineCommentStartIndex] () -> bool {
        return inlineCommentStartIndex > -1;
    };
    
    for (int i = 0; i < length; i++) {
        char c = line.at(i);
        
        if (didOnlyEncounterWhitespaceSoFar) {
            if (i < length - 3 && c == '/' && line.at(i + 1) == '/') {
                return "";
            }
            didOnlyEncounterWhitespaceSoFar = IsWhitespace(c);
        }
        
        // Check for end-of-line comments
        // The only caveat here is excluding slashes in string literals
        
        if (!escapeNextCharacter && c == '"' && !isParsingInlineComment()) {
            inStringLiteral = !inStringLiteral;
        }
        
        if (!inStringLiteral && i < length - 1) {
            char nextChar = line.at(i + 1);
            if (c == '/') {
                if (nextChar == '/' && !isParsingInlineComment()) {
                    return line.substr(0, i);
                } else if (nextChar == '*') {
                    inlineCommentStartIndex = i;
                }
            } else if (c == '*' && nextChar == '/' && isParsingInlineComment()) {
                auto length = i - inlineCommentStartIndex + 2;
                auto replacement = std::string(length, ' ');
                line = line.replace(inlineCommentStartIndex, length, replacement);
                
                inlineCommentStartIndex = -1;
                i += 1;
            }
        }
        escapeNextCharacter = !escapeNextCharacter && c == '\\';
    }
    
    return line;
    
    
}

bool StringContainsChar(std::string &String, char Char) {
    return String.find(Char) != std::string::npos;
}



LKUInteger Lexer::LexLine(std::string &line, LKUInteger LineStartPosition) {
    auto length = line.length();
    
    std::string currentToken;
    
    bool lastScalarWasEscapeSequenceSignal = false;
    bool currentlyInStringLiteral = false; // TODO
    
    for (int i = 0; i < length; i++) {
        char c = line.at(i);
        if (IgnoredCharacters.Contains(c)) {
            continue;
        }
        
        currentToken.push_back(c);
        
        auto isLast = i == length - 1;
        
        if (isLast) {
            // TODO ?this is where we should dynamically insert semicolons, depending on the type of the last token?
            HandleRawToken(currentToken, LineStartPosition + i);
            currentToken.clear();
            continue;
        }
        
        // Not last token
        auto next = line.at(i + 1);
        
        
        // Handle the current token, if
        // a) the next char is a delimiter
        // b) the current character is a delimiter (this catches things like `(x` in a function signature
        if (!currentlyInStringLiteral && (Delimiters.Contains(c) || Delimiters.Contains(next))) {
            HandleRawToken(currentToken, LineStartPosition + i);
            currentToken.clear();
        }
    }
    
    return length;
}


TokenList Lexer::Lex(std::string &string) {
    std::stringstream ss(string);
    
    LKUInteger Position = 0;
    std::string line;
    while (std::getline(ss, line, '\n')) {
        line = RemovingCommentsInLine(line);
        Position += LexLine(line, Position);
    }
    HandleRawToken("", Position, Token::TokenKind::EOF_);
    
    return Tokens;
}



void Lexer::HandleRawToken(const std::string &RawToken, LKUInteger EndLocation, Token::TokenKind TokenKind) {
    //printf("[%s] token: '%s', EndLoc: %i, Kind: %i\n", __PRETTY_FUNCTION__, RawToken.c_str(), EndLocation, TokenKind);
    
    std::shared_ptr<Token> Token;
    
    if (TokenKind != Token::TokenKind::Unknown) {
        Token = Token::WithKind(TokenKind);
    }
    
    else if (DecimalDigits.ContainsAllCharactersInString(RawToken)) {
        auto Value = std::stol(RawToken);
        Token = Token::IntegerLiteral(Value);
    }
    
    else if (TokenKindMappings.find(RawToken) != TokenKindMappings.end()) {
        Token = Token::WithKind(TokenKindMappings[RawToken]);
    }
    
    else if (IdentifierStartCharacters.Contains(RawToken.at(0)) && IdentifierCharacters.ContainsAllCharactersInString(RawToken.substr(1))) {
        Token = Token::Identifier(RawToken);
    }
    
    
    
    
    if (!Token) {
        LKLog("didn't initialize token for '%s'", RawToken.c_str());
        exit(EXIT_FAILURE);
    }
    
    auto Start = EndLocation - RawToken.length();
    Token->setSourceLocation(Range(Start, RawToken.length()));
    
    Tokens.push_back(Token);
}





std::string Lexer::RemovingComments(std::string Input) {
    std::stringstream ss(Input);
    
    std::string ReturnValue;
    ReturnValue.reserve(Input.length());
    
    std::string line;
    while (std::getline(ss, line, '\n')) {
        ReturnValue += RemovingCommentsInLine(line);
        ReturnValue += '\n'; // TODO it'd be more efficient if we just left out all newlines, but that means that errors reported by the parser won't have accurate positions
    }
    
    return ReturnValue;
}
