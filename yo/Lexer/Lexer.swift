//
//  Lexer.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation



// MARK: Constants

private let identifierStartCharacters = CharacterSet.letters.union(.init(charactersIn: "_"))
private let identifierCharacters = CharacterSet.alphanumerics.union(identifierStartCharacters)

// set of characters we ignore
private let ignoredCharacters: [Unicode.Scalar] = [" "] // TODO what about CharacterSet.whitespace?
// set of characters that delimit whetever we're currently lexing (ie the x in `(x` )
private let delimiters = CharacterSet(charactersIn: " .,+-*/;:=<>&^|~(){}[]\n") // TODO give this a more descriptive name

private let tokenMapping: [String: TokenType] = [
    "."     : .period,
    ","     : .comma,
    ":"     : .colon,
    ";"     : .semicolon,
    "("     : .openingParentheses,
    ")"     : .closingParentheses,
    "{"     : .openingCurlyBrackets,
    "}"     : .closingCurlyBrackets,
    "["     : .openingSquareBrackets,
    "]"     : .closingSquareBrackets,
    
    "+"     : .plus,
    "-"     : .minus,
    "*"     : .asterik,
    "/"     : .forwardSlash,
    "%"     : .percentageSign,
    
    // assignment
    "="     : .equalsSign,
    
    // comparisions
    "!"     : .exclamationMark,
    "<"     : .less,
    ">"     : .greater,
    
    "&"     : .ampersand,
    "|"     : .pipe,
    "^"     : .circumflex,
    "~"     : .tilde,
    
    // keywords
    "use"   : .use,
    "type"  : .type,
    "impl"  : .impl,
    "static": .static,
    "fn"    : .fn,
    "ret"   : .ret,
    "val"   : .val,
    "if"    : .if,
    "else"  : .else,
    "while" : .while
    
]


// MARK: Lexer

enum LexerError: Error {
    case unknownToken(String)
}

class Lexer {
    let source: String
    private var tokens = [Token]()
    
    init(source: String) {
        self.source = source
    }
    
    
    func tokenize() throws -> [Token] {
        var currentToken = ""
        
        let scalars = source.unicodeScalars.map { $0 }
        
        // set to true when we encounter two forward slashes
        // we then ignore everything until the next line break
        var currentlyParsingComment = false
        var currentlyParsingStringLiteral = false
        
        for (index, char) in scalars.enumerated() { // tbh i have no idea what i'm doing here
            if (ignoredCharacters.contains(char) && !currentlyParsingStringLiteral) || (!currentlyParsingComment && char == "\n") { continue }
            
            currentToken.unicodeScalars.append(char)
            
            let isLast = index == scalars.count - 1
            if !isLast {
                let next = scalars[index + 1]
                
                // Comment parsing
                
                if currentlyParsingComment && char == "\n" {
                    // currently parsing a comment & reached a newline
                    currentlyParsingComment = false
                    currentToken = ""
                    continue
                }
                
                if currentToken == "/" && next == "/" || currentToken.hasPrefix("//") {
                    // comment -> skip to next line
                    currentlyParsingComment = true
                    continue
                }
                
                
                // String literal parsing
                
                if currentToken == "\"" && !currentlyParsingStringLiteral {
                    currentlyParsingStringLiteral = true
                    continue
                }
                
                if char == "\"" && currentlyParsingStringLiteral {
                    // TODO continue string literal parsing if the previous character was \ (escape sequence)
                    currentlyParsingStringLiteral = false
                    let string = NSString(string: currentToken).substring(with: NSRange(location: 1, length: currentToken.count - 2))
                    try handleRawToken(currentToken, endLocation: index, type: TokenType.stringLiteral(string))
                    currentToken = ""
                    continue
                }
                
                
                
                
                // handle the current token, if
                // a) the next scalar is a delimiter
                // b) the current character is a delimiter (this catches things like `(x` in a function signature)
                if !currentlyParsingStringLiteral && (delimiters.contains(next) || delimiters.contains(char)) {
                    try handleRawToken(currentToken, endLocation: index)
                    currentToken = ""
                }
            } else {
                try handleRawToken(currentToken, endLocation: index)
            }
        }
        
        tokens.append(Token(type: .EOF, range: scalars.endIndex..<scalars.endIndex))
        
        return tokens
    }
    
    private func handleRawToken(_ rawToken: String, endLocation: Int, type: TokenType? = nil) throws {
        
        let start = (endLocation + 1) - rawToken.count
        tokens.append(Token(type: type != nil ? type! : try getType(token: rawToken), range: start..<endLocation))
    }
    
    
    private func getType(token: String) throws -> TokenType {
        
        if token.allScalarsInCharacterSet(.decimalDigits) { // TODO instead of the check, just call the Int/Double initializer w/ the string and return that if nonnull?
            return .numberLiteral(Int(token)!)
        }
        
        if let type = tokenMapping[token] {
            return type
        }
        
        if identifierStartCharacters.contains(token.unicodeScalars.first!) && token.allScalarsInCharacterSet(identifierCharacters) {
            return .identifier(token)
        }
        
        
        // should never reach here
        throw LexerError.unknownToken(token)
    }
}
