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
private let binaryLiteralCharacters = CharacterSet(charactersIn: "b01")
private let hexadecimalLiteralCharacters = CharacterSet(charactersIn: "x0123456789abcdef")
private let floatLiteralCharacters = CharacterSet.decimalDigits.union(.init(charactersIn: "."))

// set of characters we ignore
private let ignoredCharacters: [Unicode.Scalar] = [" "] // TODO what about CharacterSet.whitespace?
// set of characters that delimit whetever we're currently lexing (ie the x in `(x` )
private let delimiters = CharacterSet(charactersIn: " .,+-*/;:=<>&^|~(){}[]@\n") // TODO give this a more descriptive name

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
    
    "@"     : .atSign,
    "#"     : .hashtag,
    "?"     : .questionmark,
    
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
    "while" : .while,
    "for"   : .for,
    "in"    : .in,
    "break" : .break,
    "continue" : .continue,
    "__asm" : .__asm,
    "protocol" : .protocol,
    "null"  : .null,
    "as"  : .as,
    
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
        var lastScalarWasEscapeSequenceSignal = false
        
        for (index, char) in scalars.enumerated() { // tbh i have no idea what i'm doing here
            if (ignoredCharacters.contains(char) && !currentlyParsingStringLiteral) || (!currentlyParsingComment && !currentlyParsingStringLiteral && char == "\n") { continue }
            
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
                
                
                
                if currentlyParsingStringLiteral {
                    if char == "\\" {
                        
                        // support escaped escaped sequences
                        // Examples:
                        // - "\\n" -> "\n"
                        // - "\\\n" -> "\\n" (ie backslash + newline)
                        if lastScalarWasEscapeSequenceSignal {
                            currentToken.unicodeScalars.removeLast()
                        }
                        
                        lastScalarWasEscapeSequenceSignal = !lastScalarWasEscapeSequenceSignal
                        continue
                    }
                } else {
                    lastScalarWasEscapeSequenceSignal = false
                }
                
                
                if currentlyParsingStringLiteral && lastScalarWasEscapeSequenceSignal {
                    switch char {
                    case "n":
                        currentToken.unicodeScalars.removeLast()
                        currentToken.unicodeScalars.removeLast()
                        currentToken += "\n"
                        
                        // TODO add more characters that require special handling in escape sequences (\t, etc?)
                        
                    default:
                        fatalError("invalid escape sequence in string literal: '\\\(next)'") // TODO incorrect syntax highlighting (all 3 \s are white, only the last one should be. File radar!
                    }
                }
                
                
                
                
                
                
                
                // handle the current token, if
                // a) the next scalar is a delimiter
                // b) the current character is a delimiter (this catches things like `(x` in a function signature)
                if !currentlyParsingStringLiteral && (delimiters.contains(next) || delimiters.contains(char)) {
                    // TODO comment this to explain what's going on
                    let isCurrentlyParsingFloatLiteral =
                        (currentToken.allScalarsInCharacterSet(.decimalDigits) && next == "." && CharacterSet.decimalDigits.contains(scalars[index + 2]))
                    || (char == "." && currentToken.anyScalarInCharacterSet(.decimalDigits) && currentToken.allScalarsInCharacterSet(floatLiteralCharacters) && CharacterSet.decimalDigits.contains(next) )
                    
                    if isCurrentlyParsingFloatLiteral { continue }
                    
                    try handleRawToken(currentToken, endLocation: index)
                    currentToken = ""
                }
            } else {
                // TODO seems like we never reach here. investigate!
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
        
        if token.allScalarsInCharacterSet(.decimalDigits), let value = Int(token) { // TODO instead of the check, just call the Int/Double initializer w/ the string and return that if nonnull?
            return .integerLiteral(value)
        }
        
        if let type = tokenMapping[token] {
            return type
        }
        
        if identifierStartCharacters.contains(token.unicodeScalars.first!) && token.allScalarsInCharacterSet(identifierCharacters) {
            return .identifier(token)
        }
        
        if token.hasPrefix("0b") && token.allScalarsInCharacterSet(binaryLiteralCharacters) {
            return .integerLiteral(Int(token.replacingOccurrences(of: "0b", with: ""), radix: 2)!)
            
        } else if token.hasPrefix("0x") && token.allScalarsInCharacterSet(hexadecimalLiteralCharacters) {
            return .integerLiteral(Int(token.replacingOccurrences(of: "0x", with: ""), radix: 16)!)
        }
        
        if token.split(separator: ".").count == 2, let value = Double(token) {
            return .doubleLiteral(value)
        }
        
        
        // should never reach here
        throw LexerError.unknownToken(token)
    }
}
