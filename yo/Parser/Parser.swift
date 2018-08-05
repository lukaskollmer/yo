//
//  Parser.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// MARK: Constants

// Note: this array only contains the first token of multi token operators (ie `<` instead of `<<`)
let BinaryOperationTokens: [TokenType] = [
    // arithmetic operations
    .plus, .minus, .asterik, .forwardSlash, .percentageSign,
    
    // bitwise operations
    .ampersand, .pipe, .circumflex, .tilde, .less, .greater
]

// MARK: Errors
enum ParserError: Error {
    case unexpectedToken(Token)
    case other(String)
}

// MARK: Parser
class Parser {
    let tokens: [Token]
    var currentPosition = 0
    
    // Used to access parsed annotations across different functions
    var annotations = [ASTAnnotation]()
    
    init(tokens: [Token]) {
        self.tokens = tokens
    }
    
    func parse() throws -> AST {
        var topLevelExpressions = AST()
        
        while currentPosition < tokens.count {
            topLevelExpressions.append(try parseStatement(isTopLevel: true))
        }
        
        return topLevelExpressions
    }
}

extension Parser {
    func peek(_ distance: Int = 1) -> Token {
        return tokens[currentPosition + distance]
    }
    
    @discardableResult
    func next() -> Token {
        currentPosition += 1
        return tokens[currentPosition]
    }
    
    var currentToken: Token {
        return tokens[currentPosition]
    }
}
