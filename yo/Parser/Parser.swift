//
//  Parser.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// MARK: Constants
private let BinopTokenTypes: [TokenType] = [.plus, .minus, .asterik, .forwardSlash, .percentageSign]


// MARK: Errors
enum ParserError: Error {
    case expectedIdentifier // expected an identifier, but didn't get one
    case other(String)
}

private func unhandledToken(_ token: Token) -> Never {
    fatalError("unhandled token \(token)")
}

// MARK: Parser
class Parser {
    private let tokens: [Token]
    private var currentPosition = 0
    
    init(tokens: [Token]) {
        self.tokens = tokens
    }
    
    
    func parse() throws -> [ASTNode] {
        var topLevelExpressions = [ASTNode]()
        
        while currentPosition < tokens.count {
            topLevelExpressions.append(try parseStatement(isTopLevel: true))
        }
        
        return topLevelExpressions
    }
}


// MARK: - Helper functions
private extension Parser {
    private func peek(_ distance: Int = 1) -> Token {
        return tokens[currentPosition + distance]
    }
    
    @discardableResult
    private func next() -> Token {
        // TODO stop incrementing on EOF?
        currentPosition += 1
        return tokens[currentPosition]
    }
    
    var currentToken: Token {
        return tokens[currentPosition]
    }
}

// MARK: Parsing
private extension Parser {
    
    
    func parseStatement(isTopLevel: Bool = false) throws -> ASTNode {
        
        if isTopLevel {
            // Currently parsing a top level expression
            // The only allowed top level statement is `fn` // TODO add import and type
            
            switch currentToken.type {
            case .fn:
                return try parseFunction()
            case .EOF:
                currentPosition += 1 // stop parsing
                return ASTNoop()
            default: fatalError("unexpected top level token: \(currentToken)")
            }
            
        } else {
            // not a top level statement
            switch currentToken.type {
            case .ret:
                next()
                let expression = try parseExpression()
                guard case .semicolon = currentToken.type else {
                    fatalError("expected a semicolon")
                    //throw ParserError.other("expected a semicolon")
                }
                next()
                return ASTReturnStatement(returnValueExpression: expression)
                
            default: fatalError("unhandled token \(currentToken)")
            }
        }
        
        fatalError()
    }
    
    
    func parseFunction() throws -> ASTNode {
        print(#function)
        guard
            case .fn = currentToken.type,
            case .identifier(let functionName) = next().type,
            case .openingParentheses = next().type
        else {
            throw ParserError.expectedIdentifier
        }
        
        next() // step into the function signature
        var parameters = [ASTIdentifier]()
        
        while let parameter = try? parseIdentifier() {
            parameters.append(parameter)
            if case .comma = currentToken.type {
                next()
            }
        }
        
        guard case .closingParentheses = currentToken.type else {
            fatalError("expected closing parens after function signature")
        }
        
        guard case .openingCurlyBrackets = next().type else {
            fatalError("expected a { after the function signature")
            //throw ParserError.other("expected a { after the function signature")
        }
        
        next() // step into the function body
        
        var functionBody = [ASTNode]()
        functionBody.append(try parseStatement())
        
        guard case .closingCurlyBrackets = currentToken.type else {
            fatalError("expected a } after the function body")
            //throw ParserError.other("expected a } after the function body")
        }
        
        next() // step out of the function body
        
        return ASTFunctionDeclaration(
            name: ASTIdentifier(name: functionName),
            parameters: parameters,
            localVariables: [],
            body: functionBody
        )
    }
    
    
    
    
    
    
    
    
    func parseExpression() throws -> ASTExpression {
        Log.info("\(#function), \(currentToken)")
        
        guard let expression: ASTExpression = (try? parseNumberLiteral()) ?? (try? parseIdentifier()) else {
            //fatalError("ugh")
            throw ParserError.other("unable to find an expression. got \(currentToken) instead")
        }
        
        if BinopTokenTypes.contains(currentToken.type), let binopOperator = ASTBinop.Operator(tokenType: currentToken.type) {
            next()
            return ASTBinop(lhs: expression, operator: binopOperator, rhs: try parseExpression())
        }
        
        if let identifier = expression as? ASTIdentifier, case TokenType.openingParentheses = currentToken.type {
            // identifier, followed by an opening parentheses -> function call
            next() // jump into the function call
            
            var arguments = [ASTExpression]()
            
            while let argument = try? parseExpression() {
                arguments.append(argument)
                
                if case .comma = currentToken.type {
                    next()
                }
            }
            
            
            if case .closingParentheses = currentToken.type {
                next()
                return ASTFunctionCall(functionName: identifier.name, arguments: arguments)
            }
            
            fatalError("aaargh")
            
        }
        
        return expression
    }
    
    
    
    
    func parseNumberLiteral() throws -> ASTNumberLiteral {
        if case .numberLiteral(let value) = currentToken.type {
            next()
            return ASTNumberLiteral(value: value)
        }
        
        throw ParserError.other("not a number")
    }
    
    
    
    func parseIdentifier() throws -> ASTIdentifier {
        if case .identifier(let name) = currentToken.type {
            next()
            return ASTIdentifier(name: name)
        }
        
        throw ParserError.other("not an identifier")
    }
}

