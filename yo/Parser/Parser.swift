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
        
        var arguments = [String]()
        
        // TODO this can be improved
        var finishedSignatureParsing = false
        repeat {
            print(peek().type)
            switch next().type {
            case .closingParentheses:
                finishedSignatureParsing = true
            case .identifier(let argumentName):
                arguments.append(argumentName)
            default:
                fatalError("function argument declaration isn't an identifier (got \(currentToken.type)")
                //throw ParserError.other("function argument declaration isn't an identifier (got \(currentToken.type)")
            }
        } while !finishedSignatureParsing
        
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
        
        return ASTFunctionDeclaration(name: functionName, arguments: arguments, body: functionBody)
    }
    
    
    
    
    
    
    
    
    func parseExpression() throws -> ASTExpression {
        Log.info("\(#function), \(currentToken)")
        
        guard let expression: ASTExpression = (try? parseNumberLiteral()) ?? (try? parseIdentifier()) else {
            fatalError("ugh")
        }
        
        if BinopTokenTypes.contains(currentToken.type), let binopOperator = ASTBinop.Operator(tokenType: currentToken.type) {
            next()
            return ASTBinop(lhs: expression, operator: binopOperator, rhs: try parseExpression())
        }
        
        if let identifier = expression as? ASTIdentifier, case TokenType.openingParentheses = currentToken.type {
            // identifier, followed by an opening parentheses -> function call
            
            if case .closingParentheses = next().type {
                next()
                return ASTFunctionCall(functionName: identifier.name, arguments: []) // TODO parse arguments
            }
            
            fatalError("aaargh")
            
        }
        
        return expression
        
        switch currentToken.type {
        case .numberLiteral(let value):
            next() // TODO refactor this out of the switch?
            
            //if let binop = try? parseExpression(), binop is ASTBinop {
            //    Log.info("FOUND A BINOP")
            //}
            
            if BinopTokenTypes.contains(currentToken.type), let binopOperator = ASTBinop.Operator(tokenType: currentToken.type) {
                next()
                let rhs = try parseExpression()
                //return ASTBinop(lhs: value, operator: binopOperator, rhs: rhs)
            }
            
            return ASTNumberLiteral(value: value)
            
        case .identifier(_):
            switch peek().type {
            case .openingParentheses: // function call
                //return try parseFunctionCall()
                break
            default:
                unhandledToken(currentToken)
            }
            
        default:
            unhandledToken(currentToken)
        }
        
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

