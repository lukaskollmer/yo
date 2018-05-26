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
private let BinaryConditionTokenTypes: [TokenType] = [.doubleAmpersand, .doublePipe]


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
        currentPosition += 1
        return tokens[currentPosition]
    }
    
    var currentToken: Token {
        return tokens[currentPosition]
    }
}

// MARK: Parsing
private extension Parser {
    
    
    // MARK: Statements
    
    func parseStatement(isTopLevel: Bool = false) throws -> ASTStatement {
        
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
                
            case .if, .while:
                let initialTokenType = currentToken.type
                
                next()
                var condition = try parseCondition()
                
                if let op = ASTBinaryCondition.Operator(tokenType: currentToken.type) {
                    next()
                    let rhs = try parseCondition()
                    condition = ASTBinaryCondition(lhs: condition, operator: op, rhs: rhs)
                }
                
                guard case .openingCurlyBrackets = currentToken.type else {
                    fatalError("expected { after condition")
                }
                
                let body = try parseComposite()
                
                if case .while = initialTokenType {
                    return ASTConditionalStatement(condition: condition, body: body, kind: .while)
                }
                
                // if statement, which means we also need to check for a potential else branch
                var elseBody: ASTComposite?
                
                if case .else = currentToken.type {
                    next()
                    elseBody = try parseComposite()
                }
                return ASTConditionalStatement(condition: condition, body: body, kind: .if(elseBody))
                
                
                
            case .val: // variable declaration
                next()
                let name = try parseIdentifier()
                guard case .colon = currentToken.type else {
                    // TODO a colon is only required in some instances:
                    // val foo: int;
                    // val foo = bar(); // not required in this case since we can infer the type from bar's return type
                    // TODO implement that
                    fatalError("missing colon after variable name")
                }
                next()
                let typename = try parseIdentifier()
                guard case .semicolon = currentToken.type else {
                    fatalError("variable declaration should end w/ a semicolon")
                }
                next()
                return ASTVariableDeclaration(identifier: name, typename: typename)
                
                
            case .identifier(let name):
                // a statement starting e/ an identifier can be:
                // - a simple assignment
                // - a subscript assignment // TODO
                // - a function call with discarded return value // TODO
                let expression = try parseExpression()
                
                if case .equalsSign = currentToken.type {
                    next()
                    let assignedValue = try parseExpression()
                    guard case .semicolon = currentToken.type else {
                        fatalError("assignment should end w/ semicolon")
                    }
                    
                    next()
                    return ASTAssignment(target: expression, value: assignedValue)
                    
                }
                fatalError()
                
                
                
            default: fatalError("unhandled token \(currentToken)")
            }
        }
        
        fatalError()
    }
    
    
    func parseFunction() throws -> ASTFunctionDeclaration {
        guard
            case .fn = currentToken.type,
            case .identifier(let functionName) = next().type,
            case .openingParentheses = next().type
        else {
            throw ParserError.expectedIdentifier
        }
        
        next() // step into the function signature
        var parameters = [ASTVariableDeclaration]()
        
        while let parameter = try? parseIdentifier() {
            guard case .colon = currentToken.type else {
                fatalError()
            }
            next()
            parameters.append(ASTVariableDeclaration(identifier: parameter, typename: try parseIdentifier()))
            if case .comma = currentToken.type {
                next()
            }
        }
        
        guard
            case .closingParentheses = currentToken.type,
            case .colon = next().type
        else {
            fatalError("expected closing parens after function signature")
        }
        next()
        let returnType = try parseIdentifier()
        
        let functionBody = try parseComposite().statements
        let localVariables = functionBody.filter { $0 is ASTVariableDeclaration } as! [ASTVariableDeclaration]
        
        return ASTFunctionDeclaration(
            name: ASTIdentifier(name: functionName),
            parameters: parameters,
            returnType: returnType,
            localVariables: localVariables,
            body: functionBody
        )
    }
    
    func parseComposite() throws -> ASTComposite {
        guard case .openingCurlyBrackets = currentToken.type else {
            fatalError("composite has to start with opening curly braces")
        }
        
        next() // step into the composite
        
        var statements = [ASTStatement]()
        
        while let statement = try? parseStatement() {
            statements.append(statement)
            if case .closingCurlyBrackets = currentToken.type {
                next()
                break
            }
        }
        
        return ASTComposite(statements: statements)
    }
    
    
    
    
    
    // MARK: Expressions
    
    
    func parseExpression() throws -> ASTExpression {
        
        if case .minus = currentToken.type { // unary
            next()
            return ASTUnary(expression: try parseExpression())
        }
        
        if case .openingParentheses = currentToken.type {
            next()
            let expression = try parseExpression()
            guard case .closingParentheses = currentToken.type else {
                fatalError(") missing after expr starting with (")
            }
            
            next()
            return expression
        }
        
        guard let expression: ASTExpression = (try? parseNumberLiteral()) ?? (try? parseIdentifier()) else {
            //fatalError("ugh")
            throw ParserError.other("unable to find an expression. got \(currentToken) instead")
        }
        
        if BinopTokenTypes.contains(currentToken.type), let binopOperator = ASTBinop.Operator(tokenType: currentToken.type) {
            next()
            return ASTBinop(lhs: expression, operator: binopOperator, rhs: try parseExpression())
        }
        
        if let identifier = expression as? ASTIdentifier, case .openingParentheses = currentToken.type {
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
    
    
    
    
    func parseCondition() throws -> ASTCondition {
        // TODO add support for `<cond> && <cond>` and `!<expr>` conditions
        
        let lhs = try parseExpression()
        let comparisonOperator = ASTComparison.Operator(tokenType: currentToken.type)! // TODO don't force unwrap this
        next()
        let rhs = try parseExpression()
        
        return ASTComparison(lhs: lhs, operator: comparisonOperator, rhs: rhs)
        
    }
}


