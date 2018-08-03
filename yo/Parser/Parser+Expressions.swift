//
//  Parser+Expressions.swift
//  yo
//
//  Created by Lukas Kollmer on 06.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


extension Parser {
    
    func parseExpression() throws -> ASTExpression {
        
        if case .nil = currentToken.type {
            next()
            return ASTNumberLiteral(value: 0).as(.id) // TODO make this a macro instead?
        }
        
        if [TokenType.true, .false].contains(currentToken.type) {
            let value = currentToken.type == .true ? true : false
            next()
            return ASTBooleanLiteral(value: value)
        }
        
        if case .exclamationMark = currentToken.type {
            next()
            let expression = try parseExpression()
            return ASTUnaryExpression(expression: expression, operator: .logicalNegation)
        }
        
        if case .atSign = currentToken.type {
            next()
            let expression = try parseExpression()
            return ASTBoxedExpression(expression: expression)
        }
        
        if case .pipe = currentToken.type {
            return try parseLambda()
        }
        
        // Array literal
        // Either primitive or complex
        if [TokenType.openingSquareBrackets, .openingCurlyBrackets].contains(currentToken.type) {
            let isComplexArray = currentToken.type == .openingSquareBrackets
            let expectedClosingToken: TokenType = currentToken.type == .openingSquareBrackets ? .closingSquareBrackets : .closingCurlyBrackets
            next()
            
            let elements = try parseExpressionList()
            
            
            guard currentToken.type == expectedClosingToken else {
                throw ParserError.unexpectedToken(currentToken)
            }
            next()
            
            return ASTArrayLiteral(elements: elements, kind: isComplexArray ? .complex : .primitive)
        }
        
        
        var expression: ASTExpression!
        
        // unary
        if case .minus = currentToken.type {
            next()
            expression = ASTUnaryExpression(expression: try parseExpression(), operator: .negate)
        } else if case .tilde = currentToken.type {
            next()
            expression = ASTUnaryExpression(expression: try parseExpression(), operator: .bitwiseNot)
        }
        
        if case .openingParentheses = currentToken.type {
            next()
            expression = try parseExpression()
            guard case .closingParentheses = currentToken.type else {
                throw ParserError.unexpectedToken(currentToken)
            }
            
            next()
        }
        
        if expression == nil {
            expression =
                (try? parseNumberLiteral())
                ?? (try? parseStringLiteral())
                ?? (try? parseChainedAccess())
                ?? (try? parseIdentifier())
        }
        
        
        if expression == nil {
            throw ParserError.unexpectedToken(currentToken)
        }
        
        if let identifier = expression as? ASTIdentifier {
            
            if case .openingParentheses = currentToken.type {
                // identifier, followed by an opening parentheses -> function call
                next() // jump into the function call
                
                let arguments = try parseExpressionList()
                
                if case .closingParentheses = currentToken.type {
                    next()
                    expression = ASTFunctionCall(functionName: identifier.value, arguments: arguments, unusedReturnValue: false) // TODO is false the right assumption here?
                }
            }
            
            if case .period = currentToken.type {
                // member getter?
                next()
                expression = try parseChainedAccess(firstElement: expression)
            }
            
            if case .openingSquareBrackets = currentToken.type {
                // array subscript getter
                next()
                let offset = try parseExpression()
                guard case .closingSquareBrackets = currentToken.type else {
                    throw ParserError.unexpectedToken(currentToken)
                }
                next()
                expression = ASTArrayGetter(target: identifier, offset: offset)
            }
            
            if case .colon = currentToken.type, case .colon = peek().type {
                // identifier, followed by 2 colons -> static member call
                next()
                next()
                let memberName = try parseIdentifier()
                guard case .openingParentheses = currentToken.type else {
                    throw ParserError.unexpectedToken(currentToken)
                }
                next()
                
                let arguments = try parseExpressionList()
                
                guard case .closingParentheses = currentToken.type else {
                    throw ParserError.unexpectedToken(currentToken)
                }
                next()
                
                expression = ASTFunctionCall(
                    functionName: SymbolMangling.mangleStaticMember(ofType: identifier.value, memberName: memberName.value),
                    arguments: arguments,
                    unusedReturnValue: false) // TODO is false the right assumption here?
            }
        }
        
        if case .openingSquareBrackets = currentToken.type {
            next()
            
            let offset = try parseExpression()
            
            guard case .closingSquareBrackets = currentToken.type else {
                fatalError("ugh")
            }
            next()
            expression = ASTArrayGetter(target: expression, offset: offset)
        }
        
        
        binopParsing: if BinaryOperationTokens.contains(currentToken.type) {
            
            let binaryOperation: ASTBinaryOperation.BinopOperation
            
            switch (currentToken.type, peek().type) {
                
            case (.pipe, .pipe), (.ampersand, .ampersand):
                // got `||` or `&&` while parsing for an expression
                break binopParsing
                
            case (.less, .less):
                binaryOperation = .shl
                next()
                
            case (.greater, .greater):
                binaryOperation = .shr
                next()
                
            case (.plus, _):
                binaryOperation = .add
                
            case (.minus, _):
                binaryOperation = .sub
                
            case (.asterik, _):
                binaryOperation = .mul
                
            case (.forwardSlash, _):
                binaryOperation = .div
                
            case (.percentageSign, _):
                binaryOperation = .mod
                
            case (.ampersand, _):
                binaryOperation = .and
                
            case (.pipe, _):
                binaryOperation = .or
                
            case (.circumflex, _):
                binaryOperation = .xor
                
            default:
                break binopParsing
            }
            
            next()
            
            expression = ASTBinaryOperation(lhs: expression, operation: binaryOperation, rhs: try parseExpression())
        }
        
        if case .as = currentToken.type {
            next()
            expression = ASTTypecast(expression: expression, type: try parseType())
        }
        
        return expression
    }
    
    
    func parseStringLiteral() throws -> ASTStringLiteral {
        guard case .stringLiteral(let value) = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        return ASTStringLiteral(value: value)
    }
    
    
    func parseLambda() throws -> ASTLambda {
        guard case .pipe = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        
        var parameters = [ASTVariableDeclaration]()
        
        while true { // TODO maybe while let identifier = try? parseIdentifier()?
            let identifier = try parseIdentifier()
            if case .colon = currentToken.type {
                next()
                parameters.append(ASTVariableDeclaration(identifier: identifier, type: try parseType()))
            } else {
                parameters.append(ASTVariableDeclaration(identifier: identifier, type: .unresolved))
            }
            
            if case .comma = currentToken.type {
                next()
            }
            
            if case .pipe = currentToken.type {
                break
            }
        }
        
        guard
            case .pipe = currentToken.type,
            case .minus = next().type,
            case .greater = next().type,
            case .openingCurlyBrackets = next().type
            else {
                throw ParserError.unexpectedToken(currentToken)
        }
        
        let body = try parseComposite()
        
        return ASTLambda(signature: .unresolved, parameters: parameters, body: body)
    }
    
    
    
    func parseChainedAccess(firstElement: ASTExpression? = nil) throws -> ASTExpression {
        if firstElement == nil {
            guard case .identifier(_) = currentToken.type, case .period = peek().type, case .identifier(_) = peek(2).type else {
                throw ParserError.unexpectedToken(currentToken)
            }
        }
        
        // in this function, we parse member accesses, which is any expression where we access *some* member of another object
        // a member access can be either of the following:
        // - attribute (`foo.bar`)
        // - member function (`foo.bar()`)
        // ?TODO subscript access?
        
        
        
        
        // TODO what about `foo().bar` where foo is a global function?
        
        var members = [ASTMemberAccess.Kind]()
        if let functionCall = firstElement as? ASTFunctionCall {
            members.append(.initial_functionCall(functionCall))
            //members.append(.functionCall(name: ASTIdentifier(name: functionCall.functionName), arguments: functionCall.arguments, unusedReturnValue: functionCall.unusedReturnValue))
        } else if let identifier = firstElement as? ASTIdentifier {
            //members.append(.attribute(name: identifier))
            members.append(.initial_identifier(identifier))
        } else {
            members.append(.initial_identifier(try parseIdentifier()))
            //members.append(.attribute(name: try parseIdentifier()))
            next()
        }
        
        while let identifier = try? parseIdentifier() {
            
            if case .openingParentheses = currentToken.type {
                next()
                let arguments = try parseExpressionList()
                
                guard case .closingParentheses = currentToken.type else {
                    fatalError()
                }
                next()
                members.append(.functionCall(name: identifier, arguments: arguments, unusedReturnValue: false)) // TODO unusedReturnValue
                if case .period = currentToken.type {
                    next()
                }
            } else if case .period = currentToken.type {
                members.append(.attribute(name: identifier)) // TODO this is a bit redundant
                next()
            } else {
                members.append(.attribute(name: identifier)) // TODO this is a bit redundant
                break
            }
        }
        
        return ASTMemberAccess(members: members)
    }
    
    
    
    func parseExpressionList() throws -> [ASTExpression] {
        var expressions = [ASTExpression]()
        
        while let expression = try? parseExpression() {
            expressions.append(expression)
            
            if case .comma = currentToken.type {
                next()
            }
        }
        
        return expressions
    }
    
    
    func parseIdentifierList() throws -> [ASTIdentifier] {
        var identifiers = [ASTIdentifier]()
        
        while let identifier = try? parseIdentifier() {
            identifiers.append(identifier)
            
            if case .comma = currentToken.type {
                next()
            }
        }
        
        return identifiers
    }
    
    
    
    func parseNumberLiteral() throws -> ASTExpression {
        switch currentToken.type {
        case .integerLiteral(let value):
            next()
            return ASTNumberLiteral(value: value)
        case .doubleLiteral(let value):
            next()
            return ASTNumberLiteral(value: value)
        default:
            throw ParserError.unexpectedToken(currentToken)
        }
    }
    
    
    
    func parseIdentifier() throws -> ASTIdentifier {
        let isBuiltin: Bool
        
        if case .hashtag = currentToken.type {
            next()
            isBuiltin = true
        } else {
            isBuiltin = false
        }
        
        if case .identifier(let name) = currentToken.type {
            next()
            return ASTIdentifier(value: name, isBuiltin: isBuiltin)
        }
        
        throw ParserError.unexpectedToken(currentToken)
    }
    
    
    // This *does not* parse a `type` decl, but instead parses the type of a symbol (like a local variable, function parameter or type attribute)
    func parseType() throws -> ASTType {
        switch currentToken.type {
        case .identifier(let identifier):
            next()
            // TODO what if we introduce other primitive types. refactor into constant declaring all primitive types?
            return ASTType.primitiveTypenames.contains(identifier) ? .primitive(name: identifier) : .complex(name: identifier)
            
        case .fn:
            // a function pointer
            guard
                case .less = next().type,
                case .openingParentheses = next().type
                else {
                    fatalError("expected <( in function signature")
            }
            next()
            
            var parameterTypes = [ASTType]()
            while let type = try? parseType() {
                parameterTypes.append(type)
                
                if case .comma = currentToken.type {
                    next()
                }
                
            }
            
            guard case TokenType.closingParentheses = currentToken.type else {
                fatalError("oh no")
            }
            next()
            
            guard case .colon = currentToken.type else {
                fatalError("expected : after parameters in function signature")
            }
            next()
            
            let returnType = try parseType()
            
            guard case .greater = currentToken.type else {
                fatalError("expected function pointer declaration to end w/ `>`")
            }
            next()
            
            return ASTType.function(returnType: returnType, parameterTypes: parameterTypes)
            
        default:
            break
        }
        
        
        
        throw ParserError.other("not a type")
    }
    
    
    
    
    func parseCondition() throws -> ASTCondition {
        // TODO add support for `!<expr>` conditions
        
        let lhs = try parseExpression()
        
        // the comparison operator can be `<`, `>`, `<=`, `>=`, `==` or `!=`
        
        let comparisonOperator: ASTComparison.Operator
        
        switch (currentToken.type, peek().type) {
        case (TokenType.less, TokenType.equalsSign):
            comparisonOperator = .lessEqual
            next()
            
        case (TokenType.greater, TokenType.equalsSign):
            comparisonOperator = .greaterEqual
            next()
            
        case (TokenType.less, _):
            comparisonOperator = .less
            
        case (TokenType.greater, _):
            comparisonOperator = .greater
            
        case (TokenType.equalsSign, TokenType.equalsSign):
            comparisonOperator = .equal
            next()
            
        case (TokenType.exclamationMark, TokenType.equalsSign):
            comparisonOperator = .notEqual
            next()
            
        default:
            // lhs not followed by a comparison operator -> simple boolean check
            return ASTComparison(lhs: lhs, operator: .equal, rhs: ASTBooleanLiteral(value: true))
        }
        next()
        
        let rhs = try parseExpression()
        return ASTComparison(lhs: lhs, operator: comparisonOperator, rhs: rhs)
        
    }
}
