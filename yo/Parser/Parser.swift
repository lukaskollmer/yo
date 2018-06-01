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
private let BinaryOperationTokens: [TokenType] = [
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
            // The only allowed top level statement are `use`, `type`, `impl` and `fn`
            
            switch currentToken.type {
            case .use:
                return try parseImport()
            case .type:
                return try parseTypeDeclaration()
            case .fn:
                return try parseFunction()
            case .impl:
                return try parseImplementation()
            case .EOF:
                currentPosition += 1 // stop parsing
                return ASTNoop()
            default:
                //fatalError("unexpected top level token: \(currentToken)")
                throw ParserError.unexpectedToken(currentToken)
            }
            
        } else {
            // not a top level statement
            switch currentToken.type {
            case .ret:
                next()
                let expression = try parseExpression()
                guard case .semicolon = currentToken.type else {
                    //fatalError("expected a semicolon")
                    throw ParserError.unexpectedToken(currentToken)
                    //throw ParserError.other("expected a semicolon")
                }
                next()
                return ASTReturnStatement(returnValueExpression: expression)
                
            case .if, .while:
                let initialTokenType = currentToken.type
                
                next()
                var condition = try parseCondition()
                
                if let op = ASTBinaryCondition.Operator(tokenTypes: currentToken.type, peek().type) {
                    next()
                    next() // TODO is that second next correct?
                    let rhs = try parseCondition()
                    condition = ASTBinaryCondition(lhs: condition, operator: op, rhs: rhs)
                }
                
                guard case .openingCurlyBrackets = currentToken.type else {
                    // expected { after condition
                    throw ParserError.unexpectedToken(currentToken)
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
                
                
                
            case .for:
                throw ParserError.other("for loops aren't yet implemented")
                
                
                
            case .val: // variable declaration
                next()
                let name = try parseIdentifier()
                guard case .colon = currentToken.type else {
                    // TODO a colon is only required in some instances:
                    // val foo: int;
                    // val foo = bar(); // not required in this case since we can infer the type from bar's return type
                    // TODO implement that
                    
                    throw ParserError.unexpectedToken(currentToken) // //missing colon after variable name
                }
                next()
                
                let typename = try parseIdentifier()
                
                // check whether we're creating an array
                if case .openingSquareBrackets = currentToken.type, case .closingSquareBrackets = peek().type {
                    // array declaration
                    next()
                    next()
                    //typename = ASTIdentifier(name: typename.name + "[]") // this isn't necessary (yet?)
                }
                
                // a variable declaration's type can either be followed by a semicolon (`val x: Foo;`)
                // or by an equals sign, followed by an expression and a semicolon
                if case .semicolon = currentToken.type {
                    next()
                    return ASTVariableDeclaration(identifier: name, typename: typename)
                } else {
                    guard case .equalsSign = currentToken.type else {
                        throw ParserError.unexpectedToken(currentToken)
                    }
                    next()
                    
                    let assignedValue = try parseExpression()
                    guard case .semicolon = currentToken.type else {
                        // assignment should end w/ semicolon
                        throw ParserError.unexpectedToken(currentToken)
                    }
                    next()
                    
                    return ASTComposite(statements: [
                        ASTVariableDeclaration(identifier: name, typename: typename),
                        ASTAssignment(target: name, value: assignedValue)
                    ])
                }
                
                
                
            case .identifier(_):
                // a statement starting e/ an identifier can be:
                // - a simple assignment
                // - a subscript assignment // TODO
                // - a function call with discarded return value // TODO
                //let expression = try parseExpression()
                let identifier = try parseIdentifier()
                
                
                if BinaryOperationTokens.contains(currentToken.type) {
                    // in-place binary operation
                    // ie something like `foo += 1;`
                    
                    // possible operations are: `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`
                    
                    let binaryOperation: ASTBinaryOperation.BinopOperation
                    
                    switch (currentToken.type, peek().type, peek(2).type) {
                    case (.greater, .greater, .equalsSign):
                        binaryOperation = .shr
                        next()
                        
                    case (.less, .less, .equalsSign):
                        binaryOperation = .shl
                        next()
                        
                    case (.plus, .equalsSign, _):
                        binaryOperation = .add
                        
                    case (.minus, .equalsSign, _):
                        binaryOperation = .sub
                        
                    case (.asterik, .equalsSign, _):
                        binaryOperation = .add
                        
                    case (.forwardSlash, .equalsSign, _):
                        binaryOperation = .add
                        
                    case (.percentageSign, .equalsSign, _):
                        binaryOperation = .add
                        
                    case (.ampersand, .equalsSign, _):
                        binaryOperation = .add
                        
                    case (.pipe, .equalsSign, _):
                        binaryOperation = .add
                        
                    case (.circumflex, .equalsSign, _):
                        binaryOperation = .add
                    default:
                        fatalError("TODO")
                    }
                    next()
                    next()
                    
                    let rhs = try parseExpression()
                    
                    guard case .semicolon = currentToken.type else {
                        throw ParserError.unexpectedToken(currentToken)
                    }
                    next()
                    
                    return ASTAssignment(target: identifier, value: ASTBinaryOperation(lhs: identifier, operation: binaryOperation, rhs: rhs))
                }
                
                
                if case .equalsSign = currentToken.type {
                    next()
                    let assignedValue = try parseExpression()
                    guard case .semicolon = currentToken.type else {
                        // assignment should end w/ semicolon
                        throw ParserError.unexpectedToken(currentToken)
                    }
                    
                    next()
                    return ASTAssignment(target: identifier, value: assignedValue)
                }
                
                if case .openingParentheses = currentToken.type {
                    // global fununction call w/ discarded return value
                    next()
                    let arguments = try parseExpressionList()
                    guard case .closingParentheses = currentToken.type, case .semicolon = next().type else {
                        throw ParserError.unexpectedToken(currentToken)
                    }
                    next() // skip the semicolon
                    
                    return ASTFunctionCall(functionName: identifier.name, arguments: arguments, unusedReturnValue: true)
                }
                
                if case .openingSquareBrackets = currentToken.type {
                    // array subscript assignment
                    next()
                    let offset = try parseExpression()
                    
                    guard case .closingSquareBrackets = currentToken.type, case .equalsSign = next().type else {
                        throw ParserError.unexpectedToken(currentToken)
                    }
                    next() // skip the equals sign
                    
                    let assignedValue = try parseExpression()
                    guard case .semicolon = currentToken.type else {
                        throw ParserError.unexpectedToken(currentToken)
                    }
                    next()
                    
                    return ASTArraySetter(target: identifier, offset: offset, value: assignedValue)
                }
                
                
                if case .period = currentToken.type, case .identifier(_) = next().type {
                    // <identifier>.<identifier>
                    // either:
                    // - member setter (`foo.bar = x;`)
                    // - member function call (`foo.bar();`)
                    // - member subscript assignment (`foo.bar[idx] = x;`)
                    
                    let memberName = try parseIdentifier()
                    
                    if case .openingParentheses = currentToken.type {
                        // member function call w/ unused return value
                        // ie `foo.bar();`
                        next() // skip the opening parentheses
                        let arguments = try parseExpressionList()
                        guard case .closingParentheses = currentToken.type, case .semicolon = next().type else {
                            // expected ) after arguments
                            throw ParserError.unexpectedToken(currentToken)
                        }
                        next() // skip the semicolon
                        return ASTTypeMemberFunctionCall(
                            target: identifier,
                            functionName: memberName,
                            arguments: arguments,
                            unusedReturnValue: true
                        )
                    }
                    
                    if case .equalsSign = currentToken.type {
                        // member assignment
                        next()
                        let assignedValue = try parseExpression()
                        guard case .semicolon = currentToken.type else {
                            // expected ; after member assignment
                            throw ParserError.unexpectedToken(currentToken)
                        }
                        next()
                        
                        return ASTTypeMemberSetter(target: identifier, memberName: memberName, newValue: assignedValue)
                    }
                    
                    if case .openingSquareBrackets = currentToken.type {
                        next() // skip the opening [
                        let offset = try parseExpression()
                        guard case .closingSquareBrackets = currentToken.type, case .equalsSign = next().type else {
                            // expected `]` and `=` after array offset
                            throw ParserError.unexpectedToken(currentToken)
                        }
                        next() // skip the equalsSign
                        
                        let assignedValue = try parseExpression()
                        
                        guard case .semicolon = currentToken.type else {
                            // expected ; after array subscript assignment
                            throw ParserError.unexpectedToken(currentToken)
                        }
                        next()
                        
                        return ASTArraySetter(
                            target: ASTTypeMemberGetter(target: identifier, memberName: memberName),
                            offset: offset,
                            value: assignedValue
                        )
                    }
                }
                
                
                if case .colon = currentToken.type, case .colon = next().type {
                    // static member function w/ discarded return value
                    next()
                    let memberName = try parseIdentifier()
                    guard case .openingParentheses = currentToken.type else {
                        throw ParserError.unexpectedToken(currentToken)
                    }
                    next()
                    let arguments = try parseExpressionList()
                    guard case .closingParentheses = currentToken.type, case .semicolon = next().type else {
                        throw ParserError.unexpectedToken(currentToken)
                    }
                    next()
                    
                    return ASTFunctionCall(
                        functionName: SymbolMangling.mangleStaticMember(ofType: identifier.name, memberName: memberName.name),
                        arguments: arguments,
                        unusedReturnValue: true
                    )
                }
                
                fatalError()
                
            
            case .use, .type, .impl, .fn:
                throw ParserError.other("\(currentToken.type) may only be used as a top level statement")
                
            default: fatalError("unhandled token \(currentToken)")
            }
        }
        
        fatalError()
    }
    
    
    
    
    func parseImport() throws -> ASTStatement {
        guard case .use = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        
        guard case .stringLiteral(let moduleName) = next().type, case .semicolon = next().type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        
        return ASTImportStatement(moduleName: moduleName, isInternal: true) // TODO somwhow decide whether the module is internal or not
    }
    
    
    
    func parseTypeDeclaration() throws -> ASTTypeDeclaration {
        guard case .type = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        
        let name = try parseIdentifier()
        guard case .openingParentheses = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        
        let attributes = try parseParameterList()
        guard
            case .closingParentheses = currentToken.type,
            case .semicolon = next().type
        else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        
        return ASTTypeDeclaration(name: name, attributes: attributes)
    }
    
    
    func parseParameterList() throws -> [ASTVariableDeclaration] {
        var parameters = [ASTVariableDeclaration]()
        
        while let parameter = try? parseIdentifier() {
            guard case .colon = currentToken.type else {
                throw ParserError.unexpectedToken(currentToken)
            }
            next()
            let typename = try parseIdentifier()
            if case .openingSquareBrackets = currentToken.type, case .closingSquareBrackets = next().type {
                next()
            }
            parameters.append(ASTVariableDeclaration(identifier: parameter, typename: typename))
            if case .comma = currentToken.type {
                next()
            }
        }
        return parameters
    }
    
    
    func parseImplementation() throws -> ASTTypeImplementation {
        guard case .impl = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        
        let typename = try parseIdentifier()
        
        guard case .openingCurlyBrackets = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        
        var functions = [ASTFunctionDeclaration]()
        var functionKind: ASTFunctionDeclaration.Kind!
        
        let updateFunctionKind = {
            if case .static = self.currentToken.type {
                functionKind = .staticImpl(typename.name)
                self.next()
            } else {
                functionKind = .impl(typename.name)
            }
        }
        
        updateFunctionKind()
        
        
        while let function = try? parseFunction(kind: functionKind) {
            functions.append(function)
            updateFunctionKind()
            if case .closingCurlyBrackets = currentToken.type {
                break
            }
        }
        
        guard case .closingCurlyBrackets = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        
        return ASTTypeImplementation(typename: typename, functions: functions)
    }
    
    
    func parseFunction(kind: ASTFunctionDeclaration.Kind = .global) throws -> ASTFunctionDeclaration {
        guard
            case .fn = currentToken.type,
            case .identifier(let functionName) = next().type,
            case .openingParentheses = next().type
        else {
            throw ParserError.unexpectedToken(currentToken)
        }
        
        next() // step into the function signature
        let parameters = try parseParameterList()
        
        guard
            case .closingParentheses = currentToken.type,
            case .colon = next().type
        else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        let returnType = try parseIdentifier()
        
        let functionBody = try parseComposite().statements
        
        return ASTFunctionDeclaration(
            name: ASTIdentifier(name: functionName),
            parameters: parameters,
            returnType: returnType,
            body: functionBody,
            kind: kind
        )
    }
    
    
    func parseComposite() throws -> ASTComposite {
        guard case .openingCurlyBrackets = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        
        if case .closingCurlyBrackets = next().type {
            // check whether the composite is empty
            next()
            return ASTComposite(statements: [])
        }
        
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
                throw ParserError.unexpectedToken(currentToken)
            }
            
            next()
            return expression
        }
        
        if case .openingSquareBrackets = currentToken.type {
            next()
            
            let elements = try parseExpressionList()
            
            guard case .closingSquareBrackets = currentToken.type else {
                throw ParserError.unexpectedToken(currentToken)
            }
            next()
            
            return ASTArrayLiteral(elements: elements)
        }
        
        guard var expression: ASTExpression =
            (try? parseNumberLiteral())
            ?? (try? parseStringLiteral())
            ?? (try? parseMemberAccess())
            ?? (try? parseIdentifier())
        else {
            throw ParserError.unexpectedToken(currentToken)
        }
        
        
        if let identifier = expression as? ASTIdentifier {
            
            if case .openingParentheses = currentToken.type {
                // identifier, followed by an opening parentheses -> function call
                next() // jump into the function call
                
                let arguments = try parseExpressionList()
                
                if case .closingParentheses = currentToken.type {
                    next()
                    return ASTFunctionCall(functionName: identifier.name, arguments: arguments, unusedReturnValue: false) // TODO is false the right assumprion here?
                }
                
                // TODO?
                fatalError("aaargh")
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
                //return ASTArrayGetter(target: identifier, offset: offset)
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
                
                expression =  ASTFunctionCall(
                    functionName: SymbolMangling.mangleStaticMember(ofType: identifier.name, memberName: memberName.name),
                    arguments: arguments,
                    unusedReturnValue: false) // TODO is false the right assumption here?
            }
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
            
            return ASTBinaryOperation(lhs: expression, operation: binaryOperation, rhs: try parseExpression())
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
    
    
    // parse a member access
    // this returns either ASTTypeMemberAccess (when accessing a type's attribute
    // or ASTTypeMemberFunctionCall (ie when `foo.bar()`)
    // or ASTArrayGetter (ie when `foo.bar[expr]`)
    func parseMemberAccess() throws -> ASTExpression {
        guard case .identifier(_) = currentToken.type, case .period = peek().type, case .identifier(_) = peek(2).type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        
        let identifier = try parseIdentifier()
        
        next() // skip the period
        
        let memberName = try parseIdentifier()

        
        if case .openingParentheses = currentToken.type {
            next()
            let arguments = try parseExpressionList()
            guard case .closingParentheses = currentToken.type else {
                throw ParserError.unexpectedToken(currentToken)
            }
            next()
            
            return ASTTypeMemberFunctionCall(target: identifier, functionName: memberName, arguments: arguments, unusedReturnValue: false) // TODO FIXME
        }
        
        
        if case .openingSquareBrackets = currentToken.type {
            next()
            let offset = try parseExpression()
            guard case .closingSquareBrackets = currentToken.type else {
                throw ParserError.unexpectedToken(currentToken)
            }
            next()
            return ASTArrayGetter(
                target: ASTTypeMemberGetter(target: identifier, memberName: memberName),
                offset: offset
            )
        }
        
        // member variable access
        return ASTTypeMemberGetter(target: identifier, memberName: memberName)
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
    
    
    
    
    func parseNumberLiteral() throws -> ASTNumberLiteral {
        if case .numberLiteral(let value) = currentToken.type {
            next()
            return ASTNumberLiteral(value: value)
        }
        
        throw ParserError.unexpectedToken(currentToken)
    }
    
    
    
    func parseIdentifier() throws -> ASTIdentifier {
        if case .identifier(let name) = currentToken.type {
            next()
            return ASTIdentifier(name: name)
        }
        
        throw ParserError.unexpectedToken(currentToken)
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
            throw ParserError.other("invalid comparison operator")
        }
        next()
        
        let rhs = try parseExpression()
        return ASTComparison(lhs: lhs, operator: comparisonOperator, rhs: rhs)
        
    }
}


