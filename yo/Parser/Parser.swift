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
    
    private var annotations = [ASTAnnotation]()
    
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
            
            switch currentToken.type {
            case .use:
                return try parseImport()
            case .type, .struct:
                return try parseTypeDeclaration()
            case .fn:
                return try parseFunction()
            case .impl:
                return try parseImplementation()
            case .protocol:
                return try parseProtocolDeclaration()
            case .hashtag:
                annotations.append(try parseAnnotation())
                return ASTNoop()
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
                return ASTReturnStatement(expression: expression)
                
            case .__asm:
                next()
                
                guard case .openingParentheses = currentToken.type else {
                    throw ParserError.unexpectedToken(currentToken)
                }
                next()
                
                let rawOperationName = try parseIdentifier().name
                print(rawOperationName)
                guard let operation = Operation(name: rawOperationName) else {
                    print("ugh")
                    throw ParserError.other("unknown operation in __asm literal: \(rawOperationName)")
                }
                
                if case .comma = currentToken.type {
                    next()
                    // parse immediate
                    guard let immediate = (try? parseNumberLiteral()) ?? (try? parseIdentifier()) else {
                        throw ParserError.other("unable to parse __asm literal")
                    }
                    print(immediate)
                    
                    next()  // skip the closing parentheses
                    next()  // skip the semicolon
                    
                    if let immediate = immediate as? ASTNumberLiteral {
                        return ASTRawWIPInstruction(instruction: .operation(operation, immediate.value))
                    } else if let immediate = immediate as? ASTIdentifier {
                        return ASTRawWIPInstruction(instruction: .unresolved(operation, immediate.name))
                    }
                    fatalError("should not reach here")
                }
                
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
                next()
                
                let indexIdentifier = try parseIdentifier()
                
                guard case .in = currentToken.type else {
                    fatalError("invalid range expression")
                }
                next()
                
                let start = try parseExpression()
                
                guard
                    case .period = currentToken.type,
                    case .period = next().type
                else {
                      fatalError("invalid range expression")
                }
                next()
                
                
                let rangeType: ASTComparison.Operator
                
                switch currentToken.type {
                case .period:
                    rangeType = .lessEqual
                case .less:
                    rangeType = .less
                default: fatalError("invalid token in range expression")
                }
                next()
                
                let end = try parseExpression()
                
                guard case .openingCurlyBrackets = currentToken.type else {
                    fatalError("{ required in for loop")
                }
                
                let body = try parseComposite()
                
                let forLoop: ASTComposite = [
                    ASTVariableDeclaration(identifier: indexIdentifier, type: .primitive(name: "int")),
                    ASTAssignment(target: indexIdentifier, value: start),
                    
                    ASTConditionalStatement(
                        condition: ASTComparison(
                            lhs: indexIdentifier,
                            operator: rangeType,
                            rhs: end
                        ),
                        body: body.appending(statements: [
                            ASTAssignment(
                                target: indexIdentifier,
                                value: ASTBinaryOperation(lhs: indexIdentifier, operation: .add, rhs: ASTNumberLiteral(value: 1))
                            )
                        ]),
                        kind: .while
                    )
                ]
                
                return forLoop
                
                
            case .break, .continue:
                let stmt: ASTStatement = currentToken.type == .break ? ASTBreakStatement() : ASTContinueStatement()
                
                guard case .semicolon = next().type else {
                    fatalError("expected ; following break statement")
                }
                next()
                
                return stmt
                
                
            case .openingCurlyBrackets:
                return try parseComposite()
                
                
                
            case .val: // variable declaration
                next()
                let name = try parseIdentifier()
                let type: ASTType
                
                if case .colon = currentToken.type {
                    next()
                    type = try parseType()
                } else {
                    type = .unresolved
                }
                
                // a variable declaration's type can either be followed by a semicolon (`val x: Foo;`)
                // or by an equals sign, followed by an expression and a semicolon
                if case .semicolon = currentToken.type {
                    next()
                    return ASTVariableDeclaration(identifier: name, type: type)
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
                    
                    return ASTComposite(
                        statements: [
                            ASTVariableDeclaration(identifier: name, type: type),
                            ASTAssignment(target: name, value: assignedValue)
                        ],
                        introducesNewScope: false
                    )
                }
                
                
                
            case .identifier(_):
                // a statement starting e/ an identifier can be:
                // - a simple assignment
                // - a subscript assignment // TODO
                // - a function call with discarded return value // TODO
                //let expression = try parseExpression()
                
                let expression: ASTExpression! = (try? parseChainedAccess()) ?? (try? parseIdentifier())
                
                if case .semicolon = currentToken.type {
                    guard let chainedAccess = expression as? ASTMemberAccess else {
                        fatalError("should not reach here unless chained member access")
                    }
                    next()
                    
                    if case ASTMemberAccess.Kind.functionCall(let name, let arguments, _)? = chainedAccess.members.last {
                        // function call w/ discarded return value
                        return ASTMemberAccess(
                            members: Array(chainedAccess.members.prefix(upTo: chainedAccess.members.count - 1) + [ASTMemberAccess.Kind.functionCall(name: name, arguments: arguments, unusedReturnValue: true)])
                        )
                    }
                    return chainedAccess
                }
                
                
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
                        binaryOperation = .mul
                        
                    case (.forwardSlash, .equalsSign, _):
                        binaryOperation = .div
                        
                    case (.percentageSign, .equalsSign, _):
                        binaryOperation = .mod
                        
                    case (.ampersand, .equalsSign, _):
                        binaryOperation = .and
                        
                    case (.pipe, .equalsSign, _):
                        binaryOperation = .or
                        
                    case (.circumflex, .equalsSign, _):
                        binaryOperation = .xor
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
                    
                    // TODO what if `expression` is a chain containing a member call. we should only call that once, right?
                    // what about introducing a temporary variable and operating on that?
                    // ie:
                    // `foo.getParent().value += 1`
                    // becomes
                    // ```
                    // val _temp: Parent = foo.getParent();
                    // _temp.value = _temp.value + 1;
                    // ```
                    return ASTAssignment(
                        target: expression,
                        value: ASTBinaryOperation(lhs: expression, operation: binaryOperation, rhs: rhs)
                    )
                }
                
                
                if case .equalsSign = currentToken.type {
                    next()
                    let assignedValue = try parseExpression()
                    guard case .semicolon = currentToken.type else {
                        // assignment should end w/ semicolon
                        throw ParserError.unexpectedToken(currentToken)
                    }
                    
                    next()
                    return ASTAssignment(target: expression, value: assignedValue)
                }
                
                if case .openingParentheses = currentToken.type {
                    // function call w/ discarded return value
                    
                    guard let identifier = expression as? ASTIdentifier else {
                        fatalError("unable to get function name")
                    }
                    
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
                    
                    return ASTArraySetter(target: expression, offset: offset, value: assignedValue)
                }
                
                
                if let identifier = expression as? ASTIdentifier, case .colon = currentToken.type, case .colon = next().type {
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
                
                fatalError() // TODO is this still necessary?
                
            
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
        guard [TokenType.type, .struct].contains(currentToken.type) else {
            throw ParserError.unexpectedToken(currentToken)
        }
        
        let isStruct = currentToken.type == .struct
        next()
        
        let name = try parseIdentifier()
        
        let protocols: [ASTIdentifier]
        
        if case .colon = currentToken.type {
            next()
            protocols = try parseIdentifierList()
        } else {
            protocols = []
        }
        
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
        
        return ASTTypeDeclaration(
            name: name,
            attributes: attributes,
            protocols: protocols,
            annotations: isStruct ? ["struct"] : []
        )
    }
    
    
    func parseParameterList() throws -> [ASTVariableDeclaration] {
        var parameters = [ASTVariableDeclaration]()
        
        while let parameter = try? parseIdentifier() {
            guard case .colon = currentToken.type else {
                throw ParserError.unexpectedToken(currentToken)
            }
            next()
            
            parameters.append(ASTVariableDeclaration(identifier: parameter, type: try parseType()))
            
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
        
        let functions = try parseFunctionList()
        for function in functions {
            function.kind = {
                switch function.kind {
                case .impl(_):
                    return .impl(typename.name)
                case .staticImpl(_):
                    return .staticImpl(typename.name)
                default:
                    fatalError("unexpected non-impl function kind")
                }
            }()
        }
        
        guard case .closingCurlyBrackets = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        
        return ASTTypeImplementation(typename: typename, functions: functions)
    }
    
    
    func parseFunction(kind: ASTFunctionDeclaration.Kind = .global) throws -> ASTFunctionDeclaration {
        
        while let annotation = try? parseAnnotation() {
            self.annotations.append(annotation)
        }
        
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
        let returnType = try parseType()
        
        let functionBody = try parseComposite()
        
        let annotations = self.annotations.reduce(into: []) { $0.append(contentsOf: $1.elements) }
        self.annotations = []
        
        return ASTFunctionDeclaration(
            name: ASTIdentifier(name: functionName),
            parameters: parameters,
            returnType: returnType,
            kind: kind,
            annotations: annotations,
            body: functionBody
        )
    }
    
    
    func parseFunctionList() throws -> [ASTFunctionDeclaration] {
        var functions = [ASTFunctionDeclaration]()
        var functionKind: ASTFunctionDeclaration.Kind!
        
        let updateFunctionKind = {
            if case .hashtag = self.currentToken.type {
                while let annotation = try? self.parseAnnotation() {
                    self.annotations.append(annotation)
                }
            }
            
            if case .static = self.currentToken.type {
                functionKind = .staticImpl("")
                self.next()
            } else {
                functionKind = .impl("")
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
        
        return functions
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
    
    
    func parseAnnotation() throws -> ASTAnnotation {
        guard case .hashtag = currentToken.type, case .openingSquareBrackets = peek().type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        next()
        
        var elements = [ASTAnnotation.Element]()
        
        while let key = try? parseIdentifier().name {
            var value: ASTAnnotation.Element.Value?
            
            if case .equalsSign = currentToken.type {
                next()
                
                let expr = try parseExpression()
                
                if let id = expr as? ASTIdentifier {
                    switch id.name {
                    case "true":  value = .bool(true)
                    case "false": value = .bool(false)
                    default: fatalError("TODO?")
                    }
                
                } else if let str = expr as? ASTStringLiteral {
                    value = .string(str.value)
                
                } else if let num = expr as? ASTNumberLiteral {
                    value = .number(num.value)
                
                } else {
                    fatalError("unsupported data type in annotation: \(type(of: expr))")
                }
            }
            
            if case .comma = currentToken.type {
                next()
            }
            
            if case .closingSquareBrackets = currentToken.type {
                next()
            }
            
            elements.append(ASTAnnotation.Element(key: key, value: value ?? .bool(true)))
            
            // TODO how does this handle trailing commas? `#[format_function,]`
        }
        
        return ASTAnnotation(elements: elements)
    }
    
    
    
    
    func parseProtocolDeclaration() throws -> ASTProtocolDeclaration {
        guard
            case .protocol = currentToken.type,
            case .identifier(let protocolName) = next().type,
            case .openingCurlyBrackets = next().type else {
                throw ParserError.unexpectedToken(currentToken)
        }
        next()
        
        let functions = try parseFunctionList()
        for function in functions {
            function.kind = {
                switch function.kind {
                case .impl(_):
                    return .impl(protocolName)
                case .staticImpl(_):
                    return .staticImpl(protocolName)
                default:
                    fatalError("unexpected function kind in protocol")
                }
            }()
        }
        
        guard case .closingCurlyBrackets = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        
        return ASTProtocolDeclaration(name: ASTIdentifier(name: protocolName), functions: functions)
    }
    
    
    
    
    // MARK: Expressions
    
    
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
        
        if case .openingSquareBrackets = currentToken.type {
            next()
            
            let elements = try parseExpressionList()
            
            guard case .closingSquareBrackets = currentToken.type else {
                throw ParserError.unexpectedToken(currentToken)
            }
            next()
            
            return ASTArrayLiteral(elements: elements)
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
                    expression = ASTFunctionCall(functionName: identifier.name, arguments: arguments, unusedReturnValue: false) // TODO is false the right assumption here?
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
                    functionName: SymbolMangling.mangleStaticMember(ofType: identifier.name, memberName: memberName.name),
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
        if case .identifier(let name) = currentToken.type {
            next()
            return ASTIdentifier(name: name)
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
