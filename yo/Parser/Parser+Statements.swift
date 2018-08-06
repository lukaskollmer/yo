//
//  Parser+Statements.swift
//  yo
//
//  Created by Lukas Kollmer on 06.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


extension Parser {
    // MARK: Statements
    
    func parseStatement(isTopLevel: Bool = false) throws -> ASTStatement {
        
        if isTopLevel {
            // Currently parsing a top level expression
            
            switch currentToken.type {
            case .use:
                return try parseImport()
            case .static:
                return try parseStaticVariableDeclaration()
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
            case .enum:
                return try parseEnumDeclaration()
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
                return try parseReturnStatement()
                
            case .__asm:
                return try parseAsmStatement()
                
            case .if, .while:
                return try parseConditionalStatement()
                
            case .for:
                return try parseForLoop()
                
            case .break, .continue:
                let stmt: ASTStatement = currentToken.type == .break ? ASTBreakStatement() : ASTContinueStatement()
                
                guard case .semicolon = next().type else {
                    fatalError("expected ; following break statement")
                }
                next()
                return stmt
                
            case .openingCurlyBrackets:
                return try parseComposite()
                
            case .val:
                return try parseVariableDeclaration()
                
                
                // TODO refactor this into its own thing
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
                    
                    return ASTFunctionCall(functionName: identifier.value, arguments: arguments, unusedReturnValue: true)
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
                        functionName: SymbolMangling.mangleStaticMember(ofType: identifier.value, memberName: memberName.value),
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
    
    
    
    // These are sorted roughly in the order in which they might appear in a program
    
    
    func parseImport() throws -> ASTStatement {
        guard case .use = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        
        guard case .stringLiteral(let moduleName) = next().type, case .semicolon = next().type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        
        return ASTImportStatement(moduleName: moduleName)
    }
    
    
    func parseStaticVariableDeclaration() throws -> ASTStaticVariableDeclaration {
        guard case .static = currentToken.type else {
            fatalError()
        }
        next()
        
        let identifier = try parseIdentifier()
        guard case .colon = currentToken.type else {
            fatalError()
        }
        next()
        
        let type = try parseType()
        
        if case .semicolon = currentToken.type {
            // static variable w/out an initial value
            next()
            return ASTStaticVariableDeclaration(identifier: identifier, type: type, initialValue: nil)
            
        } else if case .equalsSign = currentToken.type {
            // static variable w/ an initial value
            next()
            let initialValue = try parseExpression()
            guard case .semicolon = currentToken.type else { fatalError() }
            next()
            
            return ASTStaticVariableDeclaration(identifier: identifier, type: type, initialValue: initialValue)
        }
        
        fatalError("should not reach here")
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
                    return .impl(typename.value)
                case .staticImpl(_):
                    return .staticImpl(typename.value)
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
        
        let annotations = self.annotations.allElements
        self.annotations = []
        
        return ASTFunctionDeclaration(
            name: ASTIdentifier(value: functionName),
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
        
        while let key = try? parseIdentifier().value {
            var value: ASTAnnotation.Element.Value?
            
            if case .equalsSign = currentToken.type {
                next()
                
                let expr = try parseExpression()
                
                if let id = expr as? ASTIdentifier {
                    switch id.value {
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
        
        // we have to capture them here, because `parseFunctionList` invokes `parseFunction`, which resets `self.annotations` to []
        let annotations = self.annotations.allElements
        
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
        
        return ASTProtocolDeclaration(name: ASTIdentifier(value: protocolName), functions: functions, annotations: annotations)
    }
    
    
    
    
    
    func parseVariableDeclaration() throws -> ASTStatement {
        // TODO combine this w/ parseStaticVariableDeclaration, add a `initialValue` field to ASTVariable, move away from the composite thing
        // Also: remove ASTStaticVariableDeclaration and add a 'isStatic' ivar to ASTVariableDeclaration // TODO also isConst if this ever becomes a thing
        guard case .val = currentToken.type else {
            fatalError()
        }
        
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
    }
    
    
    
    
    
    func parseConditionalStatement() throws -> ASTConditionalStatement {
        // TODO add support for else if statements? (or does that already work unintentionally?)
        guard [TokenType.if, .while].contains(currentToken.type) else {
            fatalError()
        }
        
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
        var elseBranch: ASTStatement?
        
        if case .else = currentToken.type {
            next()
            
            if case .if = currentToken.type {
                elseBranch = try parseConditionalStatement()
            } else {
                elseBranch = try parseComposite()
            }
        }
        return ASTConditionalStatement(condition: condition, body: body, kind: .if(elseBranch: elseBranch))
    }
    
    
    
    
    func parseForLoop() throws -> ASTStatement {
        // TODO make ASTForLoop its own thing and implement stepping/etc/whatever in the compiler
        // TODO this needs a full rewrite!
        
        guard case .for = currentToken.type else {
            fatalError()
        }
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
    }
    
    
    
    func parseReturnStatement() throws -> ASTReturnStatement {
        guard case .ret = currentToken.type else {
            fatalError()
        }
        
        if case .semicolon = next().type {
            // A return statement that doesn't return any particular expression
            // TODO somehow make sure this type of return statement is only allowed in functions/lambdas returning void
            next()
            return ASTReturnStatement(expression: ASTNumberLiteral(value: 0))
        }
        
        let expression = try parseExpression()
        guard case .semicolon = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        return ASTReturnStatement(expression: expression)
    }
    
    
    func parseAsmStatement() throws -> ASTRawWIPInstruction {
        // TODO this needs more testing!!!
        next()
        
        guard case .openingParentheses = currentToken.type else {
            throw ParserError.unexpectedToken(currentToken)
        }
        next()
        
        let rawOperationName = try parseIdentifier().value
        guard let operation = Operation(name: rawOperationName) else {
            throw ParserError.other("unknown operation in __asm literal: \(rawOperationName)")
        }
        
        if case .comma = currentToken.type {
            next()
            // parse immediate
            guard let immediate = (try? parseNumberLiteral()) ?? (try? parseIdentifier()) else {
                throw ParserError.other("unable to parse __asm literal")
            }
            
            next()  // skip the closing parentheses
            next()  // skip the semicolon
            
            if let immediate = immediate as? ASTNumberLiteral {
                return ASTRawWIPInstruction(instruction: .operation(operation, immediate.value))
            } else if let immediate = immediate as? ASTIdentifier {
                return ASTRawWIPInstruction(instruction: .unresolved(operation, immediate.value))
            }
            fatalError("should not reach here")
        }
        
        fatalError("TODO seems like there's a return statement missing here")
    }
    
    
    
    func parseEnumDeclaration() throws -> ASTEnumDeclaration {
        guard
            case .enum = currentToken.type,
            case _ = next(),
            let enumName = try? parseIdentifier(),
            case .openingCurlyBrackets = currentToken.type
        else {
            fatalError()
        }
        next()
        
        guard
            let cases = try parseExpressionList() as? [ASTIdentifier],
            case .closingCurlyBrackets = currentToken.type
        else {
            fatalError()
        }
        next()
        
        return ASTEnumDeclaration(name: enumName, cases: cases)
    }
    
}
