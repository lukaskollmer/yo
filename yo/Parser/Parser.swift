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
                
                // check whether we're creating an array
                if case .openingSquareBrackets = currentToken.type, case .closingSquareBrackets = peek().type, case .semicolon = peek(2).type {
                    // array declaration
                    next()
                    next()
                    //typename = ASTIdentifier(name: typename.name + "[]") // this isn't necessary (yet?)
                }
                if case .semicolon = currentToken.type {
                    next()
                    return ASTVariableDeclaration(identifier: name, typename: typename)
                } else {
                    guard case .equalsSign = currentToken.type else {
                        fatalError("the fuck are you doing?")
                    }
                    next()
                    
                    let assignedValue = try parseExpression()
                    guard case .semicolon = currentToken.type else {
                        fatalError("assignment should end w/ semicolon")
                    }
                    next()
                    
                    return ASTComposite(statements: [
                        ASTVariableDeclaration(identifier: name, typename: typename),
                        ASTAssignment(target: name, value: assignedValue)
                    ])
                    fatalError("assignments in declaration not yet implemented")
                }
                
                
                
            case .identifier(let name):
                // a statement starting e/ an identifier can be:
                // - a simple assignment
                // - a subscript assignment // TODO
                // - a function call with discarded return value // TODO
                //let expression = try parseExpression()
                let identifier = try parseIdentifier()
                
                
                if BinopTokenTypes.contains(currentToken.type),
                    case .equalsSign = peek().type,
                    let binopOperator = ASTBinop.Operator(tokenType: currentToken.type) {
                    next() // skip the binop
                    next() // skip the equals sign
                    
                    let rhs = try parseExpression()
                    
                    guard case .semicolon = currentToken.type else {
                        fatalError("ugh")
                    }
                    next()
                    
                    return ASTAssignment(target: identifier, value: ASTBinop(lhs: identifier, operator: binopOperator, rhs: rhs))
                }
                
                if case .equalsSign = currentToken.type {
                    next()
                    let assignedValue = try parseExpression()
                    guard case .semicolon = currentToken.type else {
                        fatalError("assignment should end w/ semicolon")
                    }
                    
                    next()
                    return ASTAssignment(target: identifier, value: assignedValue)
                }
                
                if case .openingParentheses = currentToken.type {
                    // global fununction call w/ discarded return value
                    next()
                    let arguments = try parseExpressionList()
                    guard case .closingParentheses = currentToken.type, case .semicolon = next().type else {
                        fatalError("TODO come up with an error message")
                    }
                    next() // skip the semicolon
                    
                    return ASTFunctionCall(functionName: identifier.name, arguments: arguments, unusedReturnValue: true)
                }
                
                if case .openingSquareBrackets = currentToken.type {
                    // array subscript assignment
                    next()
                    let offset = try parseExpression()
                    
                    guard case .closingSquareBrackets = currentToken.type, case .equalsSign = next().type else {
                        fatalError()
                    }
                    next() // skip the equals sign
                    
                    let assignedValue = try parseExpression()
                    guard case .semicolon = currentToken.type else {
                        fatalError("assignment should end w/ semicolon")
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
                            fatalError("expected ) after arguments")
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
                            fatalError("expected ; after member assignment")
                        }
                        next()
                        
                        return ASTTypeMemberSetter(target: identifier, memberName: memberName, newValue: assignedValue)
                    }
                    
                    if case .openingSquareBrackets = currentToken.type {
                        next() // skip the opening [
                        let offset = try parseExpression()
                        guard case .closingSquareBrackets = currentToken.type, case .equalsSign = next().type else {
                            fatalError("expected `]` and `=` after array offset")
                        }
                        next() // skip the equalsSign
                        
                        let assignedValue = try parseExpression()
                        
                        guard case .semicolon = currentToken.type else {
                            fatalError("expected ; after array subscript assignment")
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
                        fatalError("expected ( in static member call")
                    }
                    next()
                    let arguments = try parseExpressionList()
                    guard case .closingParentheses = currentToken.type, case .semicolon = next().type else {
                        fatalError("expected ) after static member call")
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
                fatalError("\(currentToken.type) may only be used as a top level statement")
                
            default: fatalError("unhandled token \(currentToken)")
            }
        }
        
        fatalError()
    }
    
    
    
    
    func parseImport() throws -> ASTStatement {
        guard case .use = currentToken.type else {
            fatalError("not an import")
        }
        
        guard case .stringLiteral(let moduleName) = next().type, case .semicolon = next().type else {
            fatalError("use keyword should be followed by a string literal, followed by a semicolon")
        }
        next()
        
        return ASTImportStatement(moduleName: moduleName, isInternal: true) // TODO somwhow decide whether the module is internal or not
    }
    
    
    
    func parseTypeDeclaration() throws -> ASTTypeDeclaration {
        guard case .type = currentToken.type else {
            fatalError()
        }
        next()
        
        let name = try parseIdentifier()
        guard case .openingParentheses = currentToken.type else {
            fatalError()
        }
        next()
        
        let attributes = try parseParameterList()
        guard
            case .closingParentheses = currentToken.type,
            case .semicolon = next().type
        else {
            fatalError()
        }
        next()
        
        return ASTTypeDeclaration(name: name, attributes: attributes)
    }
    
    
    func parseParameterList() throws -> [ASTVariableDeclaration] {
        var parameters = [ASTVariableDeclaration]()
        
        while let parameter = try? parseIdentifier() {
            guard case .colon = currentToken.type else {
                fatalError()
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
            fatalError("wrong token")
        }
        next()
        
        let typename = try parseIdentifier()
        
        guard case .openingCurlyBrackets = currentToken.type else {
            fatalError("impl should start w/ opening curly brackets")
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
            fatalError("todo")
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
            throw ParserError.expectedIdentifier
        }
        
        next() // step into the function signature
        let parameters = try parseParameterList()
        
        guard
            case .closingParentheses = currentToken.type,
            case .colon = next().type
        else {
            fatalError("expected closing parens after function signature")
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
            fatalError("composite has to start with opening curly braces")
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
                fatalError(") missing after expr starting with (")
            }
            
            next()
            return expression
        }
        
        if case .openingSquareBrackets = currentToken.type {
            next()
            
            let elements = try parseExpressionList()
            
            guard case .closingSquareBrackets = currentToken.type else {
                fatalError("ugh")
            }
            next()
            
            return ASTArrayLiteral(elements: elements)
        }
        
        guard let expression: ASTExpression =
            (try? parseNumberLiteral())
            ?? (try? parseStringLiteral())
            ?? (try? parseMemberAccess())
            ?? (try? parseIdentifier())
        else {
            //fatalError("ugh")
            throw ParserError.other("unable to find an expression. got \(currentToken) instead")
        }
        
        if BinopTokenTypes.contains(currentToken.type), let binopOperator = ASTBinop.Operator(tokenType: currentToken.type) {
            next()
            return ASTBinop(lhs: expression, operator: binopOperator, rhs: try parseExpression())
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
                
                fatalError("aaargh")
            }
            
            if case .openingSquareBrackets = currentToken.type {
                // array subscript getter
                next()
                let offset = try parseExpression()
                guard case .closingSquareBrackets = currentToken.type else {
                    fatalError("missing ]")
                }
                next()
                
                return ASTArrayGetter(target: identifier, offset: offset)
            }
            
            if case .colon = currentToken.type, case .colon = peek().type {
                // identifier, followed by 2 colons -> static member call
                next()
                next()
                let memberName = try parseIdentifier()
                guard case .openingParentheses = currentToken.type else {
                    fatalError("static member call missing opening parens")
                }
                next()
                
                let arguments = try parseExpressionList()
                
                guard case .closingParentheses = currentToken.type else {
                    fatalError("expected ) after static member call")
                }
                next()
                
                return ASTFunctionCall(
                    functionName: SymbolMangling.mangleStaticMember(ofType: identifier.name, memberName: memberName.name),
                    arguments: arguments,
                    unusedReturnValue: false) // TODO is false the right assumption here?
            }
        }
        
        return expression
    }
    
    
    func parseStringLiteral() throws -> ASTStringLiteral {
        guard case .stringLiteral(let value) = currentToken.type else {
            throw ParserError.other("not a string literal")
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
            throw ParserError.other("not an identifier")
        }
        
        let identifier = try parseIdentifier()
        
        next() // skip the period
        
        let memberName = try parseIdentifier()

        
        if case .openingParentheses = currentToken.type {
            next()
            let arguments = try parseExpressionList()
            guard case .closingParentheses = currentToken.type else {
                fatalError("instance function missing )")
            }
            next()
            
            return ASTTypeMemberFunctionCall(target: identifier, functionName: memberName, arguments: arguments, unusedReturnValue: false) // TODO FIXME
        }
        
        
        if case .openingSquareBrackets = currentToken.type {
            next()
            let offset = try parseExpression()
            guard case .closingSquareBrackets = currentToken.type else {
                fatalError("")
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


