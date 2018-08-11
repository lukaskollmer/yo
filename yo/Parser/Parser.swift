//
//  Parser.swift
//  yo
//
//  Created by Lukas Kollmer on 06.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


func id<T>(_ arg0: T) -> T { return arg0 }

func noop() {}



// TODO (potential improvements to the code in this file that don't necessarily make sense yet since this is still WIP but would make sense when it's all implemented)
// - there's a couple of places where we parse a list of something (types, parameters, etc) and determine whether the list has ended by checking whether one of the next tokens is a comma.
//   once we throw errors instead of the fatalError calls, we might be able to get rid of the comma checks?


typealias _Token = (tag: String, content: String)

// Single Characters
private let _openingParentheses: _Token = ("char", "(")
private let _closingParentheses: _Token = ("char", ")")
private let _openingCurlyBraces: _Token = ("char", "{")
private let _closingCurlyBraces: _Token = ("char", "}")
private let _openingSquareBrackets: _Token = ("char", "[")
private let _closingSquareBrackets: _Token = ("char", "]")
private let _pipe: _Token = ("char", "|")
private let _comma: _Token = ("char", ",")
private let _colon: _Token = ("char", ":")
private let _semicolon: _Token = ("char", ";")
private let _lessThan: _Token = ("char", "<")
private let _greaterThan: _Token = ("char", ">")

// Strings (mostly keywords)
private let _fn: _Token = ("string", "fn")
private let _static: _Token = ("string", "static")


// MARK: constants
private let binopMapping: [String: ASTBinaryOperation.BinopOperation] = [
    "+": .add,
    "-": .sub,
    "*": .mul,
    "/": .div,
    "%": .mod,
    
    "&": .and,
    "|": .or,
    "^": .xor,
    "<<": .shl,
    ">>": .shr
]







extension mpc_ast_t {
    var count: Int {
        return Int(self.children_num)
    }
    
    var lk_children: [mpc_ast_t] {
        guard self.children_num > 0 else { return [] }
        return (0..<self.count).map { children[$0]!.pointee }
    }
    
    var lk_tag: String {
        return String(cString: self.tag)
    }
    
    var lk_content: String {
        return String(cString: self.contents)
    }
    
    
    func print() {
        var _self = self
        mpc_ast_print(&_self)
    }
    
    // TODO is this actually used?
    subscript(index: Int) -> mpc_ast_t {
        set { fatalError() }
        get {
            guard index >= 0 && index < self.children_num else { fatalError() }
            
            return self.children[index]!.pointee
        }
    }
}

extension mpc_ast_t: CustomStringConvertible {
    public var description: String {
        return "<mpc_ast_t tag='\(self.lk_tag)' content='\(self.lk_content)' >"
    }
}

func == (lhs: mpc_ast_t, rhs: _Token) -> Bool {
    return lhs.lk_tag == rhs.0 && lhs.lk_content == rhs.1
}

func != (lhs: mpc_ast_t, rhs: _Token) -> Bool {
    return !(lhs == rhs)
}





class Parser {
    
    func parse(atPath path: String) throws -> AST {
        let parser = LKYOParser.sharedInstance()
        
        guard let ast_ptr = parser.parseFile(atPath: path) else {
            throw LKError.other("error parsing the file")
        }
        
        let ast = ast_ptr.pointee
        
        if CLI.hasFlag(.printAST) {
            print("\(path):")
            ast.print()
        }
        
        var retval = AST()
        
        for (index, child) in ast.lk_children.enumerated() {
            if [0, ast.count - 1].contains(index) { continue }
            
            retval.append(try parseTopLevelStatement(child))
        }
        
        parser.freeAST(ast_ptr)
        return retval
    }
    
    
    
    
    // TODO introduce an `ASTTopLevelStatement` protocol?
    func parseTopLevelStatement(_ ast: mpc_ast_t) throws -> ASTStatement {
        switch ast.lk_tag {
        case "topLevelStatement|type_decl|>":
            return try parseTypeDecl(ast)
            
        case "topLevelStatement|impl|>":
            return try parseImplementation(ast)
            
        case "topLevelStatement|function|>":
            return try parseFunctionDecl(ast)
            
        case "topLevelStatement|import|>":
            return try parseImportStatement(ast)
            
        case "topLevelStatement|protocol|>":
            return try parseProtocolDeclaration(ast)
            
        case "topLevelStatement|global_var|>":
            return try parseGlobalVariableDeclaration(ast)
            
        case "topLevelStatement|enum_decl|>":
            return try parseEnumDeclaration(ast)
            
        default:
            fatalError("unexpected top level statement '\(ast)'")
        }
    }
    
    
    func parseEnumDeclaration(_ ast: mpc_ast_t) throws -> ASTEnumDeclaration {
        let name = try parseIdentifier(ast[1])
        
        guard ast[3] != _closingCurlyBraces else {
            fatalError("enum decl needs at least one case")
        }
        
        let cases = try ast.lk_children[3...]
            .filter { $0.lk_tag == "ident|regex" }
            .map { try parseIdentifier($0) }
        
        
        return ASTEnumDeclaration(name: name, cases: cases)
    }
    
    
    func parseGlobalVariableDeclaration(_ ast: mpc_ast_t) throws -> ASTStaticVariableDeclaration {
        let identifier = try parseIdentifier(ast[1])
        let type = try parseType(ast[3])
        let initialValue: ASTExpression?
        
        if ast.count == 7 {
            // also specifies initial value
            initialValue = try parseExpression(ast[5])
        } else {
            initialValue = nil
        }
        
        return ASTStaticVariableDeclaration(identifier: identifier, type: type, initialValue: initialValue)
    }
    
    
    
    func parseStatement(_ ast: mpc_ast_t) throws -> ASTStatement {
        switch ast.lk_tag {
        case "stmt|ret|>":
            return try parseReturnStatement(ast)
            
        case "stmt|assignment|>":
            return try parseAssignment(ast)
            
        case "stmt|var_decl|>":
            return try parseVariableDeclaration(ast)
            
        case "stmt|stmt_fn_call|>":
            return try parseFunctionCall(ast[0], unusedReturnValue: true)
            
        case "stmt|cond_stmt|if_stmt|>", "stmt|cond_stmt|while_stmt|>":
            return try parseConditionalStatement(ast)
            
        case "stmt|for_loop|>":
            return try parseForLoop(ast)
            
        case "stmt|asm_stmt|>":
            return try parseAsmStatement(ast)
            
        case "stmt|defer_block|>":
            return try parseDeferBlock(ast)
            
        default:
            fatalError("unexpected statement \(ast)")
        }
    }
    
    
    func parseDeferBlock(_ ast: mpc_ast_t) throws -> ASTDeferStatement {
        let body = try parseComposite(ast[1])
        return ASTDeferStatement(body: body)
    }
    
    
    func parseAsmStatement(_ ast: mpc_ast_t) throws -> ASTRawWIPInstruction {
        let instruction: WIPInstruction
        
        let operationName = try parseIdentifier(ast[2]).value
        guard let operation = Operation(name: operationName) else {
            fatalError("unknown operation name: \(operationName)")
        }
        
        switch ast[4].lk_tag.hasPrefix {
        case "string":
            // NOTE: be careful when specifying a label manually in the source code
            // you have to keep in mind that the compiler might insert instructions
            // before or after the label, which might not lead to the desired behaviour
            // Example:
            // ```
            // fn main(): int {
            //     __asm(label, "my_label");
            //     [...]
            // }
            // ```
            // would result in the following instructions being generated:
            // ```
            // [...]
            // main:
            // alloc n  (n: stack space for the procedure)
            // my_label:
            // [...]
            // ```
            // so even though `__asm(label, "my_label");` is the first statement in the function body,
            // there can (and basically always are!) still be other instructions above it
            
            let immediate = try parseStringLiteral(ast[4]).value
            if operation == .label {
                instruction = .label(immediate)
            } else if operation == .comment {
                instruction = .comment(immediate)
            } else {
                instruction = .unresolved(operation, immediate)
            }
            
        case "number":
            let immediate = try parseNumberLiteral(ast[4]).value
            instruction = .operation(operation, immediate)
            
        default:
            fatalError("unexpected immediate type in __asm literal: \(ast[4])")
        }
        
        return ASTRawWIPInstruction(instruction: instruction)
    }
    
    
    
    
    func parseImportStatement(_ ast: mpc_ast_t) throws -> ASTImportStatement {
        let moduleName = try parseStringLiteral(ast[1])
        return ASTImportStatement(moduleName: moduleName.value)
    }
    
    
    func parseStringLiteral(_ ast: mpc_ast_t) throws -> ASTStringLiteral {
        guard ast.lk_tag.hasSuffix("string|regex") else { fatalError() }
        
        var rawValue = ast.lk_content
        
        // drop the trailing and leading quotes
        rawValue = String(rawValue.dropFirst().dropLast())
        
        var value = ""
        
        // handle escaped characters
        do {
            let scalars = rawValue.unicodeScalars.map { $0 }
            var lastScalarWasEscapeSequence = false
            
            for scalar in scalars {
                value.unicodeScalars.append(scalar)
                
                if scalar == "\\" {
                    lastScalarWasEscapeSequence = true
                    value.unicodeScalars.removeLast()
                    
                    continue
                }
                
                if lastScalarWasEscapeSequence {
                    lastScalarWasEscapeSequence = false
                    
                    switch scalar {
                    case "n":
                        value.unicodeScalars.removeLast()
                        value += "\n"
                        
                    case "\"":
                        break
                        //value.unicodeScalars.removeLast()
                        //value += ""
                    default: fatalError("unhandled escape sequence: \(scalar)")
                    }
                }
            }
        }
        
        
        return ASTStringLiteral(value: value)
    }
    
    
    
    
    func parseProtocolDeclaration(_ ast: mpc_ast_t) throws -> ASTProtocolDeclaration {
        var annotations = [ASTAnnotation.Element]()
        var protocolNameIndex = 0
        
        while ast[protocolNameIndex].lk_tag != "ident|regex" {
            if ast[protocolNameIndex].lk_tag == "annotation|>" {
                annotations.append(contentsOf: try parseAnnotation(ast[0]).elements)
            }
            
            protocolNameIndex += 1
        }
        
        let protocolName = try parseIdentifier(ast[protocolNameIndex])
        
        // assuming there are only functions in a protocol
        // TODO add support for function signatures!!!
        let functions = try ast.lk_children
            .filter { $0.lk_tag == "function|>" }
            .map    { try parseFunctionDecl($0, implTypenameContext: protocolName.value)}
        
        
        return ASTProtocolDeclaration(
            name: protocolName,
            functions: functions,
            annotations: annotations
        )
    }
    
    
    
    
    
    func parseTypeDecl(_ ast: mpc_ast_t) throws -> ASTTypeDeclaration {
        let isStruct = ast[0].lk_content == "struct"
        let typename = try parseIdentifier(ast[1])
        
        let attributes: [ASTVariableDeclaration]
        
        if ast[3] == _closingParentheses {
            attributes = []
        } else {
            attributes = try parseParameterList(ast[3])
        }
        
        return ASTTypeDeclaration(
            name: typename,
            attributes: attributes,
            protocols: [], // TODO
            annotations: isStruct ? ["struct"] : []
        )
    }
    
    
    
    
    
    func parseImplementation(_ ast: mpc_ast_t) throws -> ASTTypeImplementation {
        let typename = try parseIdentifier(ast[1])
        
        // assuming an `impl` block contains only functions
        let functions = try ast.lk_children
            .filter { $0.lk_tag == "function|>" }
            .map { try parseFunctionDecl($0, implTypenameContext: typename.value) }
        
        return ASTTypeImplementation(
            typename: typename,
            functions: functions
        )
    }
    
    
    
    func parseAnnotation(_ ast: mpc_ast_t) throws -> ASTAnnotation {
        // TODO add support for non-boolean annotation elements!
        
        let elements = try ast.lk_children
            .enumerated()
            .filter { $0.offset.isOdd } // print the ast, you'll see that only every second element is one of the annotation elements
            .map { try parseIdentifier($0.element).value }
            .map { ASTAnnotation.Element(key: $0, value: .bool(true)) }
        
        
        return ASTAnnotation(elements: elements)
    }
    
    
    
    
    
    func parseFunctionDecl(_ ast: mpc_ast_t, implTypenameContext: String = "") throws -> ASTFunctionDeclaration {
        var functionNameIndex = 0
        let kind: ASTFunctionDeclaration.Kind
        var annotations = [ASTAnnotation]()
        
        let signature = ast[0]
        
        do {
            while signature[functionNameIndex].lk_tag == "annotation|>" {
                annotations.append(try parseAnnotation(signature[functionNameIndex]))
                functionNameIndex += 1
            }
            
        }
        
        if signature[functionNameIndex] == _static {
            functionNameIndex += 2
            kind = .staticImpl(implTypenameContext)
        } else {
            functionNameIndex += 1
            kind = implTypenameContext == "" ? .global : .impl(implTypenameContext)
        }
        
        let functionName = try parseIdentifier(signature[functionNameIndex])
        
        
        let parameters: [ASTVariableDeclaration]
        let closingParenthesesIndex: Int
        
        if signature[functionNameIndex + 2] == _closingParentheses {
            // no parameters
            parameters = []
            closingParenthesesIndex = functionNameIndex + 2
        } else {
            parameters = try parseParameterList(signature[functionNameIndex + 2])
            closingParenthesesIndex = functionNameIndex + 3
        }
        
        // TODO missing return type implies void
        let returnType = try parseType(signature[closingParenthesesIndex + 2])
        let body = try parseComposite(ast[1])
        
        return ASTFunctionDeclaration(
            name: functionName,
            parameters: parameters,
            returnType: returnType,
            kind: kind,
            annotations: annotations.allElements,
            body: body
        )
    }
    
    
    
    
    
    func parseParameterList(_ ast: mpc_ast_t) throws -> [ASTVariableDeclaration] {
        var parameters = [ASTVariableDeclaration]()
        
        // TODO rewrite this loop
        var index = 0
        while let identifier = try? parseIdentifier(ast[index]) {
            guard ast[index + 1] == _colon else { fatalError() }
            
            let type = try parseType(ast[index + 2])
            parameters.append(ASTVariableDeclaration(identifier: identifier, type: type))
            
            if index + 3 < ast.count {
                guard ast[index + 3] == _comma else { fatalError() }
            } else {
                break
            }
            
            index += 4
        }
        
        return parameters
    }
    
    
    
    
    func parseComposite(_ ast: mpc_ast_t) throws -> ASTComposite {
        guard ast.lk_tag == "composite|>" else { // TODO is that guard *really* necessary?
            fatalError()
        }
        
        return ASTComposite(
            statements: try ast.lk_children.excludingFirstAndLast.map { try parseStatement($0) }
        )
        
        // or use one of the versions below?
        
        /*var statements = [ASTStatement]()
        var index = 1
        while ast[index] != _closingCurlyBraces {
            statements.append(try parseStatement(ast[index]))
            index += 1
        }*/
        
        /*var index = 1
        while let statement = try? parseStatement(ast[index]) {
            statements.append(statement)
            
            index += 1
            if ast[index] == _closingCurlyBraces {
                break
            }
        }*/
    }
    
    
    
    func parseVariableDeclaration(_ ast: mpc_ast_t) throws -> ASTStatement {
        
        let identifier = try parseIdentifier(ast[1])
        let type: ASTType
        let initialValue: ASTExpression?
        
        // index of the initial value, if the variable declaration were to have one
        let initialValueIndex: Int
        
        
        if ast[2] == _colon {
            type = try parseType(ast[3])
            initialValueIndex = 5
        } else {
            type = .unresolved
            initialValueIndex = 3
        }
        
        if initialValueIndex < ast.count {
            initialValue = try parseExpression(ast[initialValueIndex])
        } else {
            initialValue = nil
        }
        
        
        if let initialValue = initialValue {
            return ASTComposite(
                statements: [
                    ASTVariableDeclaration(identifier: identifier, type: type),
                    ASTAssignment(target: identifier, value: initialValue)
                ],
                introducesNewScope: false
            )
        }
        
        return ASTVariableDeclaration(identifier: identifier, type: type)
    }
    
    
    
    
    
    
    // Returns either ASTAssignment or ASTArraySetter
    func parseAssignment(_ ast: mpc_ast_t) throws -> ASTStatement {
        let target = try parseVariableAccess(ast[0])
        
        // TODO introduce ASTInPlaceBinaryOperation
        let isInPlaceBinop = ast[1].lk_tag == "in_place_binop|string"
        let binopOperation: ASTBinaryOperation.BinopOperation!
        
        if isInPlaceBinop {
            let rawBinopOperator = String(ast[1].lk_content.dropLast())
            binopOperation = binopMapping[rawBinopOperator]!
        } else {
            binopOperation = nil
        }
        
        // TODO this evaluates target twice which is really terrible
        // Introduce a temporary local variable instead!
        
        // subscript assignment
        if ast[1] == _openingSquareBrackets {
            let offset = try parseExpression(ast[2])
            let value = try parseExpression(ast[5])
            
            if !isInPlaceBinop {
                return ASTArraySetter(target: target, offset: offset, value: value)
            }
            
            return ASTArraySetter(
                target: target,
                offset: offset,
                value: ASTBinaryOperation(
                    lhs: target,
                    operation: binopOperation,
                    rhs: value
                )
            )
        }
        
        let value = try parseExpression(ast[2])
        
        if !isInPlaceBinop {
            return ASTAssignment(target: target, value: value)
        }
        
        return ASTAssignment(
            target: target,
            value: ASTBinaryOperation(
                lhs: target,
                operation: binopOperation,
                rhs: value
            )
        )
    }
    
    
    
    
    
    func parseConditionalStatement(_ ast: mpc_ast_t) throws -> ASTConditionalStatement {
        let kind: ASTConditionalStatement.Kind
        
        let condition = try parseCondition(ast[1])
        let body = try parseComposite(ast[2])
        
        if ast[0].lk_content == "if" {
            if ast.count == 4 { // 4 children -> has else branch
                kind = .if(elseBranch: try parseComposite(ast[3][1]))
            } else {
                kind = .if(elseBranch: nil)
            }
        } else {
            kind = .while
        }
        
        return ASTConditionalStatement(condition: condition, body: body, kind: kind)
    }
    
    
    
    func parseCondition(_ ast: mpc_ast_t) throws -> ASTCondition {
        if ast.count == 0 || ast.lk_tag.hasPrefix("cond|lcond|expr") {
            // a) no children -> single expression that is implicitly compared to true
            // b) if the entire condition ast is an expression, we're also dealing w/ some expression being implicitly compared to true
            // TODO introduce a `ASTImplicitTrueComparison` class, instead of creating the comparison object here in the parser
            return ASTComparison(
                lhs: try parseExpression(ast),
                operation: .equal,
                rhs: ASTBooleanLiteral(value: true)
            )
        }
        
        switch ast.lk_tag.hasSuffix {
        case "lcond|comp|>":
            return try parseComparison(ast)
            
        case "cond|bin_cond|>":
            return try parseBinaryCondition(ast)
            
        default:
            fatalError()
        }
        
        fatalError()
    }
    
    
    
    
    func parseComparison(_ ast: mpc_ast_t) throws -> ASTComparison {
        let lhs = try parseExpression(ast[0])
        let rhs = try parseExpression(ast[2])
        
        // TODO make this a global constant?
        let operationMapping: [String: ASTComparison.Operation] = [
            "==": .equal,
            "!=": .notEqual,
            "<" : .less,
            ">" : .greater,
            "<=": .lessEqual,
            ">=": .greaterEqual
        ]
        
        return ASTComparison(
            lhs: lhs,
            operation: operationMapping[ast[1].lk_content]!,
            rhs: rhs
        )
    }
    
    
    
    func parseBinaryCondition(_ ast: mpc_ast_t) throws -> ASTBinaryCondition {
        let lhs = try parseCondition(ast[0])
        let rhs = try parseCondition(ast[2])
        
        let operatorMapping: [String: ASTBinaryCondition.Operator] = [
            "&&": .and,
            "||": .or
        ]
        
        // TODO continue parsing for more binary conditions!
        
        return ASTBinaryCondition(
            lhs: lhs,
            operator: operatorMapping[ast[1].lk_content]!,
            rhs: rhs
        )

    }
    
    
    
    
    
    
    // TODO introduce a proper ASTForLoop type!!!
    func parseForLoop(_ ast: mpc_ast_t) throws -> ASTStatement {
        let identifier = try parseIdentifier(ast[1])
        
        // TODO add support for non-rangeliteral loops!!!
        //let target = try parseExpression(ast[3])
        
        guard ast[3].lk_tag == "for_loop_target|range|>" else {
            fatalError("non range-based loops not (yet?) supported")
        }
        
        let range_ast = ast[3]
        let start = try parseExpression(range_ast[0])
        let end = try parseExpression(range_ast[2])
        let body = try parseComposite(ast[4])
        
        let kind: ASTComparison.Operation
        
        switch range_ast[1].lk_content {
        case "...":
            kind = .lessEqual
        case "..<":
            kind = .less
        default:
            fatalError("invalid range type")
        }
        
        
        
        let forLoop: ASTComposite = [
            ASTVariableDeclaration(identifier: identifier, type: .int), // TODO either infer type, or allow specifying a type
            ASTAssignment(target: identifier, value: start),
            
            // TODO save the end index as a local variable, so that we don't have to re-evaluate it every time
            //ASTVariableDeclaration(identifier: "", type: <#T##ASTType#>)
            
            ASTConditionalStatement(
                condition: ASTComparison(
                    lhs: identifier,
                    operation: kind,
                    rhs: end
                ),
                body: body.appending(statements: [
                    ASTAssignment(
                        target: identifier,
                        value: ASTBinaryOperation(
                            lhs: identifier,
                            operation: .add,
                            rhs: 1 as ASTNumberLiteral
                        )
                    )
                ]),
                kind: .while
            )
        ]
        
        return forLoop
    }
    
    
    
    
    func parseReturnStatement(_ ast: mpc_ast_t) throws -> ASTReturnStatement {
        // TODO make ASTReturnStatement.expression optional and create the empty return value in the compiler
        if ast[1] == _semicolon {
            return ASTReturnStatement(expression: ASTNumberLiteral(value: 0).as(.any))
        }
        
        return ASTReturnStatement(expression: try parseExpression(ast[1]))
    }
    
    
    
    
    
    func parseType(_ ast: mpc_ast_t) throws -> ASTType {
        switch ast.lk_tag {
        // "simple" typename that's basically just an identifier
        case "type|ident|regex":
            let typename = ast.lk_content
            return ASTType.primitiveTypenames.contains(typename) ? .primitive(name: typename) : .complex(name: typename)
            
        // function pointer
        // ie `fn<(int, String): String>`
        case "type|fn_ptr|>":
            
            // TODO this could be extracted into a `parseTypeList` function (not sure where that would be needed/useful though)
            var parameterTypes = [ASTType]()
            
            var index = 3
            while ast[index] != _closingParentheses {
                parameterTypes.append(try parseType(ast[index]))
                
                if ast[index + 1] == _comma {
                    index += 2
                } else {
                    // next char will be the closing paren
                    index += 1
                }
            }
            
            // `index` is now the index of the parameter list's closing parentheses
            guard let returnType = try? parseType(ast[index + 2]) else {
                fatalError()
            }
            return ASTType.function(returnType: returnType, parameterTypes: parameterTypes)
            
        default:
            fatalError("unexpected type \(ast)")
        }
        
        fatalError()
    }
    
    
    
    
    
    
    
    
    
    
    
    // MARK: Expressions
    
    func parseExpression(_ ast: mpc_ast_t) throws -> ASTExpression {
        
        switch ast.lk_tag.hasSuffix {
        case "lexpr|fn_call|>":
            return try parseFunctionCall(ast)
            
        //case "lexpr|number|regex":
        case "number|number_b10|regex", "number|number_b02|regex", "number|number_b08|regex", "number|number_b16|regex":
            return try parseNumberLiteral(ast)
            
        case "binop_add|>", "binop_mul|>": // TODO test that binop_mul works properly
            return try parseBinop(ast)
            
        case "var_access|>", "var_access|ident|regex":
            return try parseVariableAccess(ast)
            
        case "expr|typecast|>":
            return try parseTypecast(ast)
            
        case "lexpr|string|regex":
            return try parseStringLiteral(ast)
            
        case "lexpr|array_literal|>":
            return try parseArrayLiteral(ast)
            
        case "expr|boxed_expr|>":
            return try parseBoxedExpression(ast)
            
        case "lexpr|>" where ast.count == 3 && ast[0] == _openingParentheses && ast[2] == _closingParentheses:
            // Expression wrappped in parentheses
            return try parseExpression(ast[1])
            
        case "lexpr|boolean|string":
            return try parseBooleanLiteral(ast)
            
        case "lexpr|subscript|>":
            return try parseSubscriptAccess(ast)
            
        case "unary|>":
            return try parseUnaryExpression(ast)
            
        case "lambda|>":
            return try parseLambda(ast)
            
        case "lexpr|static_target|>":
            return try parseStaticMemberGetter(ast)
            
        default:
            fatalError("unexpected expression \(ast)")
        }
        
        fatalError()
    }
    
    
    func parseStaticMemberGetter(_ ast: mpc_ast_t) throws -> ASTStaticMemberGetter {
        let typename = try parseIdentifier(ast[0])
        let memberName = try parseIdentifier(ast[2])
        return ASTStaticMemberGetter(typename: typename, memberName: memberName)
    }
    
    
    
    
    func parseLambda(_ ast: mpc_ast_t) throws -> ASTLambda {
        
        var parameters = [ASTVariableDeclaration]()
        
        var index = 1
        while ast[index].lk_tag.hasPrefix("ident") {
            let parameterName = try parseIdentifier(ast[index])
            let type: ASTType
            
            if ast[index + 1] == _colon {
                type = try parseType(ast[index + 2])
                index += 4
            } else {
                type = .unresolved
                index += 2
            }
            
            parameters.append(ASTVariableDeclaration(identifier: parameterName, type: type))
        }
        
        if parameters.isEmpty {
            index += 1 // make index point at the arrow
        }
        
        // `index` should now point at the arrow right before the lambda body
        let body = try parseComposite(ast[index + 1])
        
        return ASTLambda(
            signature: .unresolved,
            parameters: parameters,
            body: body
        )
    }
    
    
    
    
    
    
    
    
    
    func parseUnaryExpression(_ ast: mpc_ast_t) throws -> ASTUnaryExpression {
        // TODO make this a global constant?
        let unaryOperatorMapping: [String: ASTUnaryExpression.Operator] = [
            "-": .negate,
            "~": .bitwiseNot,
            "!": .logicalNegation
        ]
        
        return ASTUnaryExpression(
            expression: try parseExpression(ast[1]),
            operator: unaryOperatorMapping[ast[0].lk_content]!
        )
    }
    
    
    
    func parseBooleanLiteral(_ ast: mpc_ast_t) throws -> ASTBooleanLiteral {
        let value: Bool
        
        switch ast.lk_content {
        case "true":
            value = true
            
        case "false":
            value = false
            
        default:
            fatalError("only 'true' and 'false' are valid boolean literals, idiot")
        }
        
        return ASTBooleanLiteral(value: value)
    }
    
    
    
    func parseIdentifier(_ ast: mpc_ast_t) throws -> ASTIdentifier {
        guard ast.lk_tag.hasSuffix("ident|regex") else { fatalError() }
        
        // TODO add support for builtin identifiers (like `#function`)
        return ASTIdentifier(value: ast.lk_content)
    }
    
    
    func parseNumberLiteral(_ ast: mpc_ast_t) throws -> ASTNumberLiteral {
        
        let base: Int
        var rawStringValue = ast.lk_content.replacingOccurrences(of: "_", with: "")
        
        switch ast.lk_tag.hasSuffix {
        case "number|number_b02|regex":
            base = 2
        
        case "number|number_b08|regex":
            base = 8
        
        case "number|number_b10|regex":
            base = 10
        
        case "number|number_b16|regex":
            base = 16
        
        default:
            fatalError()
        }
        
        
        if [2, 16].contains(base) {
            // we can keep in the leading 0 in octal literals
            rawStringValue = rawStringValue.ns.substring(from: 2)
        }
        
        guard let value = Int(rawStringValue, radix: base) else {
            fatalError("Unable to parse number literal \(ast.lk_content)")
        }
        
        return ASTNumberLiteral(value: value)
    }
    
    
    
    func parseTypecast(_ ast: mpc_ast_t) throws -> ASTTypecast {
        return ASTTypecast(
            expression: try parseExpression(ast[0]),
            type: try parseType(ast[2])
        )
    }
    
    
    
    
    func parseArrayLiteral(_ ast: mpc_ast_t) throws -> ASTArrayLiteral {
        let kind: ASTArrayLiteral.Kind = ast[0] == _openingSquareBrackets ? .complex : .primitive
        let elements = try parseExpressionList(ast[1])
        
        return ASTArrayLiteral(elements: elements, kind: kind)
    }
    
    
    func parseBoxedExpression(_ ast: mpc_ast_t) throws -> ASTBoxedExpression {
        return ASTBoxedExpression(expression: try parseExpression(ast[1]))
    }
    
    
    
    func parseSubscriptAccess(_ ast: mpc_ast_t) throws -> ASTArrayGetter {
        return ASTArrayGetter(
            target: try parseExpression(ast[0]),
            offset: try parseExpression(ast[2])
        )
    }
    
    
    
    
    func parseExpressionList(_ ast: mpc_ast_t) throws -> [ASTExpression] {
        if ast.count == 0 {
            // guaranteed to be just a single expression
            return [try parseExpression(ast)]
        }
        
        if ast.lk_tag == "expr_list|>" {
            // guaranteed to be multiple expressions
            return try (0..<ast.count)
                .filter { $0.isEven }
                .map    { try parseExpression(ast[$0]) }
        }
        
        // might still be just a single expression
        
        switch ast.lk_tag {
        case "expr_list|expr|lexpr|fn_call|>":
            return [try parseFunctionCall(ast)]
            
        default:
            return [try parseExpression(ast)]
        }
    }
    
    
    
    
    func parseFunctionCall(_ ast: mpc_ast_t, unusedReturnValue: Bool = false) throws -> ASTFunctionCall {
        let target_ast = ast[0]
        
        let functionName: String
        
        
        let arguments: [ASTExpression]
        
        if ast[2] == _closingParentheses {
            arguments = []
        } else {
            arguments = try parseExpressionList(ast[2])
        }
        
        switch target_ast.lk_tag {
        case "call_target|var_access|ident|regex":
            functionName = try parseIdentifier(target_ast).value
            
            
        case "call_target|static_target|>":
            functionName = SymbolMangling.mangleStaticMember(
                ofType: try parseIdentifier(target_ast[0]).value,
                memberName: try parseIdentifier(target_ast[2]).value
            )
            
        case "call_target|var_access|>":
            
            // ASTMemberAccess containing exclusively attributes
            // The last attribute is the name of the function we're calling
            let variableAccess = try parseVariableAccess(target_ast) as! ASTMemberAccess // TODO is it safe to force cast here? (update: it probably is)
            
            
            guard case ASTMemberAccess.Kind.attribute(let _functionName)? = variableAccess.members.last else { fatalError() }
            
            let fixed = ASTMemberAccess(
                members:
                    Array(variableAccess.members.prefix(upTo: variableAccess.members.count - 1))
                    + [ ASTMemberAccess.Kind.functionCall(name: _functionName, arguments: arguments, unusedReturnValue: unusedReturnValue) ]
            )
            
            // Ugh god i fuckin hate myself for this
            return unsafeBitCast(fixed, to: ASTFunctionCall.self)
            
        default: fatalError()
        }
        
        return ASTFunctionCall(
            functionName: functionName,
            arguments: arguments,
            unusedReturnValue: unusedReturnValue
        )
    }
    
    
    
    
    // TODO rename?
    // returns ASTIdentifier or ASTMemberAccess
    func parseVariableAccess(_ ast: mpc_ast_t) throws -> ASTExpression {
        if ast.count == 0 {
            return try parseIdentifier(ast)
        }
        
        // chained member access?
        
        var members = [ASTMemberAccess.Kind]()
        members.append(.initial_identifier(try parseIdentifier(ast[0])))
        
        var index = 2
        while index < ast.count, let identifier = try? parseIdentifier(ast[index]) {
            // TODO what about function calls!
            members.append(.attribute(name: identifier))
            index += 2
        }
        
        return ASTMemberAccess(members: members)
    }
    
    
    
    func parseBinop(_ ast: mpc_ast_t) throws -> ASTBinaryOperation {
        return ASTBinaryOperation(
            lhs: try parseExpression(ast[0]),
            operation: binopMapping[ast[1].lk_content]!,
            rhs: try parseExpression(ast[2])
        )
    }
    
    
    
}


func ~= <T> (pattern: T, value: (T) -> Bool) -> Bool {
    return value(pattern)
}

