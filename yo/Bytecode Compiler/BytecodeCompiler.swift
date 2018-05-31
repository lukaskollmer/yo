//
//  Compiler.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


/// Class that compiles an AST to bytecode instructions
class BytecodeCompiler {
    
    private var instructions = [WIPInstruction]()
    private var _counter = 0 // counter used to generate unique labels for if/while statements
    private func getCounter() -> Int {
        _counter += 1
        return _counter
    }
    
    
    // Scope info
    private var scope = Scope(type: .global)
    private var typeCache = TypeCache()
    private var functions = [String: SemanticAnalyzer.FunctionInfo]()
    
    
    
    init() {
        // fill the functions table w/ all native functions
        for builtin in Runtime.builtins {
            functions[builtin.name] = (
                argc: builtin.argc,
                parameterTypes: [], // TODO
                returnType: "int" // TODO
            )
        }
    }
    
    
    func compile(ast: [ASTNode]) throws -> [WIPInstruction] {
        var importedPaths = [String]()
        
        let ast = try ast.lk_flatMap { node -> [ASTNode] in
            if let importStatement = node as? ASTImportStatement {
                let path = ImportPathResolver.resolve(moduleName: importStatement.moduleName)
                guard !importedPaths.contains(path) else { return [] }
                
                importedPaths.append(path)
                return try yo.tokenize(atPath: path)
            }
            return [node]
        }
        
        
        // add the bootstrapping instructions
        add(.push, unresolvedLabel: "main")
        add(.call, 0)
        add(.push, -1)
        add(.jump, unresolvedLabel: "end")
        
        // perform semantic analysis
        let semanticAnalysis = SemanticAnalyzer().analyze(ast: ast)
        
        // import semantic analysis results
        self.functions.insert(contentsOf: semanticAnalysis.globalFunctions)
        semanticAnalysis.types.forEach(self.typeCache.register)
        
        // run codegen
        ast.forEach(handle)
        
        add(label: "end")
        return instructions
        
    }
}

// MARK: Codegen
private extension BytecodeCompiler {
    
    func add(_ operation: Operation, _ immediate: Int = 0) {
        instructions.append(.operation(operation, immediate))
    }
    
    func add(label: String) {
        instructions.append(.label(label))
    }
    
    func add(_ operation: Operation, unresolvedLabel: String) {
        instructions.append(.unresolved(operation, unresolvedLabel))
    }
    
    
    // updates the scope until `block` returns
    func withScope(_ newScope: Scope, block: () -> Void) {
        let previousScope = scope
        scope = newScope
        
        block()
        
        scope = previousScope
    }
}

private extension BytecodeCompiler {
    
    func handle(node: ASTNode) {
        
        if let function = node as? ASTFunctionDeclaration {
            handle(function: function)
            
        } else if let returnStatement = node as? ASTReturnStatement {
            handle(return: returnStatement)
            
        } else if let numberLiteral = node as? ASTNumberLiteral {
            handle(numberLiteral: numberLiteral)
            
        } else if let functionCall = node as? ASTFunctionCall {
            handle(functionCall: functionCall)
            
        } else if let binop = node as? ASTBinaryOperation {
            handle(binop: binop)
            
        } else if let identifier = node as? ASTIdentifier {
            handle(identifier: identifier)
            
        } else if let unary = node as? ASTUnary {
            handle(unary: unary)
            
        } else if let composite = node as? ASTComposite {
            handle(composite: composite)
            
        } else if let assignment = node as? ASTAssignment {
            handle(assignment: assignment)
            
        } else if let _ = node as? ASTVariableDeclaration {
            // we don't need to explicitly handle variable declarations since that info is also passed in the ASTFunctionDeclaration node
            // TODO we might be able to remove all variable declarations from the a function's body statements?
            
        } else if let conditionalStatement = node as? ASTConditionalStatement {
            handle(conditionalStatement: conditionalStatement)
            
        } else if let comparison = node as? ASTComparison {
            handle(comparison: comparison)
            
        } else if let typeDeclaration = node as? ASTTypeDeclaration {
            handle(typeDeclaration: typeDeclaration)
            
        } else if let arraySetter = node as? ASTArraySetter {
            handle(arraySetter: arraySetter)
            
        } else if let arrayGetter = node as? ASTArrayGetter {
            handle(arrayGetter: arrayGetter)
            
        } else if let memberGetter = node as? ASTTypeMemberGetter {
            handle(memberGetter: memberGetter)
            
        } else if let typeImplementation = node as? ASTTypeImplementation {
            handle(typeImplementation: typeImplementation)
            
        } else if let typeMemberFunctionCall = node as? ASTTypeMemberFunctionCall {
            handle(typeMemberFunctionCall: typeMemberFunctionCall)
            
        } else if let memberSetter = node as? ASTTypeMemberSetter {
            handle(memberSetter: memberSetter)
            
        } else if let _ = node as? ASTImportStatement {
            
        } else if let stringLiteral = node as? ASTStringLiteral {
            handle(stringLiteral: stringLiteral)
            
        } else if let rawInstruction = node as? ASTRawWIPInstruction {
            instructions.append(rawInstruction.instruction)
            
        } else if let arrayLiteral = node as? ASTArrayLiteral {
            handle(arrayLiteral: arrayLiteral)
            
        } else if let _ = node as? ASTNoop {
            
        } else {
            fatalError("unhandled node \(node)")
        }
        
    }
    
    
    
    // MARK: Handle Statements
    
    func handle(typeDeclaration: ASTTypeDeclaration) {
        
        let typename = typeDeclaration.name.name

        // generate an initializer
        let _self = ASTIdentifier(name: "self")
        let initializer = ASTFunctionDeclaration(
            name: ASTIdentifier(name: "init"),
            parameters: typeDeclaration.attributes,
            returnType: typeDeclaration.name,
            body: [
                // 1. declare self
                ASTVariableDeclaration(identifier: _self, typename: typeDeclaration.name),
                
                // 2. allocate space on the heap
                ASTAssignment(
                    target: _self,
                    value: ASTFunctionCall(
                        functionName: SymbolMangling.mangleStaticMember(ofType: "runtime", memberName: "alloc"),
                        arguments: [ASTNumberLiteral(value: typeDeclaration.attributes.count + 1)],
                        unusedReturnValue: false)
                ),
                
                // set the address of the type's dealloc function
                // since we have to resolve this at compile time, we push the address of the function onto the stack, then use a noop as the value expression
                // TODO if the type did not define a dealloc function, set it to -1 to indicate that
                ASTRawWIPInstruction(instruction: .unresolved(.push, SymbolMangling.mangleInstanceMember(ofType: typename, memberName: "dealloc"))),
                ASTArraySetter(target: _self, offset: ASTNumberLiteral(value: 0), value: ASTNoop()),
                
                // go through the parameters and fill the attributes
                ASTComposite(
                    statements: typeDeclaration.attributes.enumerated().map { attribute in
                        return ASTArraySetter(target: _self, offset: ASTNumberLiteral(value: attribute.offset + 1), value: attribute.element.identifier)
                    }
                ),
                
                // 4. return the newly created object
                ASTReturnStatement(returnValueExpression: _self)
            ],
            kind: .staticImpl(typename)
        )
        
        handle(function: initializer)
    }
    
    
    func handle(function: ASTFunctionDeclaration) {
        guard function.localVariables.intersection(with: function.parameters).isEmpty else {
            fatalError("local variable cannot (yet?) shadow parameter")
        }
        
        withScope(Scope(type: .function(function.mangledName), parameters: function.parameters, localVariables: function.localVariables)) {
            // function entry point
            add(label: function.mangledName)
            
            // allocate space for local variables?
            let numberOfLocalVariables = scope.size - function.parameters.count
            add(.alloc, numberOfLocalVariables)
            
            // Generate instructions for the function body
            function.body.forEach(handle)
            
            // if the function doesn't have a return statement, we implicitly return 0
            if !(function.body.last is ASTReturnStatement) {
                handle(return: ASTReturnStatement(returnValueExpression: ASTNumberLiteral(value: 0)))
            }
        }
    }
    
    
    func handle(composite: ASTComposite) {
        composite.statements.forEach(handle)
    }
    
    
    func handle(return returnStatement: ASTReturnStatement) {
        handle(node: returnStatement.returnValueExpression)
        add(.ret, scope.size)
    }
    
    func handle(arraySetter: ASTArraySetter) {
        handle(node: arraySetter.value)
        handle(node: arraySetter.offset)
        handle(node: arraySetter.target)
        
        add(.storeh)
    }
    
    func handle(arrayGetter: ASTArrayGetter) {
        handle(node: arrayGetter.offset)
        handle(node: arrayGetter.target)
        
        add(.loadh)
    }
    
    func handle(typeImplementation: ASTTypeImplementation) {
        for function in typeImplementation.functions {
            handle(function: function)
        }
    }
    
    func handle(typeMemberFunctionCall: ASTTypeMemberFunctionCall) {
        let typename = scope.type(of: typeMemberFunctionCall.target.name)
        
        let call = ASTFunctionCall(
            functionName: SymbolMangling.mangleInstanceMember(ofType: typename, memberName: typeMemberFunctionCall.functionName.name),
            arguments: [typeMemberFunctionCall.target] + typeMemberFunctionCall.arguments,
            unusedReturnValue: typeMemberFunctionCall.unusedReturnValue
        )
        
        handle(functionCall: call)
    }
    
    
    
    
    
    
    
    func handle(conditionalStatement: ASTConditionalStatement) {
        guard case .function(let functionName) = scope.type else {
            fatalError("global if statement")
        }
        
        let counter = getCounter()
        let generateLabel: (String) -> String = { "\(functionName)_ifwhile_\(counter)_\($0)" } // TOOD replace `ifwhile` w/ just if or while?
        
        
        
        
        // 1. handle the condition
        // we only need a label for the condition if this is a while statement
        if case .while = conditionalStatement.kind {
            add(label: generateLabel("cond"))
        }
        handle(condition: conditionalStatement.condition)
        
        // 2. handle the jump to the body if the condition is true
        // if the condition is false, we fall through to jump to the else branch (or the end, if there is no else branch)
        add(.jump, unresolvedLabel: generateLabel("body"))
        
        // 3. handle the else jump
        // if this is a while loop, we just jump to the end
        // if this is an if statement that has an else branch, we jump there, otherwise to the end
        add(.push, -1)
        if case .if(let elseBranch) = conditionalStatement.kind, elseBranch != nil {
            add(.jump, unresolvedLabel: generateLabel("else"))
        } else {
            add(.jump, unresolvedLabel: generateLabel("end"))
        }
        
        
        // 4. handle the body
        add(label: generateLabel("body"))
        handle(composite: conditionalStatement.body)
        
        // depending on whether this is an if or while statement, we jump to the end (if) or the condition (while)
        add(.push, -1)
        if case .while = conditionalStatement.kind {
            add(.jump, unresolvedLabel: generateLabel("cond"))
        } else {
            add(.jump, unresolvedLabel: generateLabel("end"))
        }
        
        // 5. if this is an if statement w/ an else branch, handle the else branch
        if case .if(let elseBranch) = conditionalStatement.kind, let elseBranch_ = elseBranch {
            add(label: generateLabel("else"))
            handle(composite: elseBranch_)
        }
        
        // 6. handle the end label
        add(label: generateLabel("end"))
    }
    
    
    func handle(assignment: ASTAssignment) {
        guard let target = assignment.target as? ASTIdentifier else {
            fatalError("can only assign to variables as of right now")
        }
        
        handle(node: assignment.value)
        add(.store, scope.index(of: target.name))
    }
    
    
    
    
    
    // MARK: Handle Expressions
    
    func handle(functionCall: ASTFunctionCall) {
        guard let argc = functions[functionCall.functionName]?.argc else {
            fatalError("trying to call non-existent function")
        }
        
        guard argc == functionCall.arguments.count else {
            fatalError("wrong argc")
        }
        
        // todo push arguments on the stack
        for arg in functionCall.arguments.reversed() {
            handle(node: arg)
        }
        
        
        // push the address onto the stack
        if let builtin = Runtime.builtin(withName: functionCall.functionName) {
            add(.push, builtin.address)
        } else {
            add(.push, unresolvedLabel: SymbolMangling.mangleGlobalFunction(name: functionCall.functionName))
        }
        
        // call w/ the passed number of arguments
        add(.call, functionCall.arguments.count)
        
        if functionCall.unusedReturnValue {
            add(.pop)
        }
    }
    
    
    func handle(identifier: ASTIdentifier) {
        add(.load, scope.index(of: identifier.name))
    }
    
    func handle(memberGetter: ASTTypeMemberGetter) {
        let typename = scope.type(of: memberGetter.target.name)
        let membername = memberGetter.memberName.name
        
        guard typeCache.type(typename, hasMember: membername) else {
            fatalError("type '\(typename)' doesn't have member '\(membername)'")
        }
        
        let offset = typeCache.offset(ofMember: membername, inType: typename)
        add(.push, offset)
        handle(identifier: memberGetter.target)
        add(.loadh)
    }
    
    
    func handle(memberSetter: ASTTypeMemberSetter) {
        let typename = scope.type(of: memberSetter.target.name)
        let membername = memberSetter.memberName.name
        
        guard typeCache.type(typename, hasMember: membername) else {
            fatalError("type '\(typename)' doesn't have member '\(membername)'")
        }
        
        handle(node: memberSetter.newValue)
        
        let offset = typeCache.offset(ofMember: membername, inType: typename)
        add(.push, offset)
        
        handle(identifier: memberSetter.target)
        add(.storeh)
    }
    
    
    func handle(binop: ASTBinaryOperation) {
        handle(node: binop.rhs)
        handle(node: binop.lhs)
        
        add(binop.operation.operation)
    }
    
    func handle(unary: ASTUnary) {
        // TODO add support for NOT
        handle(binop: ASTBinaryOperation(lhs: ASTNumberLiteral(value: -1), operation: .mul, rhs: unary.expression))
    }
    
    
    func handle(numberLiteral: ASTNumberLiteral) {
        add(.push, numberLiteral.value)
    }
    
    func handle(stringLiteral: ASTStringLiteral) {
        let value = stringLiteral.value
        
        let codepoints: [Int] = value.unicodeScalars.map { Int($0.value) }
        
        let label = UUID().uuidString // TODO detect duplicate string literals
        instructions.append(.arrayLiteral(label, codepoints))
        
        add(.loadc, unresolvedLabel: label)
        
        let stringInitCall = ASTFunctionCall(
            functionName: SymbolMangling.mangleInitializer(forType: "String"),
            arguments: [ASTNoop()], // the parameter is already on the stack, from the `loadc` instruction above
            unusedReturnValue: false
        )
        
        handle(functionCall: stringInitCall)
    }
    
    
    func handle(arrayLiteral: ASTArrayLiteral) {
        // TODO if the array is just number literals, store it as a constant (like strings)
        // otherwise, just generate a bunch of Array.add calls? (that wouldn't work inline)
        
        let isConstant = arrayLiteral.elements.all { $0 is ASTNumberLiteral }
        
        if isConstant {
            let values = arrayLiteral.elements.map { ($0 as! ASTNumberLiteral).value }
            let label = UUID().uuidString
            instructions.append(.arrayLiteral(label, values))
            
            add(.loadc, unresolvedLabel: label)
            
            let initCall = ASTFunctionCall(
                functionName: SymbolMangling.mangleStaticMember(ofType: "Array", memberName: "_fromLiteral"),
                arguments: [ASTNoop()], // the parameter is already on the stack, from the `loadc` instruction above
                unusedReturnValue: false
            )
            handle(functionCall: initCall)
            return
        }
        
        fatalError("array literals for non-constant values not yet implemented")
    }
    
    
    
    func handle(condition: ASTCondition) {
        if let comparison = condition as? ASTComparison {
            handle(comparison: comparison)
            
        } else if let binaryCondition = condition as? ASTBinaryCondition {
            handle(binaryCondition: binaryCondition)
        } else {
            fatalError("unhandled condition \(condition)")
        }
    }
    
    
    func handle(comparison: ASTComparison) {
        handle(node: comparison.rhs)
        handle(node: comparison.lhs)
        
        switch comparison.operator {
        case .equal:
            add(.eq)
        case .notEqual:
            add(.eq)
            add(.not)
        case .less:
            add(.lt)
        case .greater:
            add(.le)
            add(.not)
        case .lessEqual:
            add(.le)
        case .greaterEqual:
            add(.lt)
            add(.not)
        }
    }
    
    
    func handle(binaryCondition: ASTBinaryCondition) {
        // evaluate both conditions (lhs and rhs), order doesn't matter
        handle(node: binaryCondition.lhs)
        handle(node: binaryCondition.rhs)
        
        // the last two values on the stack are now one of these options:
        // -1, -1   (lhs: true  | rhs: true )
        // -1,  0   (lhs: true  | rhs: false)
        //  0, -1   (lhs: false | rhs: true )
        //  0,  0   (lhs: false | rhs: false)
        
        // we now add the last two entries on the stack
        // if the result is -2, both are true
        // if the result is -1, one of them is true
        // if the result is  0, both are false
        
        let expectedResult = binaryCondition.operator == .and ? -2 : -1
        
        add(.add)
        add(.push, expectedResult)
        add(.eq)
    }
}
