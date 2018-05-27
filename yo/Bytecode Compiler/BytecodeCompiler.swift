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
    private var counter = 0
    
    // Scope info
    fileprivate typealias GlobalFunctionsInfo = [String: Int]
    private var scope = Scope(type: .global)
    private var globalFunctions = GlobalFunctionsInfo()
    private var typeCache = TypeCache()
    
    
    
    init() {
        // fill the functions table w/ all native functions
        for nativeFunction in Runtime.builtins {
            globalFunctions[nativeFunction.key] = nativeFunction.value.argc
        }
    }
    
    
    
    func generateInstructions(for ast: [ASTNode], includeBootstrappingCode: Bool = true) throws -> [WIPInstruction] {
        if case .global = scope.type, includeBootstrappingCode {
            // add the bootstrapping instructions
            add(.push, unresolvedLabel: "main")
            add(.call, 0)
            add(.push, -1)
            add(.jump, unresolvedLabel: "end")
        }
        
        try preflight(ast: ast)
        
        for node in ast {
            handle(node: node)
        }
        
        if includeBootstrappingCode {
            add(label: "end")
        }
        
        return instructions
    }
    
}


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
}

private extension BytecodeCompiler {
    
    func preflight(ast: [ASTNode]) throws {
        for node in ast {
            if let functionDecl = node as? ASTFunctionDeclaration {
                preflight_register(function: functionDecl)
                
            } else if let typeDecl = node as? ASTTypeDeclaration {
                typeCache.register(type: typeDecl)
                
            } else if let typeImpl = node as? ASTTypeImplementation {
                for fn in typeImpl.functions {
                    preflight_register(function: fn)
                }
                
            } else if let importStatement = node as? ASTImportStatement {
                guard importStatement.isInternal else {
                    fatalError("atm only internal imports are supported")
                }
                
                
                let path = ImportPathResolver.resolve(moduleName: importStatement.moduleName)
                let compiler = BytecodeCompiler()
                
                let importedInstructions = try compiler.generateInstructions(for: try yo.parse(file: path), includeBootstrappingCode: false)
                instructions.append(contentsOf: importedInstructions)
                
                globalFunctions.insert(contentsOf: compiler.globalFunctions)
                compiler.typeCache.types.forEach(self.typeCache.register)
            }
        }
    }
    
    func preflight_register(function: ASTFunctionDeclaration) {
        globalFunctions[function.mangledName] = function.parameters.count
    }
    
    
    
    func handle(node: ASTNode) {
        
        if let function = node as? ASTFunctionDeclaration {
            handle(function: function)
            
        } else if let returnStatement = node as? ASTReturnStatement {
            handle(return: returnStatement)
            
        } else if let numberLiteral = node as? ASTNumberLiteral {
            handle(numberLiteral: numberLiteral)
            
        } else if let functionCall = node as? ASTFunctionCall {
            handle(functionCall: functionCall)
            
        } else if let binop = node as? ASTBinop {
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
            // imports are already processed in the prefligth phase
            //handle(importStatement: importStatement)
            
        } else if let _ = node as? ASTNoop {
            
        } else {
            fatalError("unhandled node \(node)")
        }
        
    }
    
    
    
    // MARK: Handle Statements
    
    
    func handle(typeDeclaration: ASTTypeDeclaration) {
        

        // generate an initializer
        let _self = ASTIdentifier(name: "self")
        let initializer = ASTFunctionDeclaration(
            name: ASTIdentifier(name: "alloc"),
            parameters: typeDeclaration.attributes,
            returnType: typeDeclaration.name,
            localVariables: [ASTVariableDeclaration(identifier: _self, typename: typeDeclaration.name)],
            body: [
                // 1. allocate space on the heap
                ASTAssignment(
                    target: _self,
                    value: ASTFunctionCall(
                        functionName: "alloc",
                        arguments: [ASTNumberLiteral(value: typeDeclaration.attributes.count)],
                        unusedReturnValue: false)
                ),
                // 2. go through the parameters and fill the attributes
                ASTComposite(
                    statements: typeDeclaration.attributes.enumerated().map { attribute in
                        return ASTArraySetter(target: _self, offset: ASTNumberLiteral(value: attribute.offset), value: attribute.element.identifier)
                    }
                ),
                
                // 3. return the newly created object
                ASTReturnStatement(returnValueExpression: _self)
            ],
            kind: .staticImpl(typeDeclaration.name.name)
        )
        
        preflight_register(function: initializer)
        
        handle(function: initializer)
    }
    
    
    func handle(function: ASTFunctionDeclaration) {
        guard function.localVariables.intersection(with: function.parameters).isEmpty else {
            fatalError("local variable cannot (yet?) shadow parameter")
        }
        
        // Save the current scope
        let previousScope = scope
        
        // Update the scope. This is important because we need knowledge about the function's
        // parameters and local variables when generating instructions for the function body
        scope = Scope(type: .function(function.mangledName), parameters: function.parameters, localVariables: function.localVariables)
        
        // function entry point
        add(label: function.mangledName)
        
        // allocate space for local variables?
        let numberOfLocalVariables = scope.size - function.parameters.count
        add(.alloc, numberOfLocalVariables)
        
        // Generate instructions for the function body
        function.body.forEach(handle)
        
        // Restore the old scope
        scope = previousScope
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
        
        let _counter = counter.advanced(by: 1)
        let generateLabel: (String) -> String = { "\(functionName)_ifwhile_\(_counter)_\($0)" } // TOOD replace `ifwhile` w/ just if or while?
        
        
        
        
        // 1. handle the condition
        // we only need a label for the condition if this is a while statement
        if case .while = conditionalStatement.kind {
            add(label: generateLabel("cond"))
        }
        handle(condition: conditionalStatement.condition)
        
        // 2. handle the jump to the body if the condition is true
        // if the condition is false, we fall through to the else branch (or the end, if there is no else branch)
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
        guard let argc = globalFunctions[functionCall.functionName] else {
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
        if let builtin = Runtime.builtins[functionCall.functionName] {
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
    
    
    func handle(binop: ASTBinop) {
        handle(node: binop.rhs)
        handle(node: binop.lhs)
        
        add(binop.operator.operation)
    }
    
    func handle(unary: ASTUnary) {
        handle(binop: ASTBinop(lhs: ASTNumberLiteral(value: -1), operator: .mul, rhs: unary.expression))
    }
    
    
    func handle(numberLiteral: ASTNumberLiteral) {
        add(.push, numberLiteral.value)
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
