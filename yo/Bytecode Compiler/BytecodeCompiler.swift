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
    
    private enum WIPInstruction {
        case label(String)                  // A label
        case operation(Operation, Int)      // A "finalized" instruction
        case unresolved(Operation, String)  // An instruction that takes an address as parameter, which will be resolved later (after all codegen finished)
    }
    
    private var instructions = [WIPInstruction]()
    private var counter = 0
    
    // Scope info
    fileprivate typealias GlobalFunctionsInfo = [String: Int]
    private var scope = Scope(type: .global)
    private var globalFunctions = GlobalFunctionsInfo()
    
    
    
    init() {
        
        // fill the functions table w/ all native functions
        for nativeFunction in Runtime.builtins {
            globalFunctions[nativeFunction.key] = nativeFunction.value.argc
        }
    }
    
    
    
    func generateInstructions(for ast: [ASTNode]) -> [Instruction] {
        return _generateInstructions(for: ast).enumerated().map { index, instruction in
            switch instruction {
            case .operation(let operation, let immediate):
                return operation.encode(withImmediate: immediate)
            case .unresolved(let operation, let label):
                return operation.encode(withImmediate: getAddress(ofLabel: label))
            case .label(_):
                return 0
            }
        }
    }
    
    
    private func _generateInstructions(for ast: [ASTNode]) -> [WIPInstruction] {
        if case .global = scope.type {
            // add the bootstrapping instructions
            add(.noop) // push main, will be replaced later
            add(.call, 0)
            add(.push, -1)
            add(.jump, 0) // jump end, will be replaced later
        }
        
        for node in ast {
            handle(node: node)
        }
        
        if case .global = scope.type {
            // update the bootstrapping code
            add(label: "end")
            instructions[0] = .operation(.push, getAddress(ofLabel: "main"))
            instructions[3] = .operation(.jump, getAddress(ofLabel: "end"))
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
    
    func getAddress(ofLabel label: String) -> Int {
        return instructions.index { instruction in
            if case .label(let name) = instruction {
                return name == label
            }
            return false
        }!
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
            
        } else if let _ = node as? ASTNoop {
            
        } else {
            fatalError("unhandled node \(node)")
        }
        
    }
    
    
    
    // MARK: Handle Statements
    
    
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
        globalFunctions[function.mangledName] = function.parameters.count
        
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
            add(.push, getAddress(ofLabel: functionCall.functionName)) // TODO allow calling not yet declared functions
        }
        
        // call w/ the passed number of arguments
        add(.call, functionCall.arguments.count)
    }
    
    
    func handle(identifier: ASTIdentifier) {
        add(.load, scope.index(of: identifier.name))
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
}
