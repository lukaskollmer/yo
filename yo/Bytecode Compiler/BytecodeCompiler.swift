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
        case label(String)
        case operation(Operation, Int)
    }
    
    private var instructions = [WIPInstruction]()
    
    // Scope info
    fileprivate typealias GlobalFunctionsInfo = [String: Int]
    private var scope = Scope(type: .global)
    private var globalFunctions = GlobalFunctionsInfo()
    
    init() {
    }
    
    func generateInstructions(for ast: [ASTNode]) -> [Instruction] {
        return _generateInstructions(for: ast).enumerated().map { (index: Int, instruction: WIPInstruction) -> Instruction in
            switch instruction {
            case .operation(let operation, let immediate):
                return operation.encode(withImmediate: immediate)
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
            add(.noop)
            add(.push, -1)
            add(.jump, 0) // jump end, will be replaced later
        }
        
        for node in ast {
            handle(node: node)
        }
        
        if case .global = scope.type {
            // update the bootstrapping code
            add("end")
            instructions[0] = .operation(.push, getAddress(ofLabel: "main"))
            instructions[4] = .operation(.jump, getAddress(ofLabel: "end"))
        }
        
        
        return instructions
    }
    
}


private extension BytecodeCompiler {
    
    func add(_ operation: Operation, _ immediate: Int = 0) {
        instructions.append(.operation(operation, immediate))
    }
    
    func add(_ label: String) {
        instructions.append(.label(label))
    }
    
    func getAddress(ofLabel label: String) -> Int {
        print(instructions)
        return instructions.index { instruction -> Bool in
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
            
        } else if let noop = node as? ASTNoop {
            
        } else {
            fatalError("unhandled node \(node)")
        }
        
    }
    
    
    
    // MARK: Handle Statements
    
    
    func handle(function: ASTFunctionDeclaration) {
        // Save the current scope
        let previousScope = scope
        
        // Update the scope. This is important because we need knowledge about the function's
        // parameters and local variables when generating instructions for the function body
        scope = Scope(type: .function(function.mangledName), parameters: function.parameters, localVariables: function.localVariables)
        
        // function entry point
        add(function.mangledName)
        globalFunctions[function.mangledName] = function.parameters.count
        
        // allocate space for local variables?
        
        // Generate instructions for the function body
        function.body.forEach(handle)
        
        // Restore the old scope
        scope = previousScope
    }
    
    
    func handle(return returnStatement: ASTReturnStatement) {
        handle(node: returnStatement.returnValueExpression)
        add(.ret, scope.size)
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
        add(.push, getAddress(ofLabel: functionCall.functionName)) // TODO allow calling not yet declared functions
        
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
}
