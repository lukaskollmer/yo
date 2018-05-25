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
    
    private var instructions = [Instruction]()
    
    // Scope info
    fileprivate typealias GlobalFunctionsInfo = [String: (address: Int, argc: Int)]
    private var scope = Scope(type: .global)
    private var globalFunctions = GlobalFunctionsInfo()
    
    init() {
    }
    
    fileprivate init(parent: BytecodeCompiler) {
        self.scope = parent.scope
        self.globalFunctions = parent.globalFunctions
    }
    
    fileprivate init(function: ASTFunctionDeclaration, globalFunctions: GlobalFunctionsInfo) {
        self.scope = Scope(type: .function(function.mangledName), parameters: function.parameters, localVariables: function.localVariables)
        self.globalFunctions = globalFunctions
    }
    
    
    func generateInstructions(for ast: [ASTNode]) -> [Instruction] {
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
            add(.noop) // end label
            instructions[0] = Operation.push.encode(withImmediate: globalFunctions["main"]!.address)
            instructions[4] = Operation.jump.encode(withImmediate: instructions.count - 1)
        }
        
        
        return instructions
    }
    
}


private extension BytecodeCompiler {
    
    func add(_ operation: Operation, _ immediate: Int = 0) {
        instructions.append(operation.encode(withImmediate: immediate))
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
            
        } else if let noop = node as? ASTNoop {
            
        } else {
            fatalError("unhandled node \(node)")
        }
        
    }
    
    
    
    // MARK: Handle Statements
    
    
    func handle(function: ASTFunctionDeclaration) {
        // we create a new codegen for the function body
        let codegen = BytecodeCompiler(function: function, globalFunctions: self.globalFunctions)
        
        // function entry point
        add(.noop)
        globalFunctions[function.mangledName] = (instructions.count, function.parameters.count)
        
        // arguments + local variables // TODO // TODO what does the comment mean?
        
        
        // function body
        instructions.append(contentsOf: codegen.generateInstructions(for: function.body))
    }
    
    
    func handle(return returnStatement: ASTReturnStatement) {
        handle(node: returnStatement.returnValueExpression)
        add(.ret, scope.size)
    }
    
    
    
    
    
    
    // MARK: Handle Expressions
    
    func handle(functionCall: ASTFunctionCall) {
        guard let (address, argc) = globalFunctions[functionCall.functionName] else {
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
        add(.push, address)
        
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
    
    
    func handle(numberLiteral: ASTNumberLiteral) {
        add(.push, numberLiteral.value)
    }
}
