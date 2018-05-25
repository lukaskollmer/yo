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
    
    private let ast: [ASTNode]
    private var instructions = [Instruction]()
    
    private var functionAddresses = [String: Int]()
    
    init(ast: [ASTNode]) {
        self.ast = ast
    }
    
    func generateInstructions() -> [Instruction] {
        // add the bootstrapping instructions
        add(.noop) // push main, will be replaced later
        add(.call, 0)
        add(.noop)
        add(.load, -1)
        add(.jump, 0) // jump end, will be replaced later
        
        for topLevelNode in ast {
            handle(node: topLevelNode)
        }
        
        
        // update the bootstrapping code
        add(.noop) // end label
        instructions[0] = Operation.load.encode(withImmediate: functionAddresses["main"]!)
        instructions[4] = Operation.jump.encode(withImmediate: instructions.count - 1)
        
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
            
        } else if let noop = node as? ASTNoop {
            
        } else {
            fatalError("unhandled node \(node)")
        }
        
    }
    
    
    func handle(function: ASTFunctionDeclaration) {
        // function entry point
        functionAddresses[function.mangledName] = instructions.count
        add(.noop)
        
        // arguments + local variables // TODO
        
        
        // function body
        function.body.forEach(handle) // this will always call `handle(node:)`
    }
    
    
    func handle(return returnStatement: ASTReturnStatement) {
        handle(node: returnStatement.returnValueExpression)
        add(.ret, 1) // TODO calculate the correct immediate
    }
    
    
    func handle(numberLiteral: ASTNumberLiteral) {
        add(.load, numberLiteral.value)
    }
    
    
    func handle(functionCall: ASTFunctionCall) {
        guard let address = functionAddresses[functionCall.functionName] else {
            fatalError("trying to call non-existent function")
        }
        
        // todo push arguments on the stack
        
        // push the address onto the stack
        add(.load, address)
        
        // call w/ the passed number of arguments
        add(.call, functionCall.arguments.count)
    }
}
