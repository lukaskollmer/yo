//
//  BytecodeInterpreter.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class BytecodeInterpreter {
    private let stack = Stack<Int>(size: 12, initialValue: 0)
    private let instructions: [InstructionDescriptor]
    
    
    init(instructions: [Instruction]) {
        self.instructions = instructions.map(InstructionDescriptor.init)
    }
    
    
    /// Evaluate the instructions. Returns the last value on the stack (aka whetever `main` returned)
    func run() throws -> Int {
        var instructionPointer = 0
        
        while instructionPointer < instructions.count && instructionPointer != -1 {
            let instruction = instructions[instructionPointer]
            let previousInstructionPointer = instructionPointer
            
            try eval(instruction, &instructionPointer)
            
            if instructionPointer == previousInstructionPointer {
                instructionPointer += 1
            }
        }
        
        
        return try stack.pop()
    }
}


private extension BytecodeInterpreter {
    func eval(_ instruction: InstructionDescriptor, _ instructionPointer: inout Int) throws {
        let immediate = instruction.immediate
        
        Log.info("")
        Log.info("")
        Log.info("[eval] ip=\(instructionPointer) op=\(instruction.operation) imm=\(immediate)")
        Log.info("[eval] ip=\(instructionPointer) stack before: \(stack)")
        
        
        switch instruction.operation {
        case .noop:
            break
            
        case .add:
            try stack.push(try stack.pop() + stack.pop())
            
        case .sub:
            try stack.push(try stack.pop() - stack.pop())
        
        case .mul:
            try stack.push(try stack.pop() * stack.pop())
        
        case .div:
            try stack.push(try stack.pop() / stack.pop())
        
        case .mod:
            try stack.push(try stack.pop() % stack.pop())
            
        case .not:
            try stack.push(~(try stack.pop()))
            
        case .eq:
            try stack.push((try stack.pop() == stack.pop()) ? -1 : 0)
            
        case .lt:
            try stack.push((try stack.pop() < stack.pop()) ? -1 : 0)
            
        case .le:
            try stack.push((try stack.pop() <= stack.pop()) ? -1 : 0)
        
        
        case .push:
            try stack.push(immediate)
            
        case .load:
            try stack.push(stack.getFrameElement(atIndex: immediate))
            
        case .jump:
            if try stack.pop() == -1 {
                instructionPointer = immediate
            }
        
        case .call:
            let destinationInstructionPointer = try stack.pop()
            
            // TODO handle arguments
            var args = [Int]()
            for _ in 0..<immediate {
                args.append(try stack.pop())
            }
            
            try stack.push(stack.framePointer)
            try stack.push(instructionPointer + 1)
            
            try args.reversed().forEach { try stack.push($0) }
            
            stack.framePointer = stack.stackPointer
            instructionPointer = destinationInstructionPointer
        
            
        case .ret:
            let returnValue = try stack.pop()
            
            for _ in 0..<immediate {
                _ = try stack.pop()
            }
            
            instructionPointer = try stack.pop()
            stack.framePointer = try stack.pop()
            
            try stack.push(returnValue)
        }
        
        Log.info("[eval] ip=\(instructionPointer) stack after: \(stack)")
        
    }
}
