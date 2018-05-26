//
//  Instruction.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


typealias Instruction = Int
let InstructionSize = MemoryLayout<Instruction>.size
// An instruction is an integer, with the following layout:
// The lower 7 bits are used to encode the opcode and the upper 57 to encode the immediate

// Wrapper struct that extracts data from an instruction
// TODO maybe find a better name?
struct InstructionDescriptor {
    let instruction: Instruction
    
    var opcode: Int {
        return instruction & 0b1111111
    }
    
    var operation: Operation {
        return Operation(rawValue: opcode)!
    }
    
    var immediate: Int {
        return instruction >> 7
    }
}


enum Operation: Int {
    case noop
    
    // arithmetic operations
    case add
    case sub
    case mul
    case div
    case mod
    
    case not
    
    // comparisons
    case eq
    case lt
    case le
    
    // stack operations
    case alloc  // reserve stack space for local variables
    case push   // push the immediate onto the stack
    case load   // copies the value in the frame at the index specified by the immediate onto the stack
    case store
    
    // heap operations
    case alloch // allocate space on the heap
    
    case jump
    case call
    case ret
    
    
    func encode(withImmediate immediate: Int = 0) -> Int {
        // TODO guard that the immediate fits in `InstructionSize - 7`
        return (immediate << 7) | self.rawValue
    }
}


// String + padding
extension String {
    
    enum PadDirection {
        case left, right
    }
    
    func padding<T: StringProtocol>(_ padDirection: PadDirection, toLength length: Int, withPad pad: T) -> String {
        switch padDirection {
        case .right:
            return self.padding(toLength: length, withPad: pad, startingAt: 0)
        case .left:
            return String.init(repeating: pad as! String, count: length - self.count) + self
        }
    }
}


extension Array where Element == Instruction {
    var fancyDescription: String {
        var desc = [String]()
        
        for (idx, element) in self.enumerated() {
            let instruction = InstructionDescriptor(instruction: element)
            
            let lineNumber = String(describing: idx).padding(.left, toLength: 3, withPad: "0")
            let operation = String(describing: instruction.operation).padding(.right, toLength: 7, withPad: " ")
            desc.append("  [\(lineNumber)] \(operation) \(instruction.immediate)")
        }
        
        return desc.joined(separator: "\n")
    }
}
