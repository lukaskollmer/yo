//
//  Instruction.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


typealias Instruction = Int
let InstructionSize = MemoryLayout<Instruction>.size * 8
let ImmediateSize = InstructionSize - 8 // It's probably best to avoid using the sign bit
// An instruction is an integer, with the following layout:
// The lower 7 bits are used to encode the opcode and the upper 57 to encode the immediate

// Wrapper struct that extracts data from an instruction
// TODO maybe find a better name?
struct InstructionDescriptor: CustomStringConvertible {
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
    
    var description: String {
        return "<InstructionDescriptor op=\(operation) immediate=\(immediate)>"
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
    
    case d_add
    case d_sub
    case d_mul
    case d_div
    
    // bitwise operations
    case and
    case or
    case xor
    case shl
    case shr
    
    case not    // ~<expr>  (bitwise not)
    case lnot   // !<expr>  (logical not)
    
    // floating point support // TODO
    case cvti2d
    case cvtd2i
    
    // comparisons
    case eq
    case lt
    case le
    
    case d_eq
    case d_lt
    case d_le
    
    // stack operations
    case alloc  // reserve stack space for local variables
    case push   // push the immediate onto the stack
    case pop    // pop a value off the stack and discard it
    case load   // copies the value in the frame at the index specified by the immediate onto the stack
    case store  // pops a value off the stack and stores it in the frame, at the index specified by the immediate
    
    // heap operations
    case loadh  // read a value from an array allocated on the heap
    case storeh // write a value to an array allocated on the heap
    case loadc  // load the array constant starting at immediate into a heap array and push the address onto the heap
    
    case readh  // read a value from the address in the immediate
    case writeh // write a value at the address in the immediate
    
    case jump
    case call
    case ret
    
    case label      // noop, used for manually specifying labels in the source code
    case comment    // noop, used for manually specifying asm comments in the source code
    
    // debugging
    case debug
    
    
    func encode(withImmediate immediate: Int = 0) -> Int {
        // TODO guard that the immediate fits in `InstructionSize - 7`
        return (immediate << 7) | self.rawValue
    }
    
    init?(name: String) {
        var i = 0
        while let item = Operation(rawValue: i) {
            if String(describing: item) == name {
                self = item
                return
            }
            i += 1
        }
        
        return nil
    }
    
    
    var doubleVariant: Operation {
        switch self {
        case .add: return .d_add
        case .sub: return .d_sub
        case .mul: return .d_mul
        case .div: return .d_div
        default: fatalError("operation \(self) does not have a double variant")
        }
    }
}
