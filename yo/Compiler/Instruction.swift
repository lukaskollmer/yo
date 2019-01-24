//
//  Instruction.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


typealias Instruction = Int
typealias Immediate = Int32

// An instruction is a 64-bit wide integer, with the opcode stored in the lowest byte, and the immediate stored in the upper 7 bytes


// Wrapper struct that extracts data from an instruction
struct InstructionDescriptor: CustomStringConvertible {
    let instruction: Instruction
    
    var opcode: Operation.RawValue {
        return Operation.RawValue(truncatingIfNeeded: instruction) & Operation.RawValue.max
    }
    
    var operation: Operation {
        return Operation(rawValue: opcode)!
    }
    
    var immediate: Int {
        return instruction >> Operation.bitWidth
    }
    
    var description: String {
        return "<InstructionDescriptor op=\(operation) immediate=\(immediate)>"
    }
}


enum Operation: UInt8, NameInitializable {
    static let bitWidth = RawValue.bitWidth
    
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
    
    // floating point conversions
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
    case push64 // push the entire contents of the next instruction onto the stack
    case pop    // pop a value off the stack and discard it
    case popi   // pop {immediate} values off the stack and discard them
    case load   // copies the value in the frame at the index specified by the immediate onto the stack
    case store  // pops a value off the stack and stores it in the frame, at the index specified by the immediate
    
    
    // heap operations
    case loadh_8, loadh_16, loadh_32, loadh_64      // read a value from an array allocated on the heap
    case storeh_8, storeh_16, storeh_32, storeh_64  // write a value to an array allocated on the heap
    case loadc  // load the array constant starting at immediate into a heap array and push the address onto the heap
    
    case readh  // read a value from the address in the immediate
    case writeh // write a value at the address in the immediate
    
    case jump   // conditional jump
    case ujump  // unconditional jump
    case call
    case ret
    
    // ARC
    case retain
    case release
    
    case label      // noop, used for manually specifying labels in the source code
    case comment    // noop, used for manually specifying asm comments in the source code
    
    // debugging
    case debug
    
    
    
    var opcode: RawValue {
        return self.rawValue
    }
    
    
    func encode(withImmediate immediate: Int = 0) -> Int {
        // TODO pretty sure this is wrong
        guard immediate.magnitude.leadingZeroBitCount > Operation.bitWidth else {
            fatalError("value \(immediate) cannot be represented as an immediate")
        }
        return (immediate << Operation.bitWidth) | Int(self.rawValue)
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
