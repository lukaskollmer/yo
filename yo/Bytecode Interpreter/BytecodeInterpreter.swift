//
//  BytecodeInterpreter.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class BytecodeInterpreter {
    private let heap = Heap(size: 1 << 8)
    private let instructions: [InstructionDescriptor]
    
    private var stack: Stack {
        return heap.stack
    }
    
    
    init(instructions: [Instruction]) {
        self.instructions = instructions.map(InstructionDescriptor.init)
    }
    
    
    /// Evaluate the instructions. Returns the last value on the stack (aka whetever `main` returned)
    func run() throws -> Int {
        var instructionPointer = 0
        
        while instructionPointer < instructions.count && instructionPointer != -1 { // -1 is HALT (TODO implement)
            
            // check whether we're calling a native function (native functions have a negative "virtual" address)
            if instructionPointer < 0 {
                let nativeFunction = Runtime.getNativeFunction(withAddress: instructionPointer)
                if nativeFunction.name == SymbolMangling.mangleStaticMember(ofType: "runtime", memberName: "dealloc") {
                    
                    // call the object's dealloc function
                    // a negative destination address indicates that the type does not have a deallc function
                    let destinationAddress = heap[stack.peek()] >> 40
                    if destinationAddress > 0 {
                        try eval(InstructionDescriptor(instruction: Operation.push.encode(withImmediate: -1)), &instructionPointer)
                        try eval(InstructionDescriptor(instruction: Operation.jump.encode(withImmediate: destinationAddress)), &instructionPointer)
                    }
                } else {
                    
                    try stack.push(nativeFunction.imp(stack))
                    // return from the native function
                    try eval(InstructionDescriptor(instruction: Operation.ret.encode(withImmediate: nativeFunction.argc)), &instructionPointer)
                }
                
                
            } else {
                let instruction = instructions[instructionPointer]
                let previousInstructionPointer = instructionPointer
                
                try eval(instruction, &instructionPointer)
                
                if instructionPointer == previousInstructionPointer {
                    instructionPointer += 1
                }
            }
            
        }
        
        
        print("heap after: \(heap.backing)")
        
        
        return try stack.pop()
    }
}


extension BytecodeInterpreter {
    func eval(_ instruction: InstructionDescriptor, _ instructionPointer: inout Int) throws {
        let immediate = instruction.immediate
        
        //Log.info("")
        //Log.info("")
        //Log.info("[eval] ip=\(instructionPointer) op=\(instruction.operation) imm=\(immediate)")
        //Log.info("[eval] ip=\(instructionPointer) stack before: \(stack)")
        
        
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
            
            
        case .and:
            try stack.push(try stack.pop() & stack.pop())
            
        case .or:
            try stack.push(try stack.pop() | stack.pop())
        
        case .xor:
            try stack.push(try stack.pop() ^ stack.pop())
        
        case .shl:
            try stack.push(try stack.pop() << stack.pop())
            
        case .shr:
            try stack.push(try stack.pop() >> stack.pop())
            
            
        case .not:
            try stack.push(~(try stack.pop()))
            
            
        case .eq:
            try stack.push((try stack.pop() == stack.pop()) ? -1 : 0)
            
        case .lt:
            try stack.push((try stack.pop() < stack.pop()) ? -1 : 0)
            
        case .le:
            try stack.push((try stack.pop() <= stack.pop()) ? -1 : 0)
        
        
        // Stack operations
        case .alloc:
            for _ in 0..<immediate {
                try stack.push(0)
            }
        
        case .push:
            try stack.push(immediate)
            
        case .pop:
            _ = try stack.pop()
            
        case .load:
            try stack.push(stack.getFrameElement(atIndex: immediate))
            
        case .store:
            stack.pushFrame(index: immediate, value: try stack.pop())
            
            
        // Heap operations
        case .loadh:
            let address = try stack.pop()
            let offset  = try stack.pop()
            try stack.push(heap[address + offset])
            
        case .storeh:
            let address = try stack.pop()
            let offset  = try stack.pop()
            let value   = try stack.pop()
            heap[address + offset] = value
            
            // TODO get the old value, release it if it's a pointer pointing to somewhere on the heap
            // TODO retain the new value, if it's a pointer pointing to somewhere on the heap
            
            
        case .loadc: // load constant
            let size = instructions[immediate + 1].immediate + 1 // +1 bx we include the size in the heap array
            let address = heap.alloc(size: size)
            // TODO retain address? (prob only once since we load constants new every time)
            
            for i in 0..<size {
                let value = instructions[immediate + 1 + i].immediate
                heap[address + i] = value
            }
            
            try! stack.push(address)
            
        
            
        // jump/call/ret
        case .jump:
            if try stack.pop() == -1 {
                instructionPointer = immediate
            }
        
        case .call:
            let destinationInstructionPointer = try stack.pop()
            
            var args = [Int]()
            for _ in 0..<immediate {
                args.append(try stack.pop())
            }
            
            try stack.push(stack.framePointer)
            try stack.push(instructionPointer + 1)
            
            try args.reversed().forEach {
                try stack.push($0)
                // TODO if $0 is an address pointing to somewhere on the heap, retain it
            }
            
            stack.framePointer = stack.stackPointer
            instructionPointer = destinationInstructionPointer
            
        case .ret:
            let returnValue = try stack.pop()
            // TODO if returnValue is an address pointing to somewhere on the heap, retain it
            
            for _ in 0..<immediate {
                _ = try stack.pop()
                // TODO if _ is an address pointing to somewhere on the heap, retain it
            }
            
            instructionPointer = try stack.pop()
            stack.framePointer = try stack.pop()
            
            try stack.push(returnValue)
        }
        
        //Log.info("[eval] ip=\(instructionPointer) stack after: \(heap.backing)")
    }
}
