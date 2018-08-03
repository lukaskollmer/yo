//
//  BytecodeInterpreter.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class BytecodeInterpreter {
    static let verboseLogging = false
    
    let heap: Heap
    let instructions: [InstructionDescriptor]
    let procedureEntryAddresses: [Int: String]
    
    private var instructionPointer = 0  // plz don't use this directly
    private var callStack = [Int]()     // function entry points
    
    
    var stack: Stack {
        return heap.stack
    }
    
    
    init(wipInstructions: [WIPInstruction], heapSize: Int) {
        self.heap = Heap(size: heapSize)
        let finalizedInstructions    = wipInstructions.finalized()
        self.instructions            = finalizedInstructions.instructions.map(InstructionDescriptor.init)
        self.procedureEntryAddresses = finalizedInstructions.procedureEntryAddresses
    }
    
    
    /// Evaluate the instructions. Returns the last value on the stack (aka whetever `main` returned)
    func run() throws -> Int {
        
        while instructionPointer < instructions.count && instructionPointer != -1 { // -1 is HALT (TODO implement)
            let previousInstructionPointer = instructionPointer
            
            try performAtCurrentInstructionPointer()
            
            if instructionPointer == previousInstructionPointer {
                instructionPointer += 1
            }
        }
        
        return try stack.pop()
    }
    
    
    
    private func performAtCurrentInstructionPointer() throws {
        // check whether we're calling a native function (native functions have a negative "virtual" address)
        if instructionPointer < 0 {
            let nativeFunction = Runtime.shared[address: instructionPointer]
            try stack.push(nativeFunction.imp(self))
            // return from the native function
            try eval(InstructionDescriptor(instruction: Operation.ret.encode(withImmediate: nativeFunction.info.argc)))
            
        } else {
            let instruction = instructions[instructionPointer]
            try eval(instruction)
        }
    }
    
    
    
    func call(address: Int, arguments: [Int]) throws -> Int {
        // simulating a function call
        // this is problematic for several reasons:
        // - after the function call, the stack has to be in the *exact* same state as before
        // - we need to somehow detect when we're returning from the function call (might get difficult when the called function calls other functions
        
        var address = address
        var arguments = arguments
        
        if address > 0 && address.isEven {
            arguments.insert(address, at: 0)
            address = heap[address + 1]
        }
        
        try stack.push(stack.framePointer)
        try stack.push(instructionPointer + 0)  // return address. since this is a native call, we return to the same address?
        
        //try arguments/*.reversed()*/.forEach(stack.push)
        try arguments.reversed().forEach(stack.push)
        
        stack.framePointer = stack.stackPointer
        instructionPointer = address
        
        var depth = 1
        
        while true {
            if (0..<instructions.count).contains(instructionPointer) {
                let prevImmediate = instructions[instructionPointer - 1].immediate
                let next = instructions[instructionPointer]
                
                if next.operation == .call && prevImmediate > 0 {
                    depth += 1
                }
                if next.operation == .ret {
                    depth -= 1
                }
                
                if depth == 0 {
                    let returnValue = try stack.pop()
                    
                    for _ in 0..<next.immediate {
                        _ = try stack.pop()
                    }
                    
                    instructionPointer = try stack.pop()
                    stack.framePointer = try stack.pop()
                    
                    // TODO run some assertion to make sure the stack is unchanged?
                    return returnValue
                }
            }
            
            let previousIP = instructionPointer
            try performAtCurrentInstructionPointer()
            if previousIP == instructionPointer {
                instructionPointer += 1
            }
        }
        fatalError("should not reach here")
    }
    
    
    func eval(_ instruction: InstructionDescriptor) throws {
        let immediate = instruction.immediate
        
        if BytecodeInterpreter.verboseLogging {
            Log.info("")
            Log.info("")
            Log.info("[eval] ip=\(instructionPointer) op=\(instruction.operation) imm=\(immediate)")
            //Log.info("[eval] ip=\(instructionPointer) stack before: \(stack)")
        }
        
        
        switch instruction.operation {
        case .noop:
            break
            
        // Arithmetic Operations
        case .add:
            try stack.push(try stack.pop() + stack.pop())
            
        case .d_add:
            try stack.push((try stack.pop().unsafe_loadAsDouble + stack.pop().unsafe_loadAsDouble).unsafe_loadAsInt)
            
            
        case .sub:
            try stack.push(try stack.pop() - stack.pop())
            
        case .d_sub:
            try stack.push((try stack.pop().unsafe_loadAsDouble - stack.pop().unsafe_loadAsDouble).unsafe_loadAsInt)
            
        
        case .mul:
            try stack.push(try stack.pop() * stack.pop())
            
        case .d_mul:
            try stack.push((try stack.pop().unsafe_loadAsDouble * stack.pop().unsafe_loadAsDouble).unsafe_loadAsInt)
            
        
        case .div:
            try stack.push(try stack.pop() / stack.pop())
            
        case .d_div:
            try stack.push((try stack.pop().unsafe_loadAsDouble / stack.pop().unsafe_loadAsDouble).unsafe_loadAsInt)
            
        
        case .mod:
            try stack.push(try stack.pop() % stack.pop())
            
            
        // Bitwise Operations
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
            
        case .lnot:
            try stack.push(try stack.pop() == Constants.BooleanValues.true ? Constants.BooleanValues.false : Constants.BooleanValues.true)
            
            
        // Int <-> Double conversion
        case .cvti2d:
            try stack.push(Double(try stack.pop()).unsafe_loadAsInt)
            
        case .cvtd2i:
            try stack.push(Int(try stack.pop().unsafe_loadAsDouble))
            
            
        // Comparisons
        case .eq:
            try stack.push((try stack.pop() == stack.pop()) ? Constants.BooleanValues.true : Constants.BooleanValues.false)
            
        case .lt:
            try stack.push((try stack.pop() < stack.pop()) ? Constants.BooleanValues.true : Constants.BooleanValues.false)
            
        case .le:
            try stack.push((try stack.pop() <= stack.pop()) ? Constants.BooleanValues.true : Constants.BooleanValues.false)
        
        
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
            
            
        case .loadc: // load constant
            let size = instructions[immediate + 1].immediate + 1 // +1 bx we include the size in the heap array
            let address = heap.alloc(size: size)
            
            for i in 0..<size {
                let value = instructions[immediate + 1 + i].immediate
                heap[address + i] = value
            }
            
            try! stack.push(address)
            
            
        case .readh:
            try stack.push(heap[immediate])
            
        case .writeh:
            heap[immediate] = try stack.pop()
            
        
            
        // jump/call/ret
        case .jump:
            if try stack.pop() == Constants.BooleanValues.true {
                instructionPointer = immediate
            }
        
        case .call:
            var destinationInstructionPointer = try stack.pop()
            
            var args = [Int]()
            for _ in 0..<immediate {
                args.append(try stack.pop())
            }
            
            if destinationInstructionPointer > 0 && destinationInstructionPointer.isEven {
                args.insert(destinationInstructionPointer, at: 0)
                destinationInstructionPointer = heap[destinationInstructionPointer + 1]
            }
            
            try stack.push(stack.framePointer)
            try stack.push(instructionPointer + 1)
            
            //try args/*.reversed()*/.forEach(stack.push)
            try args.reversed().forEach(stack.push)
            
            stack.framePointer = stack.stackPointer
            instructionPointer = destinationInstructionPointer
            
            callStack.append(destinationInstructionPointer)
            
        case .ret:
            let returnValue = try stack.pop()
            
            for _ in 0..<immediate {
                _ = try stack.pop()
            }
            
            instructionPointer = try stack.pop()
            stack.framePointer = try stack.pop()
            
            try stack.push(returnValue)
            
            callStack.removeLast()
            
            
        case .debug:
            
            // print current call stack
            print("Current Call Stack:")
            for (index, address) in callStack.reversed().enumerated() {
                var entry = ""
                entry += "\(index)".padding(.left, toLength: 4, withPad: " ").padding(.right, toLength: 7, withPad: " ")
                entry += "\(address)".padding(.left, toLength: 5, withPad: "0")
                entry += " " + String(procedureEntryAddresses[address] ?? "(unknown)")
                print(entry)
            }
            
            
            print()
        }
        
        if BytecodeInterpreter.verboseLogging {
            Log.info("[eval] ip=\(instructionPointer) stack after: \(heap)")
        }
    }
}
