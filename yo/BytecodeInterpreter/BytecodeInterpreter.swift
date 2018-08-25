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
    
    private let logAllCalls = CLI.hasFlag(.logAllCalls)
    
    
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
            let nativeFunction = Runtime.shared[address: instructionPointer]! // TODO the subscript returns an IOU, no idea why the force unwrap is still necessary tbh
            try stack.push(nativeFunction.imp(self))
            // return from the native function
            try eval(InstructionDescriptor(instruction: Operation.ret.encode(withImmediate: nativeFunction.info.argc)))
            
        } else {
            let instruction = instructions[instructionPointer]
            try eval(instruction)
        }
    }
    
    
    
    func call(address: Int, arguments: [Int]) throws -> Int {
        //print("INIT", stack)
        if address == 0 {
            fatalError("fuckin hell")
        }
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
        let designatedReturnAddress = instructionPointer + 0 // return address. since this is a native call, we return to the same address?
        try stack.push(designatedReturnAddress)

        
        // We have to push the arguments onto the stack *in reverse order*
        // Why? When handling a normal function call, we go through the arguments in order (ie left-to-right)
        // and call the `handle` function for each argument. That means that they will end up on the stack in reverse order
        // (the last argument is evaluated last and therefore pushed onto the stack last and therefore the top argument on the stack)
        try arguments.reversed().forEach(stack.push)
        
        stack.framePointer = stack.stackPointer
        instructionPointer = address
        
        let previousFramePointer = stack.framePointer
        
        recordCallEvent(.entry(address))
        
        // NOTE: there are 2 obvious options for detecting when the function we're simulating a call to returns:
        // 1. keep track of the current "depth": all function calls from w/in the called function increment the depth, all returns decrement it
        //      Problem: it's difficult to get this right (there's a more or less working version somewhere in the git history)
        // 2. remember the return address and check whether we're about to return to that address
        //      Problem: what if one of the "nested" function calls is to out callee, in which case the return address would be the same?
        //      We solve this by also keeping track of the frame pointer, to make sure we're returning from the same function invocation
        
        while true {
            if (0..<instructions.count).contains(instructionPointer) {
                let next = instructions[instructionPointer]
                
                let isReturningFromInitialFunctionCall = next.operation == .ret
                        && stack.peek(offset: -(next.immediate + 1)) == designatedReturnAddress
                        && previousFramePointer == stack.framePointer
                
                if isReturningFromInitialFunctionCall {
                    let returnValue = try stack.pop()
                    
                    for _ in 0..<next.immediate {
                        _ = try stack.pop()
                    }
                    
                    instructionPointer = try stack.pop()
                    stack.framePointer = try stack.pop()
                    
                    recordCallEvent(.exit)
                    
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
    
    
    
    private func call<T, R, U>(_ fn: (T, T) -> R, arg0: U, arg1: U) -> R {
        return withoutActuallyEscaping(fn) {
            let fn = unsafeBitCast($0, to: ((U, U) -> R).self)
            return fn(arg0, arg1)
        }
    }
    
    
    
    private func eval_binop<T>(type: T.Type, fn: (T, T) -> T) throws {
        // rhs first because lhs is evaluated first, meaning that rhs lies before lhs on the stack
        let rhs = try stack.pop()
        let lhs = try stack.pop()
        
        if type == Int.self {
            let result = call(fn, arg0: lhs, arg1: rhs) as! Int
            try stack.push(result)
            
        } else if type == Double.self {
            let result = call(fn, arg0: lhs.unsafe_loadAsDouble, arg1: rhs.unsafe_loadAsDouble) as! Double
            try stack.push(result.unsafe_loadAsInt)
            
        } else {
            fatalError("Only Int and Double are supported types!")
        }
    }
    
    
    private func eval_comp<T>(type: T.Type, fn: (T, T) -> Bool) throws {
        let rhs = try stack.pop()
        let lhs = try stack.pop()
        
        let result: Bool
        
        if type == Int.self {
            result = call(fn, arg0: lhs, arg1: rhs)
            
        } else if type == Double.self {
            result = call(fn, arg0: lhs.unsafe_loadAsDouble, arg1: rhs.unsafe_loadAsDouble)
        
        } else {
            fatalError("Unsupported type!")
        }
        
        try stack.push(result == true ? Constants.BooleanValues.true : Constants.BooleanValues.false)
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
        case .noop, .label, .comment:
            break
            
        // Arithmetic Operations
        case .add:
            try eval_binop(type: Int.self, fn: +)
            
        case .d_add:
            try eval_binop(type: Double.self, fn: +)
            
            
        case .sub:
            try eval_binop(type: Int.self, fn: -)
        
        case .d_sub:
            try eval_binop(type: Double.self, fn: -)
            
        
        case .mul:
            try eval_binop(type: Int.self, fn: *)
            
        case .d_mul:
            try eval_binop(type: Double.self, fn: *)
        
        case .div:
            try eval_binop(type: Int.self, fn: /)
            
        case .d_div:
            try eval_binop(type: Double.self, fn: /)
        
        case .mod:
            try eval_binop(type: Int.self, fn: %)
            
            
        // Bitwise Operations
        case .and:
            try eval_binop(type: Int.self, fn: &)
            
        case .or:
            try eval_binop(type: Int.self, fn: |)
        
        case .xor:
            try eval_binop(type: Int.self, fn: ^)
        
        case .shl:
            try eval_binop(type: Int.self, fn: <<)
            
        case .shr:
            try eval_binop(type: Int.self, fn: >>)
            
            
        case .not:
            try stack.push(~(try stack.pop()))
            
        case .lnot:
            try stack.push(try stack.pop() != 0 ? 0 : 1)
            
            
        // Int <-> Double conversion
        case .cvti2d:
            try stack.push(Double(try stack.pop()).unsafe_loadAsInt)
            
        case .cvtd2i:
            try stack.push(Int(try stack.pop().unsafe_loadAsDouble))
            
            
        // Comparisons
        case .eq:
            try eval_comp(type: Int.self, fn: ==)
            
        case .lt:
            try eval_comp(type: Int.self, fn: <)
            
        case .le:
            try eval_comp(type: Int.self, fn: <=)
            
            
        case .d_eq:
            try eval_comp(type: Double.self, fn: ==)
            
        case .d_lt:
            try eval_comp(type: Double.self, fn: <)
            
        case .d_le:
            try eval_comp(type: Double.self, fn: <=)
            
        
        
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
                // Calling a lambda -> set the lambda itself as the first parameter
                
                // appending to the end means that the argument will pushed last and therefore is the first argument
                args.append(destinationInstructionPointer)
                
                // fetch the address of the lambda's invoke function pointer
                destinationInstructionPointer = heap[destinationInstructionPointer + 1]
            } else if destinationInstructionPointer == 0 {
                fatalError("the fuck")
            }
            
            try stack.push(stack.framePointer)
            try stack.push(instructionPointer + 1)
            
            try args.forEach(stack.push)
            
            stack.framePointer = stack.stackPointer
            instructionPointer = destinationInstructionPointer
            
            recordCallEvent(.entry(destinationInstructionPointer))
            
        case .ret:
            let returnValue = try stack.pop()
            
            for _ in 0..<immediate {
                _ = try stack.pop()
            }
            
            instructionPointer = try stack.pop()
            stack.framePointer = try stack.pop()
            
            try stack.push(returnValue)
            
            recordCallEvent(.exit)
            
            
        case .debug:
            print("Call Stack:")
            print(callStackSymbols().joined(separator: "\n"))
            
            noop()
        }
        
        
        if BytecodeInterpreter.verboseLogging {
            Log.info("[eval] ip=\(instructionPointer) stack after: \(heap)")
        }
    }
    
    
    // MARK: Debugging
    
    func callStackSymbols() -> [String] {
        return callStack.reversed().enumerated().map { index, address in
            var entry = ""
            entry += "\(index)".padding(.left, toLength: 4, withPad: " ").padding(.right, toLength: 7, withPad: " ")
            entry += "\(address)".padding(.left, toLength: 5, withPad: address > 0 ? "0" : " ")
            //entry += " " + String(procedureEntryAddresses[address] ?? "(unknown)")
            entry += " " + nameOfProcedure(atAddress: address)
            
            return entry
        }
    }
    
    
    private func nameOfProcedure(atAddress address: Int) -> String {
        if let entryPointName = procedureEntryAddresses[address] {
            return entryPointName
        }
        if let nativeFunction = Runtime.shared[address: address] {
            return nativeFunction.name
        }
        return "(unknown)"
    }
    
    
    enum CallEvent {
        case entry(Int)
        case exit
    }
    
    private func recordCallEvent(_ event: CallEvent) {
        switch event {
        case .entry(let address):
            callStack.append(address)
            if logAllCalls {
                print(indent, "CALL", nameOfProcedure(atAddress: address))
                level += 1
            }
            
        case .exit:
            let address = callStack.removeLast()
            if logAllCalls {
                level -= 1
                print(indent, "RET ", nameOfProcedure(atAddress: address))
            }
        }
    }
    
    
    var level = 0
    
    var indent: String {
        return String.init(repeating: "    ", count: level)
    }
}
