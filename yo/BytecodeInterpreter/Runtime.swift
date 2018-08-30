//
//  Runtime.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

// TODO don't have Runtime conform to NativeFunctions. it's just a temporary hack to get the getString function
class Runtime: NativeFunctions {
    static func register(_ runtime: Runtime) {}
    
    // TODO move into NativeFunction?
    typealias NativeFunctionImp = (BytecodeInterpreter) -> Int
    
    class NativeFunction: FunctionSignature {
        let name: String
        let address: Int
        let imp: NativeFunctionImp
        
        let parameterTypes: [ASTType]
        let returnType: ASTType
        var annotations: [ASTAnnotation.Element] { return [] }
        
        init(name: String, address: Int, imp: @escaping NativeFunctionImp, parameterTypes: [ASTType], returnType: ASTType) {
            self.name = name
            self.address = address
            self.imp = imp
            self.parameterTypes = parameterTypes
            self.returnType = returnType
        }
    }
    
    
    static let shared = Runtime()
    private var addressCounter = Counter.init(initialValue: -1000)
    
    private(set) var builtins = [NativeFunction]()
    
    
    
    subscript(mangledName name: String) -> NativeFunction? {
        return builtins.first { $0.name == name }
    }
    
    // TODO return optional?
    subscript(address address: Int) -> NativeFunction! {
        return builtins.first { $0.address == address }
    }
    
    subscript(ns: String, name: String, returnType: ASTType, parameterTypes: [ASTType]) -> NativeFunctionImp {
        get { fatalError() }
        
        set {
            builtins.append(Runtime.NativeFunction(
                name: SymbolMangling.mangleStaticMember(ofType: ns, memberName: name),
                address: addressCounter.get(),
                imp: newValue,
                parameterTypes: parameterTypes,
                returnType: returnType
            ))
        }
    }
    
    
    private init() {
        
        NativeFunctions_IO.register(self)
        NativeFunctions_FFI.register(self)
        
        self["runtime", "alloc", .int, [.int]] = { interpreter in
            let size = interpreter.stack.peek()
            return interpreter.heap.alloc(size: size)
        }
        
        self["runtime", "free", .void, [.int]] = { interpreter in
            interpreter.stack.heap.free(address: interpreter.stack.peek())
            return 0
        }
        
        self["runtime", "fatalError", .void, [.String]] = { interpreter in
            let errorMessagePtr = interpreter.stack.peek()
            let errorMessage = errorMessagePtr == 0 ? "(null)" : Runtime.getString(atAddress: errorMessagePtr, heap: interpreter.heap)
            
            print("Aborting due to fatal error: \(errorMessage)")
            print("Call Stack:")
            print(interpreter.callStackSymbols().joined(separator: "\n"))
            
            fatalError()
        }
        
        self["runtime", "decltype", .String, [.any]] = {_ in return 0 }   // manually implemented in the compiler
        
        
        self["runtime", "_lookupAddress", .int, [.String]] = { interpreter in
            let symbolName = Runtime.getString(0, interpreter)
            return interpreter.procedureEntryAddresses[reverse: symbolName] ?? -1
        }
        
        
        // arguments:
        // 1. function address
        // 2. argc
        // 3. argv (pointer to primitive array)
        self["runtime", "_invoke", .any, [.int, .int, .int]] = { interpreter in
            let address = interpreter.stack.peek()
            let argc = interpreter.stack.peek(offset: -1)
            let argv = interpreter.stack.peek(offset: -2)
            
            let args: [Int] = argc == 0
                ? []
                : Array(interpreter.heap[argv..<(argv + argc)])
            
            guard let _ = interpreter.procedureEntryAddresses[address] else {
                fatalError("Trying to invoke an address which is not a function entry point")
            }
            
            return try! interpreter.call(address: address, arguments: args) // TODO handle error?
        }
        
        
        
        // Sorting
        
        self["runtime", "sort", .void, [.int, .int]] = { interpreter in
            let address = interpreter.stack.peek()
            let count = interpreter.stack.peek(offset: -1)
            interpreter.heap.sort(address: address, count: count, fn: <)
            return 0
        }
        
        self["runtime", "sortf", .void, [.int, .int, .function(returnType: .bool, parameterTypes: [.int, .int])]] = { interpreter in // todo have the sorting function take any?
            let address = interpreter.stack.peek()
            let count = interpreter.stack.peek(offset: -1)
            let fn_address = interpreter.stack.peek(offset: -2)
            
            interpreter.heap.sort(address: address, count: count) { a, b in
                let areInIncreasingOrder = try! interpreter.call(address: fn_address, arguments: [a, b])
                return areInIncreasingOrder == Constants.BooleanValues.true
            }
            
            return 0
        }
        
        // MARK: Hashing?
        
        self["runtime", "_hashString", .int, [.String]] = { interpreter in
            return Runtime.getString(atAddress: interpreter.stack.peek(), heap: interpreter.heap).hashValue
        }
        
        // MARK: IO
        
        
        self["runtime", "_print", .void, [.String]] = { interpreter in
            print(Runtime.getString(atAddress: interpreter.stack.peek(), heap: interpreter.heap))
            return 0
        }
        
        self["runtime", "_printi", .void, [.int]] = { interpreter in
            print(interpreter.stack.peek())
            return 0
        }
        
        
        self["runtime", "_printd", .void, [.double]] = { interpreter in
            print(interpreter.stack.peek().unsafe_loadAsDouble)
            return 0
        }
        
        
        self["runtime", "__format", .int, [.String, .Array]] = { interpreter in
            let heap = interpreter.heap
            
            let format = Runtime.getString(atAddress: interpreter.stack.peek(), heap: heap)
            let args_ptr = heap[interpreter.stack.peek(offset: -1) + 3]
            
            let getArgAtIndex: (Int) -> Int = { heap[args_ptr + $0] }
            
            var text = ""
            let scalars = format.unicodeScalars.map { $0 }
            var nextScalarFormatToken = false
            var arg_index = 0
            
            var skipNext = false
            for (index, scalar) in scalars.enumerated() {
                if skipNext {
                    skipNext = false
                    continue
                }
                
                let isLast = index >= scalars.count - 1
                
                if nextScalarFormatToken {
                    switch scalar {
                    case "i": // int
                        text += String(getArgAtIndex(arg_index))
                        
                    case "s": // String
                        text += Runtime.getString(atAddress: getArgAtIndex(arg_index), heap: heap)
                        
                    case "n": // Number
                        let addr = getArgAtIndex(arg_index)
                        let value = heap[addr + 1]
                        let type = heap[addr + 2]
                        switch type {
                        case Constants.NumberTypeMapping.integer:
                            if !isLast && scalars[index + 1] == "h" {
                                skipNext = true
                                text += "0x" + String(value, radix: 16).padding(.left, toLength: 9, withPad: "0")
                            } else {
                                text += String(value, radix: 10)
                            }
                        case Constants.NumberTypeMapping.boolean:
                            text += value == 0 ? "false" : "true"
                        case Constants.NumberTypeMapping.double:
                            text += String(value.unsafe_loadAsDouble)
                        default:
                            fatalError("Unsupported Number type \(type)")
                        }
                        break
                        
                    default:
                        fatalError("invalid format specifier '\(scalar)'")
                    }
                    
                    arg_index += 1
                    
                } else if scalar != "%" {
                    text.unicodeScalars.append(scalar)
                }
                nextScalarFormatToken = scalar == "%"
            }
            
            let string_backing = heap.alloc(size: text.count + 1)
            heap[string_backing] = text.count
            text.unicodeScalars.enumerated().forEach { heap[string_backing + $0.offset + 1] = Int($0.element.value) }
            
            return string_backing
        }
    }

}
