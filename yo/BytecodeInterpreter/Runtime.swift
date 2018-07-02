//
//  Runtime.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class Runtime {
    typealias NativeFunctionImp = (BytecodeInterpreter) -> Int
    typealias NativeFunction = (name: String, address: Int, info: SemanticAnalyzer.FunctionInfo, imp: NativeFunctionImp)
    
    
    static let shared = Runtime()
    private var addressCounter = Counter.init(initialValue: -1000)
    
    private(set) var builtins = [NativeFunction]()
    
    
    
    subscript(mangledName name: String) -> NativeFunction? {
        return builtins.first { $0.name == name }
    }
    
    // TODO return optional?
    subscript(address address: Int) -> NativeFunction {
        if let builtin = builtins.first(where: { $0.address == address }) {
            return builtin
        }
        fatalError("no native func at address \(address)")
    }
    
    subscript(ns: String, name: String, returnType: ASTType, parameterTypes: [ASTType]) -> NativeFunctionImp {
        get { fatalError() }
        
        set {
            builtins.append((
                name: SymbolMangling.mangleStaticMember(ofType: ns, memberName: name),
                address: addressCounter.get(),
                info: (parameterTypes.count, parameterTypes, returnType, []),
                imp: newValue)
            )
        }
    }
    
    
    private init() {
        
        self["runtime", "alloc", .int, [.int]] = { interpreter in
            let size = interpreter.stack.peek()
            return interpreter.heap.alloc(size: size)
        }
        
        self["runtime", "free", .void, [.int]] = { interpreter in
            interpreter.stack.heap.free(address: interpreter.stack.peek())
            return 0
        }
        
        self["runtime", "fatalError", .void, [.String]] = { interpreter in
            fatalError(self.getString(atAddress: interpreter.stack.peek(), heap: interpreter.stack.heap))
        }
        
        self["runtime", "typeof", .String, [.any]] = {_ in return 0 }   // manually implemented in the compiler
        
        // Sorting
        
        self["runtime", "sort", .void, [.int, .int]] = { interpreter in
            let address = interpreter.stack.peek()
            let count = interpreter.stack.peek(offset: -1)
            interpreter.heap.sort(address: address, count: count, fn: <)
            return 0
        }
        
        self["runtime", "sortf", .void, [.int, .int, .function(returnType: .int, parameterTypes: [.int, .int])]] = { interpreter in // todo have the sorting function take any?
            let address = interpreter.stack.peek()
            let count = interpreter.stack.peek(offset: -1)
            let fn_address = interpreter.stack.peek(offset: -2)
            
            interpreter.heap.sort(address: address, count: count) { a, b in
                let areInIncreasingOrder = try! interpreter.call(address: fn_address, arguments: [a, b])
                return Bool(areInIncreasingOrder == 1)
            }
            
            return 0
        }
        
        // MARK: Hashing?
        
        self["runtime", "_hashString", .int, [.String]] = { interpreter in
            return self.getString(atAddress: interpreter.stack.peek(), heap: interpreter.heap).hashValue
        }
        
        // MARK: IO
        
        
        self["runtime", "_print", .void, [.String]] = { interpreter in
            print(self.getString(atAddress: interpreter.stack.peek(), heap: interpreter.heap))
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
            
            let format = self.getString(atAddress: interpreter.stack.peek(), heap: heap)
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
                        text += self.getString(atAddress: getArgAtIndex(arg_index), heap: heap)
                        
                    case "n": // Number
                        let addr = getArgAtIndex(arg_index)
                        let value = heap[addr + 1]
                        let type = heap[addr + 2]
                        switch type {
                        case 0:
                            if !isLast && scalars[index + 1] == "h" {
                                skipNext = true
                                text += "0x" + String(value, radix: 16).padding(.left, toLength: 9, withPad: "0")
                            } else {
                                text += String(value, radix: 10)
                            }
                        case 1:
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
    
    
    
    
    
   
    
    
    // MARK: helpers
    
    private func getString(atAddress _address: Int, heap: Heap) -> String {
        let address = heap[_address + 1]
        let size = heap[address]
        
        let start = address + 1
        let end = start + size
        
        let characters: [Character] = heap[start..<end]
            .compactMap(UnicodeScalar.init)
            .map(Character.init)
        
        return String(characters)
    }

}
