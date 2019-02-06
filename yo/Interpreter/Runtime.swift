//
//  Runtime.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

private let ResolvedAtCompileTime: Runtime.NativeFunctionImp = { interpreter in
    let functionName = Runtime.shared[address: interpreter.callStack.last!]!.name
    fatalError("'\(functionName)' should be resolved at compile time")
}

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
        
        NativeFunctions_MemoryManagement.register(self)
        NativeFunctions_IO.register(self)
        NativeFunctions_FFI.register(self)
        
        
        
        self["runtime", "fatalError", .void, [.String]] = { interpreter in
            let errorMessagePtr = interpreter.stack.peek()
            let errorMessage = errorMessagePtr == 0 ? "(null)" : Runtime.getString(atAddress: errorMessagePtr, heap: interpreter.heap)
            
            log(.info, "Aborting due to fatal error: \(errorMessage)")
            log(.info, "Call Stack:")
            log(.info, interpreter.callStackSymbols().joined(separator: "\n"))
            
            fatalError()
        }
        
        // These are resolved at compile time
        self["runtime", "decltype", .String, [.any]]        = ResolvedAtCompileTime
        self["runtime", "offset", .int, [.String, .String]] = ResolvedAtCompileTime
        
        
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
                ? []    // NOTE: since arguments are passed on the stack (which is always 64 bit wide), there's no need to take element sizes into account
                : stride(from: argv, to: (argv + argc * sizeof(.i64)), by: sizeof(.i64)).map { interpreter.heap[$0] }
            
            guard let _ = interpreter.procedureEntryAddresses[address] else {
                fatalError("Trying to invoke an address which is not a function entry point")
            }
            
            return interpreter.call(address: address, arguments: args) // TODO handle error?
        }
        
        
        
        
        // MARK: Sorting
        
        // helper function used by `runtime::sort` and `runtime::sortf`
        func _sort(interpreter: BytecodeInterpreter, address: Int, count: Int, elementSize: Int, fn_address: Int?) {
            typealias SortingImp = (UnsafeRawPointer?, UnsafeRawPointer?) -> Int32
            
            // Q: Why are these structs?
            // A: From how i unserstand it, you can't do `fn<Int>` when refering to a function,
            // which means we have to use the struct as a wrapper to pass the generic parameter
            
            struct SortingImp_Default<T: SignedInteger> {
                static func imp(_ arg0: UnsafeRawPointer!, _ arg1: UnsafeRawPointer!) -> Int32 {
                    let x = arg0.load(as: T.self)
                    let y = arg1.load(as: T.self)
                    
                    return y > x ? -1 : (y == x ? 0 : 1)
                }
            }
            
            struct SortingImp_CustomCall<T: SignedInteger> {
                let interpreter: BytecodeInterpreter
                let address: Int
                
                func imp(_ arg0: UnsafeRawPointer!, _ arg1: UnsafeRawPointer!) -> Int32 {
                    let x = Int(arg0.load(as: T.self))
                    let y = Int(arg1.load(as: T.self))
                    
                    let retval = interpreter.call(address: address, arguments: [x, y])
                    return retval == Constants.BooleanValues.true ? -1 : 1
                }
            }
            
            func GetSortingImp<T: SignedInteger>(withType type: T.Type) -> SortingImp {
                if let fn_address = fn_address {
                    return SortingImp_CustomCall<T>(interpreter: interpreter, address: fn_address).imp
                } else {
                    return SortingImp_Default<T>.imp
                }
            }
            
            let base = interpreter.heap.base.advanced(by: address)
            let imp: SortingImp
            
            switch elementSize {
            case sizeof(.i8):
                imp = GetSortingImp(withType: Int8.self)
                
            case sizeof(.i16):
                imp = GetSortingImp(withType: Int16.self)
                
            case sizeof(.i32):
                imp = GetSortingImp(withType: Int32.self)
                
            case sizeof(.i64):
                imp = GetSortingImp(withType: Int64.self)
                
            default:
                fatalError("invalid size")
            }
            
            qsort_b(base, count, elementSize, imp)
            
        }
        
        self["runtime", "sort", .void, [.ref(.any), .i64, .i64]] = { interpreter in
            let address = interpreter.stack.peek()
            let count = interpreter.stack.peek(offset: -1)
            let elementSize = interpreter.stack.peek(offset: -2)
            
            _sort(interpreter: interpreter, address: address, count: count, elementSize: elementSize, fn_address: nil)
            return 0
        }
        
        
        self["runtime", "sortf", .void, [.ref(.any), .i64, .i64, .function(returnType: .bool, parameterTypes: [.any, .any])]] = { interpreter in
            let address = interpreter.stack.peek()
            let count = interpreter.stack.peek(offset: -1)
            let elementSize = interpreter.stack.peek(offset: -2)
            let fn_address = interpreter.stack.peek(offset: -3)
            
            _sort(interpreter: interpreter, address: address, count: count, elementSize: elementSize, fn_address: fn_address)
            return 0
        }
        
        
        // MARK: Hashing?
        
        self["runtime", "_hashString", .int, [.String]] = { interpreter in
            return Runtime.getString(atAddress: interpreter.stack.peek(), heap: interpreter.heap).hashValue
        }
        
        self["runtime", "_strlen", .int, [.ref(.i8)]] = { interpreter in
            return Runtime.strlen(address: interpreter.stack.peek(), heap: interpreter.heap)
        }
        
        self["runtime", "_stringDebugDescription", .ref(.i8), [.String]] = { interpreter in
            let string = Runtime.getString(0, interpreter)
            return Runtime.allocateBacking(forString: string.debugDescription, heap: interpreter.heap)
        }
        
        // MARK: IO
        
        self["runtime", "_printc", .void, [.ref(.i8)]] = { interpreter in
            let address = interpreter.stack.peek()
            //let cString = interpreter.heap.base.advanced(by: address).assumingMemoryBound(to: Int8.self)
            let cString = interpreter.heap[ptr: address, Int8.self]
            puts(cString)
            return 0
        }
        
        
        self["runtime", "_print", .void, [.String]] = { interpreter in
            Swift.print(Runtime.getString(atAddress: interpreter.stack.peek(), heap: interpreter.heap))
            return 0
        }
        
        self["runtime", "_printi", .void, [.int]] = { interpreter in
            Swift.print(interpreter.stack.peek())
            return 0
        }
        
        
        self["runtime", "_printd", .void, [.double]] = { interpreter in
            Swift.print(interpreter.stack.peek().unsafe_loadAsDouble)
            return 0
        }
        
        
        self["runtime", "itoa", .String, [.i64, .int]] = { interpreter in
            let value = interpreter.stack.peek()
            let base = interpreter.stack.peek(offset: -1)
            let string = String(value, radix: base)
            return Runtime.allocateBacking(forString: string, heap: interpreter.heap)
        }
        
        self["runtime", "_char_to_string", .ref(.i8), [.i8]] = { interpreter in
            let string = interpreter.stack.peek() |> UnicodeScalar.init |> unwrap |> Character.init |> String.init
            return Runtime.allocateBacking(forString: string, heap: interpreter.heap)
        }
        
        
        self["runtime", "__format", .ref(.i8), [.String, .Array]] = { interpreter in
            let heap = interpreter.heap
            
            let format = Runtime.getString(atAddress: interpreter.stack.peek(), heap: heap)
            let args_ptr: Int = heap[interpreter.stack.peek(offset: -1) + TypeCache.sizeof([.i64, .i64, .i64])]
            
            let getArgAtIndex: (Int) -> Int = { heap[args_ptr + $0 * ASTType.i64.size] }
            
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
                        let str = Runtime.getString(atAddress: getArgAtIndex(arg_index), heap: heap)
                        //text += Runtime.getString(atAddress: getArgAtIndex(arg_index), heap: heap)
                        text += str
                        
                    case "n": // Number
                        let addr = getArgAtIndex(arg_index)
                        let value: Int = heap[addr + sizeof(.i64)]
                        let type: Int = heap[addr + 2*sizeof(.i64)]
                        switch type {
                        case Constants.NumberTypeMapping.integer:
                            if !isLast && scalars[index + 1] == "h" {
                                skipNext = true
                                text += value.asHexString
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
            return Runtime.allocateBacking(forString: text, heap: interpreter.heap)
        }
    }

}
