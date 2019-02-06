//
//  NativeFunctions+FFI.swift
//  yo
//
//  Created by Lukas Kollmer on 05.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


private let ffi_type_mapping: [FFIType] = [
    .void,
    .uint8, .int8, .uint16, .int16, .uint32, .int32, .uint64, .int64,
    .float, .double,
    .pointer,
    .complex_float,
    .complex_double,
    .complex_longdouble,
    .longdouble
]


// This is either a fucking genius idea, or absolutely terrible
private class RetainPool<T: AnyObject> {
    private var objects = [Int : T]()
    private var counter = Counter()
    
    init() {}
    
    func retain(_ object: T) -> Int {
        let id = counter.get()
        self.objects[id] = object
        return id
    }
    
    func release(id: Int) -> T? {
        return objects.removeValue(forKey: id)!
    }
    
    func get(id: Int) -> T {
        return objects[id]!
    }
}

private let ffi_function_pool = RetainPool<FFIFunction>()


enum NativeFunctions_FFI: NativeFunctions {
    
    static func register(_ runtime: Runtime) {
        
        
        runtime["ffi", "dlopen", .int, [.String]] = { interpreter in
            retain(offset: 0, interpreter: interpreter)
            defer { release(offset: 0, interpreter: interpreter) }
            
            let path = getString(0, interpreter)
            if let handle = dlopen(path, RTLD_LAZY) {
                return Int(bitPattern: handle)
            }
            return 0
        }
        
        
        runtime["ffi", "dlclose", .int, [.int]] = { interpreter in
            let handle = UnsafeMutableRawPointer(bitPattern: interpreter.stack.peek())
            return Int(dlclose(handle))
        }
        
        
        
        // Parameters:
        // - symbol
        // - return type
        // - #parameters
        // - parameter types (pointer to int array)
        // - lib handle
        runtime["ffi", "declareFunction", .int, [.String, .int, .int, .ref(.int), .int]] = { interpreter in
            let symbol = getString(0, interpreter)
            let returnType = ffi_type_mapping[interpreter.stack.peek(offset: -1)]
            let argc = interpreter.stack.peek(offset: -2)
            let parameterTypesPtr = interpreter.stack.peek(offset: -3)
            let handle = UnsafeMutableRawPointer(bitPattern: interpreter.stack.peek(offset: -4))
            
            var parameterTypes = [FFIType]()
            
            for i in 0..<argc {
                let number = interpreter.heap[_64: parameterTypesPtr + i * sizeof(.id)]
                let type = ffi_type_mapping[number]
                parameterTypes.append(type)
            }
            
            let fn = FFIFunction(
                symbol: symbol,
                handle: handle,
                returnType: returnType,
                parameterTypes: parameterTypes,
                isVariadic: false // TODO implement
            )
            
            return ffi_function_pool.retain(fn)
        }
        
        
        // Parameters:
        // - function handle
        // - arguments
        runtime["ffi", "invoke", .i64, [.int, .ref(.any)]] = { interpreter in
            let function = ffi_function_pool.get(id: interpreter.stack.peek())
            let argsPtr = interpreter.stack.peek(offset: -1) // Pointer to int array
            
            let read: (Int) -> Int = {
                interpreter.heap[_64: argsPtr + $0 * sizeof(.i64)]
            }
            
            let argc = function.isVariadic ? read(0) : function.parameterTypes.count
            
            for i in 0..<argc {
                function.setArgument(interpreter.heap.base.advanced(by: argsPtr + i * sizeof(.i64)), atIndex: i)
            }
            
            return function.invoke() as Int
        }
    }
}
