//
//  NativeFunctions+FFI.swift
//  yo
//
//  Created by Lukas Kollmer on 05.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// WIP

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

private var _functions = [FFIFunction]()

enum NativeFunctions_FFI: NativeFunctions {
    static func register(_ runtime: Runtime) {
        
        runtime["ffi", "_call_old", .int, [.String, .int]] = { interpreter in
            let _symbolName = getString(0, interpreter)
            let returnType = ffi_type_mapping[interpreter.stack.peek(offset: -1)]
            
            let functionInvocation = FFIFunction(symbol: _symbolName, returnType: returnType, parameterTypes: [])
            return functionInvocation.invoke() as Int
        }
        
        
        runtime["ffi", "_call", .int, [.int, .int]] = { interpreter in
            var _handle = interpreter.stack.peek()
            let fn: FFIFunction = cast(&_handle)
            
            let arguments_ptr = interpreter.stack.peek(offset: -1)
            
            for i in 0..<fn.parameterTypes.count {
                fatalError("TODO: implement")
                //interpreter.heap.backing.withUnsafeMutableBufferPointer { ptr in
                //    fn.setArgument(ptr.baseAddress!.advanced(by: arguments_ptr + i), atIndex: i)
                //}
            }
            
            return fn.invoke() as Int
        }
        
        
        
        
        
        // Parameters:
        // - (String)   symbol name
        // - (int)      return type
        // - (int)      argc
        // - (int)      pointer to array containing argument types
        // - (int)      lib handle
        // - (bool)     is variadic
        runtime["ffi", "_declareFunction", .int, [.String, .int, .int, .int, .int, .bool]] = { interpreter in
            let _symbolName = getString(0, interpreter)
            let returnType = ffi_type_mapping[interpreter.stack.peek(offset: -1)]
            let argc = interpreter.stack.peek(offset: -2)
            let parameterTypesPtr = interpreter.stack.peek(offset: -3)
            var _libHandle  = interpreter.stack.peek(offset: -4)
            let isVariadic = interpreter.stack.peek(offset: -5) == Constants.BooleanValues.true
            
            var parameterTypes = [FFIType]()
            
            for i in 0..<argc {
                let numberPtr = interpreter.heap[_64: parameterTypesPtr + i]
                let intValue = number_getIntValue(address: numberPtr, heap: interpreter.heap)
                let type = ffi_type_mapping[intValue]
                parameterTypes.append(type)
            }
            
            
            var fn = FFIFunction(
                symbol: _symbolName,
                handle: _libHandle == 0 ? nil : cast(&_libHandle),
                returnType: returnType,
                parameterTypes: parameterTypes
            )
            _functions.append(fn) // retain it
            
            return cast(&fn)
            
            /*let libobjc = dlopen("/usr/lib/libobjc.A.dylib", RTLD_LAZY)!
            
            let _objc_getClass = FFIFunctionInvocation(symbol: "objc_getClass", handle: libobjc, returnType: .pointer, parameterTypes: [.pointer])
            
            var text = "NSString".utf8CString
            
            var arg: UnsafePointer<CChar>!
            text.withUnsafeBufferPointer { ptr in
                arg = ptr.baseAddress!
            }
            
            _objc_getClass.setArgument(&arg, atIndex: 0)
            
            var ptr: Int = _objc_getClass.invoke()
            print("ptr", ptr)
            
            
            
            
            let _class_getName = FFIFunctionInvocation(symbol: "class_getName", handle: libobjc, returnType: .pointer, parameterTypes: [.pointer])
            
            _class_getName.setArgument(&ptr, atIndex: 0)
            
            let _classname: UnsafePointer<CChar> = _class_getName.invoke()
            print("name", _classname)
            puts(_classname)*/
        }
        
        
        // Parameters:
        // - path
        // - mode
        runtime["ffi", "_dlopen", .int, [.String]] = { interpreter in
            let path = getString(0, interpreter)
            
            guard var handle = dlopen(path, RTLD_LAZY) else {
                return -1
            }
            
            return cast(&handle)
        }
        
        
        // TODO implement
        runtime["ffi", "_dlclose", .int, [.int]] = { interpreter in
            //dlclose(<#T##__handle: UnsafeMutableRawPointer!##UnsafeMutableRawPointer!#>)
            fatalError()
        }
        
        
        
        
        // MARK: C String Utils
        
        
        runtime["ffi", "_allocCString", .int, [.String]] = { interpreter in
            let inputString = getString(0, interpreter)
            
            var dest = malloc(MemoryLayout<CChar>.size * inputString.count + 1).bindMemory(to: CChar.self, capacity: inputString.count + 1)
            let src = inputString.utf8CString.withUnsafeBufferPointer { $0.baseAddress }
            strcpy(dest, src)
            
            return cast(&dest)
        }
        
        
        runtime["ffi", "_printCString", .void, [.int]] = { interpreter in
            var ptr = interpreter.stack.peek()
            
            let str: UnsafePointer<CChar> = cast(&ptr)
            puts(str)
            
            return 0
        }
        
        runtime["ffi", "_freeCString", .void, [.int]] = { interpreter in
            var ptr = interpreter.stack.peek()
            
            let str: UnsafeMutablePointer<CChar> = cast(&ptr)
            free(str)
            
            return 0
        }    
    }
}
