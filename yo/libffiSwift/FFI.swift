//
//  FFI.swift
//  yo
//
//  Created by Lukas Kollmer on 07.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// TODO all the pointer stuff is probably wrong


class FFIFunctionInvocation {
    typealias FunctionPointerType = @convention(c) () -> Void
    static private let _defaultHandle = dlopen(nil, RTLD_LAZY)
    
    let functionPointer: FunctionPointerType
    
    let returnType: FFIType
    let parameterTypes: [FFIType]
    
    private var cif = ffi_cif()
    
    private var argTypes:  UnsafeMutablePointer<UnsafeMutablePointer<ffi_type>?>
    private var arguments: UnsafeMutablePointer<UnsafeMutableRawPointer?>
    
    init(symbol: String, handle: UnsafeMutableRawPointer? = nil, returnType: FFIType, parameterTypes: [FFIType]) {
        self.functionPointer = unsafeBitCast(dlsym(handle ?? FFIFunctionInvocation._defaultHandle, symbol), to: FunctionPointerType.self)
        
        self.returnType = returnType
        self.parameterTypes = parameterTypes
        
        arguments = .allocate(capacity: parameterTypes.count)
        arguments.initialize(repeating: nil, count: parameterTypes.count)
        
        argTypes = .allocate(capacity: parameterTypes.count)
        argTypes.initialize(repeating: nil, count: parameterTypes.count)

        
        for (offset, type) in parameterTypes.enumerated() {
            argTypes.advanced(by: offset).pointee = .allocate(capacity: 1)
            argTypes.advanced(by: offset).pointee?.assign(from: type._pointer, count: 1)
        }
        
        let status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, UInt32(parameterTypes.count), returnType._pointer, argTypes)
        guard status == FFI_OK else { fatalError() }
    }
    
    
    func setArgument(_ argument: UnsafeMutableRawPointer, atIndex index: Int) {
        arguments.advanced(by: index).pointee = argument
    }
    
    
    func invoke<T>(/*returnValue: UnsafeMutableRawPointer*/) -> T {
        var retval = ffi_sarg() // TODO switch between ffi_arg and ffi_sarg based on which return type was defined?
        ffi_call(&cif, functionPointer, &retval, arguments)
        
        return cast(&retval)
    }
    
    
    deinit {
        arguments.deinitialize(count: parameterTypes.count)
        argTypes.deinitialize(count: parameterTypes.count)
    }
}

class FFIType {
    let _pointer: UnsafeMutablePointer<ffi_type>
    
    init(_ pointer: UnsafeMutablePointer<ffi_type>) {
        self._pointer = pointer
    }
}

extension FFIType {
    static let void               = FFIType(&ffi_type_void)
    static let uint8              = FFIType(&ffi_type_uint8)
    static let int8               = FFIType(&ffi_type_sint8)
    static let uint16             = FFIType(&ffi_type_uint16)
    static let int16              = FFIType(&ffi_type_sint16)
    static let uint32             = FFIType(&ffi_type_uint32)
    static let int32              = FFIType(&ffi_type_sint32)
    static let uint64             = FFIType(&ffi_type_uint64)
    static let int64              = FFIType(&ffi_type_sint64)
    static let float              = FFIType(&ffi_type_float)
    static let double             = FFIType(&ffi_type_double)
    static let pointer            = FFIType(&ffi_type_pointer)
    static let complex_float      = FFIType(&ffi_type_complex_float)
    static let complex_double     = FFIType(&ffi_type_complex_double)
    static let complex_longdouble = FFIType(&ffi_type_complex_longdouble)
    static let longdouble         = FFIType(&ffi_type_longdouble)
}
