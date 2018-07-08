//
//  FFI.swift
//  yo
//
//  Created by Lukas Kollmer on 07.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// TODO all the pointer stuff is most likely wrong



typealias LKFFIFunctionPointerType = @convention(c) () -> Void

class FFIFunctionInvocation {
    static private let _defaultHandle = dlopen(nil, RTLD_LAZY)
    
    let functionPointer: LKFFIFunctionPointerType
    
    let returnType: FFIType
    let parameterTypes: [FFIType]
    
    private var cif = ffi_cif()
    
    private var argTypes: UnsafeMutablePointer<UnsafeMutablePointer<ffi_type>?>
    private var arguments: UnsafeMutablePointer<UnsafeMutableRawPointer?>
    
    init(symbol: String, handle: UnsafeMutableRawPointer? = nil, returnType: FFIType, parameterTypes: [FFIType]) {
        self.functionPointer = unsafeBitCast(dlsym(handle ?? FFIFunctionInvocation._defaultHandle, symbol), to: LKFFIFunctionPointerType.self)
        
        self.returnType = returnType
        self.parameterTypes = parameterTypes
        self.arguments = UnsafeMutablePointer<UnsafeMutableRawPointer?>.allocate(capacity: parameterTypes.count)
        
        self.argTypes = UnsafeMutablePointer.allocate(capacity: parameterTypes.count)
        for (offset, type) in parameterTypes.enumerated() {
            self.argTypes.advanced(by: offset).pointee = UnsafeMutablePointer<ffi_type>.allocate(capacity: 1)
            self.argTypes.advanced(by: offset).pointee?.assign(from: type._pointer, count: 1)
        }
        
        let status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, UInt32(parameterTypes.count), returnType._pointer, argTypes)
        guard status == FFI_OK else { fatalError() }
    }
    
    func setArgument(_ argument: UnsafeMutableRawPointer, atIndex index: Int) {
        arguments.advanced(by: index).pointee = argument
    }
    
    // TODO make this generic?
    func invoke<T>(/*returnValue: UnsafeMutableRawPointer*/) -> T {
    
        var retval = ffi_sarg() // TODO switch between ffi_arg and ffi_sarg based on which return type was defined?
        ffi_call(&cif, functionPointer, &retval, arguments)
        
        return retval as! T
    }
    
    
    deinit {
        // TODO is deallocating this correct here?
        arguments.deallocate()
        argTypes.deallocate()
    }
}

class FFIType {
    let _pointer: UnsafeMutablePointer<ffi_type>
    
    static let pointer = FFIType(&ffi_type_pointer)
    static let int32   = FFIType(&ffi_type_sint32)
    static let uint32  = FFIType(&ffi_type_uint32)
    static let int64   = FFIType(&ffi_type_sint64)
    static let uint64  = FFIType(&ffi_type_uint64)
    // TODO add all other types
    
    init(_ pointer: UnsafeMutablePointer<ffi_type>) {
        self._pointer = pointer
    }
}

