//
//  NativeFunctions.swift
//  yo
//
//  Created by Lukas Kollmer on 05.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


protocol NativeFunctions {
    static func register(_ runtime: Runtime)
}


extension NativeFunctions {
    
    static func number_getIntValue(address: Int, heap: Heap) -> Int {
        let value = heap[_64: address + sizeof(.i64)]
        let type = heap[_64: address + 2 * sizeof(.i64)]
        
        guard type == Constants.NumberTypeMapping.integer else {
            fatalError("TODO add support for non-int types")
        }
        return value
    }
    
    
    static func strlen(address: Int, heap: Heap) -> Int {
        // TODO just forward to libc's strlen or use String.count?
        var length = 0
        while heap[_8: address + (length * sizeof(.i8))] != 0 {
            length += 1
        }
        return length
    }
    
    
    static func getString(_ offset: Int, _ interpreter: BytecodeInterpreter) -> String {
        return getString(atAddress: interpreter.stack.peek(offset: offset), heap: interpreter.heap)
    }
    
    static func getString(atAddress _address: Int, heap: Heap) -> String {
        guard _address != 0 else { return "(null)" }
        
        let address = heap[_64: _address + sizeof(.i64)]
        // TODO Can we use String(bytesNoCopy) here?
        return String(cString: heap[ptr: address, Int8.self])
    }
    
    static func allocateBacking(forString _string: String, heap: Heap) -> Int {
        var string = _string
        if !string.hasSuffix("\0") {
            string += "\0"
        }
        
        let string_backing = heap.alloc(size: string.count * sizeof(.i8))
        string.unicodeScalars.enumerated().forEach { heap[_8: string_backing + ($0.offset * sizeof(.i8))] = Int8($0.element.value) }
        
        return string_backing
    }

}
