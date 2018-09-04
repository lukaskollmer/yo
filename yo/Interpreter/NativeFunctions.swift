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
        fatalError("needs updated implementation")
        let value = heap[address + 1]
        let type = heap[address + 2]
        guard type == Constants.NumberTypeMapping.integer else {
            fatalError("TODO add support for non-int types")
        }
        return value
    }
    
    
    static func strlen(address: Int, heap: Heap) -> Int {
        // TODO just forward to libc's strlen or use String.count?
        var length = 0
        while heap.backing[_8: address + (length * sizeof(.i8))] != 0 {
            length += 1
        }
        return length
    }
    
    
    static func getString(_ offset: Int, _ interpreter: BytecodeInterpreter) -> String {
        return getString(atAddress: interpreter.stack.peek(offset: offset), heap: interpreter.heap)
    }
    
    static func getString(atAddress _address: Int, heap: Heap) -> String {
        guard _address != 0 else { return "(null)" }
        
        let address = heap[_address + sizeof(.i64)]
        return String(cString: heap.backing.base.advanced(by: address).assumingMemoryBound(to: Int8.self))
    }

}
