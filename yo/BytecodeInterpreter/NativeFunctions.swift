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
        let value = heap[address + 1]
        let type = heap[address + 2]
        guard type == Constants.NumberTypeMapping.integer else {
            fatalError("TODO add support for non-int types")
        }
        return value
    }
    
    
    static func strlen(address: Int, heap: Heap) -> Int {
        var length = 0
        while heap[address + (length * sizeof(.i64))] != 0 {
            length += 1
        }
        return length
    }
    
    
    static func getString(_ offset: Int, _ interpreter: BytecodeInterpreter) -> String {
        return getString(atAddress: interpreter.stack.peek(offset: offset), heap: interpreter.heap)
    }
    
    static func getString(atAddress _address: Int, heap: Heap) -> String {
        guard _address != 0 else { return "(null)" }
        
        let i64_s = ASTType.i64.size
        
        let address = heap[_address + i64_s]
        let length = strlen(address: address, heap: heap)
        
        let start = address
        let end = start + (length * i64_s)
        
        
        let characters: [Character] =
            stride(from: start, to: end, by: i64_s).map { heap[$0] }
            .compactMap(UnicodeScalar.init)
            .map(Character.init)
        
        return String(characters)
    }

}
