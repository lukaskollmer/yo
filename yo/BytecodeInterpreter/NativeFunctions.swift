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
    
    
    static func getString(_ offset: Int, _ interpreter: BytecodeInterpreter) -> String {
        return getString(atAddress: interpreter.stack.peek(offset: offset), heap: interpreter.heap)
    }
    
    static func getString(atAddress _address: Int, heap: Heap) -> String {
        guard _address != 0 else { return "(null)" }
        
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
