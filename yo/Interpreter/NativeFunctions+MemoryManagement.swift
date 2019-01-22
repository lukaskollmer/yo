//
//  NativeFunctions+MemoryManagement.swift
//  yo
//
//  Created by Lukas Kollmer on 16.09.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


enum ARC {
    static let kIsMarkedForReleaseBit = 29
    static let kIsDeallocatingBit = 30
    
    
    static func isObject(_ address: Int, _ heap: Heap) -> Bool {
        return address != 0 && address % 16 == 0 && heap.backing[_64: address] != 0
    }
    
    static func getRetainCount(_ address: Int, _ heap: Heap) -> Int {
        return heap[address] & 0x3fffffff
    }
    
    static func isMarkedForRelease(_ address: Int, _ heap: Heap) -> Bool {
        return heap[address] & (1 << kIsMarkedForReleaseBit) != 0
    }
    
    static func setIsMarkedForReleaseBit(_ address: Int, _ heap: Heap, _ isMarkedForRelease_newValue: Bool) {
        switch isMarkedForRelease_newValue {
        case true:
            guard !isMarkedForRelease(address, heap) else {
                fatalError("Internal inconsistency: object at \(address.asHexString) is already marked for release")
            }
            heap[address] |= 1 << kIsMarkedForReleaseBit
            
        case false:
            guard isMarkedForRelease(address, heap) else {
                fatalError("Internal inconsistency: object at \(address.asHexString) is already not marked for release")
            }
            heap[address] &= ~(1 << kIsMarkedForReleaseBit)
        }
    }
    
    
    static func isDeallocating(_ address: Int, _ heap: Heap) -> Bool {
        return heap[address] & (1 << kIsDeallocatingBit) != 0
    }
    
    static func setIsDeallocating(_ address: Int, _ heap: Heap) {
        guard !isDeallocating(address, heap) else {
            fatalError("Internal inconsistency: object at \(address.asHexString) is already deallocating")
        }
        
        heap[address] |= 1 << kIsDeallocatingBit
    }
    
    
    @discardableResult
    static func retain(_ address: Int, heap: Heap) -> Int {
        guard isObject(address, heap) else {
            return address
        }
        
        if isMarkedForRelease(address, heap) {
            setIsMarkedForReleaseBit(address, heap, false)
            return address
        }
        
        heap[address] += 1
        return address
    }
    
    
    @discardableResult
    static func release(_ address: Int, interpreter: BytecodeInterpreter) -> Int {
        let heap = interpreter.heap
        
        guard isObject(address, heap) else {
            return address
        }
        
        if isDeallocating(address, heap) {
            // the object is already in the process of being deallocated, so we'll just ignore this release call
            return address
        }
        
        if isMarkedForRelease(address, heap) {
            setIsMarkedForReleaseBit(address, heap, false)
        }
        
        if getRetainCount(address, heap) == 1 {
            setIsDeallocating(address, heap)
            
            let typeof = interpreter.procedureEntryAddresses[reverse: SymbolMangling.mangleStaticMember(ofType: "runtime", memberName: "typeof")]!
            let type = try! interpreter.call(address: typeof, arguments: [address])
            let dealloc_fn_address = heap[type + sizeof(.i64) + sizeof(.String)] // TODO this is tied to the structure of the `Type` struct, which is bad
            _ = try! interpreter.call(address: dealloc_fn_address, arguments: [address])
            
            heap.free(address: address)
        } else {
            heap[address] -= 1
        }
        
        return address
    }
}


class NativeFunctions_MemoryManagement: NativeFunctions {
    
    static func register(_ runtime: Runtime) {
        
        // MARK: Memory allocation
        
        runtime["runtime", "alloc", .int, [.int]] = { interpreter in
            let size = interpreter.stack.peek()
            return interpreter.heap.alloc(size: size)
        }
        
        runtime["runtime", "free", .void, [.int]] = { interpreter in
            let address = interpreter.stack.peek()
            interpreter.stack.heap.free(address: address)
            return 0
        }
        
        
        // MARK: Reference Counting
        
        
        func wrap(_ fn: @escaping (Int, Heap) -> Int) -> (BytecodeInterpreter) -> Int {
            return { interpreter in
                return fn(interpreter.stack.peek(), interpreter.heap)
            }
        }
        
        runtime["runtime", "getRetainCount", .i64, [.any]] = wrap(ARC.getRetainCount)
        runtime["runtime", "markForRelease", .i64, [.any]] = wrap { ARC.setIsMarkedForReleaseBit($0, $1, true); return 0 }
        
        
        runtime["runtime", "retain", .any, [.any]] = { interpreter in
            return ARC.retain(interpreter.stack.peek(), heap: interpreter.heap)
        }
        
        runtime["runtime", "release", .any, [.any]] = { interpreter in
            return ARC.release(interpreter.stack.peek(), interpreter: interpreter)
        }
    }
}
