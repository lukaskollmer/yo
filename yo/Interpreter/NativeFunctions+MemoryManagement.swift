//
//  NativeFunctions+MemoryManagement.swift
//  yo
//
//  Created by Lukas Kollmer on 16.09.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


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
        
        let kIsMarkedForReleaseBit = 29
        let kIsDeallocatingBit = 30
        
        
        let isObject: (Int, Heap) -> Bool = { address, heap in
            return address != 0 && address % 16 == 0 && heap.backing[_64: address] != 0
        }
        
        let getRetainCount: (Int, Heap) -> Int = { address, heap in
            return heap[address] & 0x3fffffff;
        }
        
        let isMarkedForRelease: (Int, Heap) -> Bool = { address, heap in
            return heap[address] & (1 << kIsMarkedForReleaseBit) != 0
        }
        
        let setIsMarkedForReleaseBit: (Int, Heap, Bool) -> Void = { address, heap, isMarkedForRelease_newValue in
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
        
        
        let isDeallocating: (Int, Heap) -> Bool = { address, heap in
            return heap[address] & (1 << kIsDeallocatingBit) != 0
        }
        
        let setIsDeallocating: (Int, Heap) -> Void = { address, heap in
            guard !isDeallocating(address, heap) else {
                fatalError("Internal inconsistency: object at \(address.asHexString) is already deallocating")
            }
            
            heap[address] |= 1 << kIsDeallocatingBit
        }
        
        
        func wrap(_ fn: @escaping (Int, Heap) -> Int) -> (BytecodeInterpreter) -> Int {
            return { interpreter in
                return fn(interpreter.stack.peek(), interpreter.heap)
            }
        }
        
        runtime["runtime", "getRetainCount", .i64, [.any]] = wrap(getRetainCount)
        runtime["runtime", "markForRelease", .i64, [.any]] = wrap { setIsMarkedForReleaseBit($0, $1, true); return 0 }
        
        
        runtime["runtime", "retain", .any, [.any]] = { interpreter in
            let heap = interpreter.heap
            let address = interpreter.stack.peek()
            
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
        
        runtime["runtime", "release", .any, [.any]] = { interpreter in
            let heap = interpreter.heap
            let address = interpreter.stack.peek()
            
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
}
