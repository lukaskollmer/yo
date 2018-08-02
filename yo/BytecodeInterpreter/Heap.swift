//
//  Heap.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// MARK: Helpers
private func roundUpToNextEvenNumber(_ x: Int) -> Int {
    return Int(round(Double(x) / 2) * 2)
}


// MARK: Heap
enum HeapError: Error {
    case stackOverflow
    case stackUnderflow // TODO throw this one
}

class Heap {
    // whether data should be zeroed out on free
    private static let resetOnFree = true
    
    let size: Int
    private(set) var stack: Stack! // we can't make this a stored property (let) bc the initializer takes `self`
    
    var backing = [Int]()
    let initialValue: Int = 0
    private(set) var allocations = [(address: Int, size: Int)]()
    
    init(size: Int) {
        self.size = size
        self.stack = Stack(heap: self)
        
        for _ in 0..<size {
            backing.append(initialValue)
        }
        
        _ = alloc(size: 1) // make sure all addresses are > 0
    }
    
    // returns the address of the beginning of the allocated space
    func alloc(size: Int) -> Int {
        guard size > 0 else {
            fatalError()
        }
        
        // round up `size` to the next even number to ensure that all memory addresses are even
        // this might be useful down the line (think tagged pointers, etc)
        let size = roundUpToNextEvenNumber(size)
        
        let address = firstFreeAddress(forSize: size)
        allocations.append((address, size))
        
        // TODO this is a giant bottleneck
        allocations.sort { $0.address < $1.address } // not sure why but it seems like new tupels aren't always appended to the end of the array?
        
        return address
    }
    
    
    func free(address: Int) {
        guard address > 0 else { return }
        
        let index = allocations.index { $0.address == address }!
        let allocation = allocations.remove(at: index)
        
        if Heap.resetOnFree {
            for i in allocation.address..<(allocation.address + allocation.size) {
                backing[i] = initialValue
            }
        }
    }
    
    private func _invalidHeapAccess(atIndex index: Int) -> Never {
        fatalError("Ran out of heap space. Tried to access \(index). Heap size: \(self.size)")
    }
    
    
    subscript(index: Int) -> Int {
        get {
            guard let value = backing[safe: index] else {
                _invalidHeapAccess(atIndex: index)
            }
            return value
        }
        set {
            guard backing.isValidIndex(index) else {
                _invalidHeapAccess(atIndex: index)
            }
            backing[index] = newValue
        }
    }
    
    
    subscript(range: Range<Int>) -> ArraySlice<Int> {
        get {
            return backing[range]
        }
        set {
            backing[range] = newValue
        }
    }
    
    
    private func firstFreeAddress(forSize size: Int) -> Int {
        if allocations.isEmpty {
            return 0
        }
        
        let numberOfAllocations = allocations.count
        for i in 0..<numberOfAllocations {
            let allocation = allocations[i]
            
            if i < numberOfAllocations - 1 {
                let nextAllocation = allocations[i + 1]
                let addressOfNextAllocation = nextAllocation.address
                
                if addressOfNextAllocation > allocation.address + allocation.size + size - 0 {
                    return allocation.address + allocation.size
                }
            }
        }
        
        let lastAllocation = allocations[numberOfAllocations - 1]
        return lastAllocation.address + lastAllocation.size
        
    }
}



/// MARK: sorting




extension Heap {
    
    func sort(address: Int, count: Int, fn: (Int, Int) -> Bool) {
        // TODO there has to be a better way than this...
        
        let range: Range<Int> = address..<(address + count)
        backing.replaceSubrange(range, with: self[range].sorted(by: fn))
    }
    
}


