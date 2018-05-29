//
//  Heap.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


enum HeapError: Error {
    case stackOverflow
    case stackUnderflow // TODO throw this one
}

class Heap {
    // static stored properties aren't supported in generic types :/
    private static var resetOnFree: Bool { return false }
    
    let size: Int
    private(set) var stack: StackView! // we can't make this a stored property (let) bc the initializer takes `self`
    
    var backing = [Int]()
    let initialValue: Int = 0
    private var allocations = [(address: Int, size: Int)]()
    private var retainCounts = [Int: Int]() // [address: #refs]
    
    init(size: Int) {
        self.size = size
        self.stack = StackView(heap: self)
        
        for _ in 0..<size {
            backing.append(initialValue)
        }
    }
    
    // returns the address of the beginning of the allocated space
    func alloc(size: Int) -> Int {
        guard size > 0 else {
            fatalError()
        }
        
        if allocations.isEmpty {
            allocations.append((0, size))
            return 0
        }
        
        let numberOfAllocations = allocations.count
        for i in 0..<numberOfAllocations {
            let allocation = allocations[i]
            
            if i < numberOfAllocations - 1 {
                let nextAllocation = allocations[i + 1]
                let addressOfNextAllocation = nextAllocation.address
                
                if addressOfNextAllocation > allocation.address + allocation.size + size - 1 {
                    let newAddress = allocation.address + allocation.size
                    allocations.append((newAddress, size))
                    return newAddress
                }
            }
        }
        
        let lastAllocation = allocations[numberOfAllocations - 1]
        let newAddress = lastAllocation.address + lastAllocation.size
        
        allocations.append((newAddress, size))
        return newAddress
    }
    
    
    func free(address: Int) {
        let index = allocations.index { $0.address == address }!
        let allocation = allocations.remove(at: index)
        
        if Heap.resetOnFree {
            for i in allocation.address..<(allocation.address + allocation.size) {
                backing[i] = initialValue
            }
        }
    }
    
    
    
    // MARK: ARC
    
    // Get the retain count of the memory block allocated at `address`
    // returns -1 if `address` was never retained in the first place
    // TODO throw an error instead?
    func retainCount(ofAddress address: Int) -> Int {
        return retainCounts[address] ?? -1
    }
    
    // Increase the retain count of the memory block allocated at `address` by 1
    func retain(address: Int) {
        if retainCounts.keys.contains(address) {
            retainCounts[address]! += 1
        } else {
            retainCounts[address] = 1
        }
    }
    
    // Decrease the retain count of the memory block allocated at `address` by 1
    // Also frees the memory when the new retain count is 0
    func release(address: Int) {
        retainCounts[address]! -= 1
        
        if retainCount(ofAddress: address) == 0 {
            free(address: address)
        }
    }
    
    
    
    // TODO
    // returns the highest address used by the heap
    // used to throw an error when the stack grows into currently allocated heap space
    fileprivate var usedHeapSize: Int {
        let sorted = allocations.sorted(by: { (val0, val1) -> Bool in
            val0.address > val1.address
        })
        
        return -1
    }
    
    
    subscript(index: Int) -> Int {
        get {
            return backing[index]
        }
        set {
            backing[index] = newValue
        }
    }
    
    
    subscript(range: Range<Int>) -> ArraySlice<Int> {
        get {
            return backing[range]
        }
    }
}




