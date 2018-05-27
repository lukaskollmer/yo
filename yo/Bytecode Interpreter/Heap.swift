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
    case StackUnderflow // TODO throw this one
}

class Heap<T> {
    
    let size: Int
    private(set) var stack: StackView<T>! // we can't make this a stored property (let) bc the initializer takes `self`
    
    var backing = [T]()
    var initialValue: T
    private var allocations = [(address: Int, size: Int)]()
    
    init(size: Int, initialValue: T) {
        self.size = size
        self.initialValue = initialValue
        self.stack = StackView<T>(heap: self)
        
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
        // TODO add an option to override the freed data w/ initialValue
        let index = allocations.index { $0.address == address }!
        allocations.remove(at: index)
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
    
    
    subscript(index: Int) -> T {
        get {
            return backing[index]
        }
        set {
            backing[index] = newValue
        }
    }
}




