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
        // TODO
        return -1
    }
    
    // marks the
    func free(address: Int) {
        // TODO
    }
    
    fileprivate var usedHeapSize: Int {
        let sorted = allocations.sorted(by: { (val0, val1) -> Bool in
            val0.address > val1.address
        })
        
        
        return -1
    }
}




