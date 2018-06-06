//
//  Stack.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// A wrapper around a `Heap` that implements a stack growing from the end of the heap
class Stack {
    unowned let heap: Heap // TODO does unowned make sense here?
    let size: Int
    
    var stackPointer = -1
    var framePointer = -1
    
    init(heap: Heap) {
        self.heap = heap
        self.size = heap.size
    }
    
    var isEmpty: Bool {
        return stackPointer == -1
    }
    
    private func actualIndex(for index: Int) -> Int {
        return heap.size - index - 1
    }
    
    func push(_ newValue: Int) throws {
        guard heap.backing.count <= size else {
            throw HeapError.stackOverflow
        }
        
        stackPointer += 1
        heap.backing[actualIndex(for: stackPointer)] = newValue
    }
    
    func pop() throws -> Int {
        let value = heap.backing[actualIndex(for: stackPointer)]
        heap.backing[actualIndex(for: stackPointer)] = heap.initialValue
        stackPointer -= 1
        return value
    }
    
    func pushFrame(index: Int, value: Int) {
        heap.backing[actualIndex(for: framePointer + index)] = value
    }
    
    func getFrameElement(atIndex index: Int) -> Int {
        return heap.backing[actualIndex(for: framePointer + index)]
    }
    
    func peek(offset: Int = 0) -> Int {
        return heap.backing[actualIndex(for: stackPointer + offset)]
    }
}
