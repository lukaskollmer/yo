//
//  StackView.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// A wrapper around a `Heap<T>` that implements a `Stack<T>` growing from the end of the heap
class StackView<T> {
    unowned let heap: Heap<T> // TODO does unowned make sense here?
    let size: Int
    
    var stackPointer = -1
    var framePointer = -1
    
    init(heap: Heap<T>) {
        self.heap = heap
        self.size = heap.size
    }
    
    var isEmpty: Bool {
        return stackPointer == -1
    }
    
    private func actualIndex(for index: Int) -> Int {
        return heap.size - index - 1
    }
    
    func push(_ newValue: T) throws {
        guard heap.backing.count <= size else {
            throw HeapError.stackOverflow
        }
        
        stackPointer += 1
        heap.backing[actualIndex(for: stackPointer)] = newValue
    }
    
    func pop() throws -> T {
        let value = heap.backing[actualIndex(for: stackPointer)]
        heap.backing[actualIndex(for: stackPointer)] = heap.initialValue
        stackPointer -= 1
        return value
    }
    
    func pushFrame(index: Int, value: T) {
        heap.backing[actualIndex(for: framePointer + index)] = value
    }
    
    func getFrameElement(atIndex index: Int) -> T {
        return heap.backing[actualIndex(for: framePointer + index)]
    }
    
    func peek(offset: Int = 0) -> T {
        return heap.backing[actualIndex(for: stackPointer + offset)]
    }
}
