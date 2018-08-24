//
//  Stack.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// A wrapper around a `Heap` that implements a stack growing from the end of the heap
class Stack: CustomStringConvertible {
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
        stackPointer += 1
        heap[actualIndex(for: stackPointer)] = newValue
    }
    
    func pop() throws -> Int {
        let value = heap.backing[actualIndex(for: stackPointer)]
        heap[actualIndex(for: stackPointer)] = heap.initialValue
        stackPointer -= 1
        return value
    }
    
    func pushFrame(index: Int, value: Int) {
        heap[actualIndex(for: framePointer + index)] = value
    }
    
    func getFrameElement(atIndex index: Int) -> Int {
        return heap[actualIndex(for: framePointer + index)]
    }
    
    func peek(offset: Int = 0) -> Int {
        return heap[actualIndex(for: stackPointer + offset)]
    }
    
    
    private var elements: ArraySlice<Int> {
        return heap[actualIndex(for: stackPointer)..<actualIndex(for: 0)]
    }
    
    
    var description: String {
        return "<Stack sp=\(stackPointer) fp=\(framePointer) elements=\(elements)>"
    }
}
