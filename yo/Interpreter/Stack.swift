//
//  Stack.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


enum StackError: Error {
    case stackOverflow
    case stackUnderflow
}

class Stack<T> {
    
    private let size: Int
    private var backing = [T]()
    private let initialValue: T
    
    var stackPointer = -1
    var framePointer = -1
    
    init(size: Int, initialValue: T) {
        self.size = size
        self.initialValue = initialValue
        
        // TODO make the stack grow dynamically
        for _ in 0..<size {
            backing.append(initialValue)
        }
    }
    
    var isEmpty: Bool {
        return stackPointer == -1
    }
    
    func push(_ newValue: T) throws {
        guard backing.count <= size else {
            throw StackError.stackOverflow
        }
        
        stackPointer += 1
        backing[stackPointer] = newValue
    }
    
    func pop() throws -> T {
        let value = backing[stackPointer]
        backing[stackPointer] = initialValue
        stackPointer -= 1
        return value
    }
    
    func pushFrame(index: Int, value: T) {
        backing[framePointer + index] = value
    }
    
    func getFrameElement(atIndex index: Int) -> T {
        return backing[framePointer + index]
    }
}

extension Stack: CustomStringConvertible {
    var description: String {
        return "Stack<\(String(describing: T.self)) stackPtr=\(stackPointer) framePtr=\(framePointer) backing: \(String(describing: backing))>"
    }
}
