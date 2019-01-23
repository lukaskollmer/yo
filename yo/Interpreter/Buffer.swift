//
//  Buffer.swift
//  yo
//
//  Created by Lukas Kollmer on 05.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class Buffer {
    let size: Int
    let base: UnsafeMutableRawPointer
    
    init(byteCount: Int, alignment: Int = 0) {
        size = byteCount
        base = .allocate(byteCount: byteCount, alignment: alignment)
        base.initializeMemory(as: UInt8.self, repeating: 0, count: byteCount)
    }
    
    subscript<T>(ptr offset: Int, type: T.Type) -> UnsafeMutablePointer<T> {
        get {
            return base.advanced(by: offset).assumingMemoryBound(to: T.self)
        }
    }
    
    subscript<T>(offset: Int) -> T {
        get {
            return self[ptr: offset, T.self].pointee
        }
        
        set {
            self[ptr: offset, T.self].pointee = newValue
        }
    }
    
    subscript(_8 offset: Int) -> Int8 {
        get { return self[offset] }
        set { self[offset] = newValue }
    }
    
    subscript(_16 offset: Int) -> Int16 {
        get { return self[offset] }
        set { self[offset] = newValue }
    }
    
    subscript(_32 offset: Int) -> Int32 {
        get { return self[offset] }
        set { self[offset] = newValue }
    }
    
    subscript(_64 offset: Int) -> Int {
        get { return self[offset] }
        set { self[offset] = newValue }
    }
    
    func asArray<T>(ofType type: T.Type) -> Array<T> {
        return Array(UnsafeMutableRawBufferPointer.init(start: base, count: size).bindMemory(to: T.self))
    }
}
