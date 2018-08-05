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
    let baseAddress: UnsafeMutablePointer<Int>
    
    init(size: Int) {
        self.size = size
        baseAddress = UnsafeMutablePointer<Int>.allocate(capacity: size)
        baseAddress.initialize(repeating: 0, count: size)
    }
    
    subscript(index: Int) -> Int {
        get {
            return baseAddress.advanced(by: index).pointee
        }
        set {
            baseAddress.advanced(by: index).pointee = newValue
        }
    }
    
    deinit {
        fatalError()
        baseAddress.deinitialize(count: size)
        baseAddress.deallocate()
    }
}
