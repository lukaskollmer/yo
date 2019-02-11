//
//  NativeFunctions+IO.swift
//  yo
//
//  Created by Lukas Kollmer on 05.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

// WIP. Does not work yet

enum NativeFunctions_IO: NativeFunctions {
    
    static func register(_ runtime: Runtime) {
        runtime["io", "_open", .i32, [.String, .i32]] = { interpreter in
            retain(offset: 0, interpreter: interpreter)
            defer {
                release(offset: 0, interpreter: interpreter)
            }
            let path = self.getString(atAddress: interpreter.stack.peek(), heap: interpreter.stack.heap)
            let flag = Int32(interpreter.stack.peek(offset: -1))
            
            return Int(open(path, flag))
        }
        
        runtime["io", "_close", .i32, [.i32]] = { interpreter in
            let fd = Int32(interpreter.stack.peek())
            return Int(close(fd))
        }
        
        runtime["io", "_read", .i32, [.i32, .ptr(.i8), .i64]] = { interpreter in
            let fd = Int32(interpreter.stack.peek())
            let buf_ptr = interpreter.stack.peek(offset: -1)
            let len = interpreter.stack.peek(offset: -2)
            let buffer = interpreter.heap.base.advanced(by: buf_ptr)
            return read(fd, buffer, len)
        }
        
        runtime["io", "_lseek", .i64, [.i32, .i64, .i32]] = { interpreter in
            let arg0 = Int32(interpreter.stack.peek())
            let arg1 = Int64(interpreter.stack.peek(offset: -1))
            let arg2 = Int32(interpreter.stack.peek(offset: -2))
            return lseek(arg0, arg1, arg2) |> numericCast
        }
    }
}
