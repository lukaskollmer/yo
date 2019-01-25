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
        runtime["io", "_open", .int, [.String, .String]] = { interpreter in
            let path = self.getString(atAddress: interpreter.stack.peek(), heap: interpreter.stack.heap)
            let mode = self.getString(atAddress: interpreter.stack.peek(offset: -1), heap: interpreter.stack.heap)
            
            guard let handle = fopen(path, mode) else {
                return -1
            }
            
            return reinterpret_cast(handle)
            
            //var dest = [UInt8](repeating: 0, count: 5)
            //fread(&dest, 1, 5, file)
        }
        
        runtime["io", "_close", .int, [.int]] = { interpreter in
            let arg0 = interpreter.stack.peek()
            let handle: UnsafeMutablePointer<FILE>! = reinterpret_cast(arg0)
            fclose(handle)
            return 0
        }
        
        // TODO
        //runtime["io", "_read", .]
    }
}
