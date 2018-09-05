//
//  Unsafe.swift
//  yo
//
//  Created by Lukas Kollmer on 05.09.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

// Collection of terrible hacks that actually seem to work fine
enum LK_Unsafe {
    static func call<T, R, U>(_ fn: (T, T) -> R, arg0: U, arg1: U) -> R {
        return withoutActuallyEscaping(fn) {
            let fn = unsafeBitCast($0, to: ((U, U) -> R).self)
            return fn(arg0, arg1)
        }
    }
    
    
    static func eval_binop<T>(lhs: Int, rhs: Int, type: T.Type, fn: (T, T) -> T) -> Int {
        if type == Int.self {
            return call(fn, arg0: lhs, arg1: rhs) as! Int
            
        } else if type == Double.self {
            let retval = call(fn, arg0: lhs.unsafe_loadAsDouble, arg1: rhs.unsafe_loadAsDouble) as! Double
            return retval.unsafe_loadAsInt
            
        } else {
            fatalError("Only Int and Double are supported types!")
        }
    }
    
    
    static func eval_comp<T>(lhs: Int, rhs: Int, type: T.Type, fn: (T, T) -> Bool) -> Bool {
        let retval: Bool
        
        if type == Int.self {
            retval = call(fn, arg0: lhs, arg1: rhs)
            
        } else if type == Double.self {
            retval = call(fn, arg0: lhs.unsafe_loadAsDouble, arg1: rhs.unsafe_loadAsDouble)
            
        } else {
            fatalError("Unsupported type!")
        }
        
        return retval
    }

}
