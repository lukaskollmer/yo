//
//  Runtime.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


enum Runtime {
    
    typealias NativeFunctionImp = (Stack<Int>) -> ()
    typealias NativeFunction = (argc: Int, address: Int, imp: NativeFunctionImp)
    
    static let builtins: [String: NativeFunction] = [
        "__version": (0, -999, printVersion),
        "__add"    : (2, -998, add)
    ]
    
    
    static func getNativeFunction(withAddress address: Int) -> NativeFunction {
        if let builtin = builtins.first(where: { $0.value.address == address }) {
            return builtin.value
        }
        fatalError("no native func at address \(address)")
    }
    
    
    private static func printVersion(_ stack: Stack<Int>) {
        print("OH MY FUCKING GOD THIS ACTUALLY WORKS")
        try! stack.push(-100)
    }
    
    private static func add(_ stack: Stack<Int>) {
        try! stack.push(stack.peek() + stack.peek(offset: -1))
    }
}
