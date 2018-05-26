//
//  NativeFunction.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


extension Runtime {
    
    typealias NativeFunctionImp = (StackView<Int>) -> ()
    typealias NativeFunction = (argc: Int, address: Int, imp: NativeFunctionImp)
    
    static let builtins: [String: NativeFunction] = [
        "alloc"    : (1, -998, alloc)
    ]
    
    
    static func getNativeFunction(withAddress address: Int) -> NativeFunction {
        if let builtin = builtins.first(where: { $0.value.address == address }) {
            return builtin.value
        }
        fatalError("no native func at address \(address)")
    }
    
    
    // MARK: Native functions
    
    private static func alloc(_ stack: StackView<Int>) {
        let size = stack.peek()
        try! stack.push(stack.heap.alloc(size: size))
    }
}
