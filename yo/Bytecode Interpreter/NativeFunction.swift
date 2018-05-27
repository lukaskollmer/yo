//
//  NativeFunction.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


extension Runtime {
    
    private static var _address = -1000
    private static var addr: Int {
        _address += 1
        return _address
    }
    
    private static func name(_ name: String) -> String {
        return SymbolMangling.mangleStaticMember(ofType: "runtime", memberName: name)
    }
    
    typealias NativeFunctionImp = (StackView<Int>) -> ()
    typealias NativeFunction = (argc: Int, address: Int, imp: NativeFunctionImp)
    
    static let builtins: [String: NativeFunction] = [
        name("alloc")  : (1, addr, alloc  ),
        name("retain") : (1, addr, retain ),
        name("release"): (1, addr, release),
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
        let address = stack.heap.alloc(size: size)
        //stack.heap.retain(address: address)
        try! stack.push(address)
    }
    
    private static func retain(_ stack: StackView<Int>) {
        stack.heap.retain(address: stack.peek())
        try! stack.push(0)
    }
    
    private static func release(_ stack: StackView<Int>) {
        stack.heap.release(address: stack.peek())
        try! stack.push(0)
    }
}
