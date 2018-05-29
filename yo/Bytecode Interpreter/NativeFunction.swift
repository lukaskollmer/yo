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
    
    private static func ns(_ ns: String, _ name: String) -> String {
        return SymbolMangling.mangleStaticMember(ofType: ns, memberName: name)
    }
    
    typealias NativeFunctionImp = (StackView<Int>) -> ()
    typealias NativeFunction = (argc: Int, address: Int, imp: NativeFunctionImp)
    
    static let builtins: [String: NativeFunction] = [
        ns("runtime", "alloc")  : (1, addr, alloc  ),
        ns("runtime", "retain") : (1, addr, retain ),
        ns("runtime", "release"): (1, addr, release),
        ns("io", "print")       : (1, addr, printf), // TODO make this variardic?
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
    
    private static func printf(_ stack: StackView<Int>) {
        let address = stack.heap[stack.peek()]
        let size = stack.heap[address]
        
        let start = address + 1
        let end = start + size
        
        let characters: [Character] = stack.heap[start..<end]
            .compactMap(UnicodeScalar.init)
            .map(Character.init)
        
        print(String(characters))
        try! stack.push(0)
    }
}
