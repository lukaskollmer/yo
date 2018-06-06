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
    
    typealias NativeFunctionImp = (Stack) -> Int
    typealias NativeFunction = (name: String, argc: Int, address: Int, imp: NativeFunctionImp)
    
    static let builtins: [NativeFunction] = [
        (ns("runtime", "alloc"),            1, addr, runtime_alloc),
        (ns("runtime", "free"),             1, addr, runtime_free),
        (ns("runtime", "dealloc"),          1, addr, { _ in 0 }),   // implemented manually in BytecodeInterpreter.run
        (ns("io", "print"),                 1, addr, io_print),    // TODO make this variardic?
        (ns("io", "printi"),                1, addr, io_printi),
        (ns("io", "printf"),                2, addr, io_printf),
        (ns("runtime", "fatalError"),       1, addr, runtime_fatalError),
    ]
    
    
    static func getNativeFunction(withAddress address: Int) -> NativeFunction {
        if let builtin = builtins.first(where: { $0.address == address }) {
            return builtin
        }
        fatalError("no native func at address \(address)")
    }
    
    
    static func builtin(withName name: String) -> NativeFunction? {
        return builtins.first { $0.name == name }
    }
    
    
    // MARK: Native functions
    
    
    private static func runtime_fatalError(_ stack: Stack) -> Int {
        fatalError(getString(atAddress: stack.peek(), heap: stack.heap))
    }
    
    
    // MARK: memory
    
    private static func runtime_alloc(_ stack: Stack) -> Int {
        let size = stack.peek()
        let address = stack.heap.alloc(size: size)
        //stack.heap.retain(address: address)
        return address
    }
    
    private static func runtime_free(_ stack: Stack) -> Int {
        stack.heap.free(address: stack.peek())
        return 0;
    }
    
    
    // MARK: io
    
    private static func getString(atAddress _address: Int, heap: Heap) -> String {
        let address = heap[_address + 1]
        let size = heap[address]
        
        let start = address + 1
        let end = start + size
        
        let characters: [Character] = heap[start..<end]
            .compactMap(UnicodeScalar.init)
            .map(Character.init)
        
        return String(characters)
    }
    
    // print a string
    private static func io_print(_ stack: Stack) -> Int {
        print(getString(atAddress: stack.peek(), heap: stack.heap))
        return 0
    }
    
    // print an integer
    private static func io_printi(_ stack: Stack) -> Int {
        print(stack.peek())
        return 0;
    }
    
    
    private static func io_printf(_ stack: Stack) -> Int {
        // TODO WIP
        //let string = getString(atAddress: stack.peek() + 1, heap: stack.heap)
        //print(stack.peek(offset: -1))
        return 0;
    }
}
