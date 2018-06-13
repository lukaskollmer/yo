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
    
    typealias NativeFunctionImp = (BytecodeInterpreter) -> Int
    typealias NativeFunction = (name: String, argc: Int, address: Int, imp: NativeFunctionImp)
    
    static let builtins: [NativeFunction] = [
        (ns("runtime", "alloc"),            1, addr, runtime_alloc),
        (ns("runtime", "free"),             1, addr, runtime_free),
        (ns("runtime", "dealloc"),          1, addr, { _ in 0 }),   // implemented manually in BytecodeInterpreter.run
        (ns("io", "print"),                 1, addr, io_print),    // TODO make this variardic?
        (ns("io", "printi"),                1, addr, io_printi),
        (ns("io", "printf"),                2, addr, io_printf),
        (ns("runtime", "fatalError"),       1, addr, runtime_fatalError),
        (ns("runtime", "sort"),             2, addr, runtime_sort),
        (ns("runtime", "sortf"),            3, addr, runtime_sortf),
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
    
    
    private static func runtime_fatalError(_ interpreter: BytecodeInterpreter) -> Int {
        fatalError(getString(atAddress: interpreter.stack.peek(), heap: interpreter.stack.heap))
    }
    
    
    // MARK: memory
    
    private static func runtime_alloc(_ interpreter: BytecodeInterpreter) -> Int {
        let size = interpreter.stack.peek()
        let address = interpreter.heap.alloc(size: size)
        return address
    }
    
    private static func runtime_free(_ interpreter: BytecodeInterpreter) -> Int {
        interpreter.stack.heap.free(address: interpreter.stack.peek())
        return 0;
    }
    
    // MARK: sorting
    private static func runtime_sort(_ interpreter: BytecodeInterpreter) -> Int {
        let address = interpreter.stack.peek()
        let count = interpreter.stack.peek(offset: -1)
        interpreter.heap.sort(address: address, count: count, fn: <)
        return 0
    }
    
    // the sorting function should return 1 if the first parameter should be ordered before the second one, otherwise 0
    private static func runtime_sortf(_ interpreter: BytecodeInterpreter) -> Int {
        let address = interpreter.stack.peek()
        let count = interpreter.stack.peek(offset: -1)
        let fn_address = interpreter.stack.peek(offset: -2)
        
        interpreter.heap.sort(address: address, count: count) { a, b in
            let areInIncreasingOrder = try! interpreter.call(address: fn_address, arguments: [a, b])
            return Bool(areInIncreasingOrder == 1)
        }
        
        return 0
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
    private static func io_print(_ interpreter: BytecodeInterpreter) -> Int {
        print(getString(atAddress: interpreter.stack.peek(), heap: interpreter.heap))
        return 0
    }
    
    // print an integer
    private static func io_printi(_ interpreter: BytecodeInterpreter) -> Int {
        print(interpreter.stack.peek())
        return 0;
    }
    
    
    private static func io_printf(_ interpreter: BytecodeInterpreter) -> Int {
        // TODO WIP
        //let string = getString(atAddress: stack.peek() + 1, heap: stack.heap)
        //print(stack.peek(offset: -1))
        return 0;
    }
}
