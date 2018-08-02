//
//  yo.swift
//  yo
//
//  Created by Lukas Kollmer on 27.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


enum yo {
    
    //static var workingDirectory: String = FileManager.default.currentDirectoryPath
    
    
    static func read(file path: String) throws -> String {
        guard
            let data = FileManager.default.contents(atPath: path),
            let rawSource = String(data: data, encoding: .utf8)
            else {
                throw NSError(domain: "yo", code: 0) // TODO
        }
        
        return rawSource
    }
    
    
    
    static func tokenize(atPath path: String) throws -> [ASTNode] {
        let tokens = try Lexer(source: try read(file: path)).tokenize()
        return try Parser(tokens: tokens).parse()
    }
    
    
    static func run(atPath path: String, heapSize: Int) throws -> Int {
        if CLI.isVerbose {
            Log.info("Input file: \(filepath)")
        }
        
        let code = try read(file: path)
        let tokens = try Lexer(source: code).tokenize()
        let ast = try Parser(tokens: tokens).parse()
        
        var (instructions, stats) = try BytecodeCompiler().compile(ast: ast)
        
        instructions = instructions.withArrayLiteralsResolved()
        
        // TODO optimize
        
        //let optimizer = Optimizer(instructions: instructions, ast: ast, stats: stats)
        //instructions = optimizer.optimize([.unusedSymbols]) // TODO
        
        instructions = instructions.withLabelsPadded()
        
        if CLI.printInstructions || CLI.isVerbose {
            Log.info("\n\(instructions.fancyDescription)")
        }
        
        let interpreter = BytecodeInterpreter(instructions: instructions.finalized(), heapSize: heapSize)
        
        let retval = try interpreter.run()
        
        if CLI.printHeap || CLI.isVerbose {
            print("heap after: \(interpreter.heap.backing)")
            Log.info("main returned with exit code \(retval)")
        }
        
        if CLI.checkHeapEmpty {
            // the second part (checking that all allocations have been freed is arguably a bad idea since there's no actual reason to free everything before the program exits
            // also, there's always going to be at least one allocation, since we have to make sure no object can get address 0
            let heapEmpty = interpreter.heap.backing.all { $0 == 0 } && interpreter.heap.allocations.isEmpty
            Log.info("Heap empty: \(heapEmpty)")
            
            if !heapEmpty {
                Log.info("allocations: \(interpreter.heap.allocations)")
            }
        }
        
        return retval
    }
}
