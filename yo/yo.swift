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
                Log.error("Unable to read file '\(path)'")
                throw NSError(domain: "yo", code: 0) // TODO
        }
        
        return rawSource
    }
    
    
    
    static func parse(atPath path: String) throws -> AST {
        return try Parser().parse(atPath: path)
    }
    
    
    static private var _importedPaths = [String]()
    static func resolveImports(in ast: AST, currentWorkingDirectory: String) throws -> AST {
        // TODO This is terrible code
        
        // why is this a local function, insetad of a closure?
        // closured can't be recursive, but functions can
        func resolveImports(in ast: AST) throws -> AST {
            return try ast.lk_flatMap { node -> AST in
                if let importStatement = node as? ASTImportStatement {
                    let path = ImportPathResolver.resolve(moduleName: importStatement.moduleName, currentWorkingDirectory: currentWorkingDirectory)
                    guard !_importedPaths.contains(path) else { return [] }
                    
                    _importedPaths.append(path)
                    return try resolveImports(in: try yo.parse(atPath: path))
                }
                return [node]
            }
        }
        
        return try resolveImports(in: ast)
    }
    
    
    static func run(atPath path: String, heapSize: Int) throws -> Int {
        if CLI.hasFlag(.verbose) {
            Log.info("Input file: \(filepath)")
        }
        
        Profiling.recordStart(event: .parseAST)
        var ast = try parse(atPath: path)
        ast = try resolveImports(in: ast, currentWorkingDirectory: path.directory)
        LKYOParser.sharedInstance()._printFilePathIndexes()
        Profiling.recordEnd(event: .parseAST)
        
        // TODO optimize
        if CLI.hasFlag(.optimize) {
            ast = Optimizer().optimize(ast: ast)
        }
        
        Profiling.recordStart(event: .compile)
        var instructions = try BytecodeCompiler().compile(ast: ast)
        Profiling.recordEnd(event: .compile)
        
        instructions = instructions.withArrayLiteralsResolved()
        instructions = instructions.withLabelsPadded()
        
        if CLI.hasFlag(.printInstructions) {
            Log.info("\n\(instructions.fancyDescription)")
        }
        
        let interpreterDebugOptions: BytecodeInterpreter.DebugOptions = {
            var options = BytecodeInterpreter.DebugOptions()
            
            if CLI.hasFlag(.logAllCalls) {
                options.insert(.logCallEvents)
            }
            
            if CLI.hasFlag(.recordCallStats) {
                options.insert(.recordCallStats)
            }
            
            return options
        }()
        
        let interpreter = BytecodeInterpreter(unresolvedInstructions: instructions, heapSize: heapSize, debugOptions: interpreterDebugOptions)
        
        Profiling.recordStart(event: .interpret)
        let start_timestamp = Date()
        let retval = try interpreter.run()
        Profiling.recordEnd(event: .interpret)
        
        if CLI.hasFlag(.printHeap) {
            print("heap after: \(interpreter.heap.backing)")
        }
        
        Log.info("Run duration: \(abs(start_timestamp.timeIntervalSinceNow))")
        Log.info("main returned with exit code \(retval)")
        
        if CLI.hasFlag(.checkHeapEmpty) {
            // the second part (checking that all allocations have been freed is arguably a bad idea since there's no actual reason to free everything before the program exits
            // also, there's always going to be at least one allocation, since we have to make sure no object can get address 0
            let heapEmpty = interpreter.heap.backing.asArray(ofType: Int.self).all { $0 == 0 } && interpreter.heap.allocations.isEmpty
            Log.info("Heap empty: \(heapEmpty)")
            
            if !heapEmpty {
                Log.info("allocations: \(interpreter.heap.allocations)")
            }
        }
        
        return retval
    }
}
