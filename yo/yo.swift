//
//  yo.swift
//  yo
//
//  Created by Lukas Kollmer on 27.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


enum yo {
    
    static var workingDirectory: String = FileManager.default.currentDirectoryPath
    
    
    static func run(atPath path: String) throws -> Int {
        let instructions = try compile(atPath: path)
        
        let interpreter = BytecodeInterpreter(instructions: instructions.finalized())
        return try interpreter.run()
    }
    
    
    static func parse(file path: String) throws -> [ASTNode] {
        guard
            let data = FileManager.default.contents(atPath: path),
            let rawSource = String(data: data, encoding: .utf8)
            else {
                throw NSError(domain: "yo", code: 0) // TODO
        }
        
        let tokens = try Lexer(source: rawSource).tokenize()
        
        let parser = Parser(tokens: tokens)
        return try parser.parse()
    }
    
    
    static func compile(atPath path: String, includeBootstrappingCode: Bool = true) throws -> [WIPInstruction] {
        let compiler = BytecodeCompiler()
        let instructions = try compiler.generateInstructions(for: try parse(file: path), includeBootstrappingCode: includeBootstrappingCode)
        
        Log.info("\n\(instructions.fancyDescription)")
        return instructions
    }
}
