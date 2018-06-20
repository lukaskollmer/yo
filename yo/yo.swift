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
        return try tokenize(code: read(file: path))
    }
    
    
    static func tokenize(code: String) throws -> [ASTNode] {
        let tokens = try Lexer(source: code).tokenize()
        return try Parser(tokens: tokens).parse()
    }
    
    
    
    static func compile(atPath path: String) throws -> [WIPInstruction] {
        return try compile(code: try read(file: path))
    }
    
    
    static func compile(code: String) throws -> [WIPInstruction] {
        return try BytecodeCompiler().compile(ast: try tokenize(code: code))
    }
    
    
    static func run(atPath path: String) throws -> Int {
        return try run(code: read(file: path))
    }
    
    static func run(code: String) throws -> Int {
        let instructions = try compile(code: code).withArrayLiteralsResolved().withLabelsPadded()
        Log.info("\n\(instructions.fancyDescription)")
        
        return try BytecodeInterpreter(instructions: instructions.finalized()).run()
    }
}
