//
//  main.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

let filename: String = {
    if ProcessInfo.processInfo.arguments.count > 1 {
        return ProcessInfo.processInfo.arguments[1]
    } else {
        return "~/Developer/yo/main.yo"
    }
}()

let filepath: String = {
    if filename.hasPrefix("/") {
        return filename
    } else if filename.hasPrefix("~") {
        return NSString(string: filename).expandingTildeInPath
    } else {
        return FileManager.default.currentDirectoryPath.appending(pathComponent: filename)
    }
}()

Log.info("Input file: \(filepath)")

guard FileManager.default.fileExists(atPath: filepath) else {
    fatalError("input file does not exist")
}

guard
    let data = FileManager.default.contents(atPath: filepath),
    let rawSource = String(data: data, encoding: .utf8)
else {
    Log.error("Unable to read file")
    exit(1)
}


//let tokens = Tokenizer.tokenize(source: rawSource)
Log.info("")
Log.info("")
Log.info("===== LEXER =====")
Log.info("")
let tokens = try Lexer(source: rawSource).tokenize()
//Log.info("tokens: \(tokens)")

Log.info("")
Log.info("")
Log.info("===== PARSER =====")
Log.info("")
let parser = Parser(tokens: tokens)
let ast = try parser.parse()
Log.info("ast: \(ast)")


Log.info("")
Log.info("")
Log.info("===== Codegen =====")
Log.info("")
let compiler = BytecodeCompiler(ast: ast)
let instructions = compiler.generateInstructions()

Log.info("generated instructions")
for (index, instruction) in instructions.enumerated() {
    let info = InstructionDescriptor(instruction: instruction)
    Log.info("  [\(String(format:"%02d", index))] \(info.operation) \(info.immediate)")
}


print(try BytecodeInterpreter(instructions: instructions).run())


