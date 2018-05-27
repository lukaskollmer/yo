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

let tokens = try Lexer(source: rawSource).tokenize()

let parser = Parser(tokens: tokens)
let ast = try parser.parse()

let compiler = BytecodeCompiler()
let instructions = compiler.generateInstructions(for: ast)
//Log.info("\n\(instructions.fancyDescription)")

let interpreter = BytecodeInterpreter(instructions: instructions)
let retval = try interpreter.run()
print(retval)


