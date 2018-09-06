//
//  main.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

let VERSION = "0.0.1"


extension CLIOptions {
    static let help = CLIOption<Void>(
        flag: "-help",
        description: "Print this message"
    )
    
    static let verbose = CLIOption<Void>(
        flag: "-v",
        description: "Verbose output"
    )
    
    static let checkHeapEmpty = CLIOption<Void>(
        flag: "-check-heap-empty",
        description: "After running the program, checks whether the heap is empty and - if not - prints all remaining allocations"
    )
    
    static let printInstructions = CLIOption<Void>(
        flag: "-print-instructions",
        description: "Print pre-finalized instructions to stdout before passing them to the interpreter"
    )
    
    static let printHeap = CLIOption<Void>(
        flag: "-print-heap",
        description: "Print the contents of the heap after running the program"
    )
    
    static let heapSize = CLIOption<Int>(
        flag: "-heap-size",
        defaultValue: Int(1 << 12),
        description: "Specify the size of the heap"
    )
    
    static let stdlibPath = CLIOption<String>(
        flag: "-stdlib-path",
        defaultValue: "/Users/lukas/Developer/yo/stdlib",
        description: "Path of the standard library"
    )
    
    static let printAST = CLIOption<Void>(
        flag: "-print-ast",
        description: "Print the parsed AST"
    )
    
    static let logAllCalls = CLIOption<Void>(
        flag: "-log-calls",
        description: "Log all method calls"
    )
    
    static let recordCallStats = CLIOption<Void>(
        flag: "-record-call-stats",
        description: "Records all method invocations as a text file (similar to NSObjCMessageLoggingEnabled in Objective-C)"
    )
    
    static let emitSignposts = CLIOption<Void>(
        flag: "-emit-signposts",
        description: "(internal) Call `kdebug_signpost` et al to mark points of interest"
    )
    
    static let optimize = CLIOption<Void>(
        flag: "--optimize",
        description: "Apply some optimizations"
    )
    
    static let all: [CLIOptions] = [.help, .verbose, .checkHeapEmpty, .printInstructions, .printHeap, .heapSize, .stdlibPath, .printAST, .logAllCalls, .recordCallStats, .emitSignposts, .optimize]
}


func printHelpAndExit() -> Never {
    var message = "OVERVIEW: yo programming language v\(VERSION)"
    message += "\n\n"
    message += "USAGE: yo [options] <input>"
    message += "\n\n"
    message += "OPTIONS:"
    message += "\n"
    
    let optionsSorted = CLIOptions.all.sorted { $0.flag < $1.flag }
    
    for option in optionsSorted {
        
        let maxColumnWidth = 24
        var entry = "  "
        entry += option.flag
        
        if option.hasDefaultValue {
            entry += " <value>"
        }
        
        if option.flag.count < maxColumnWidth {
            entry = entry.padding(.right, toLength: maxColumnWidth, withPad: " ")
        } else {
            entry += "\n" + String.init(repeating: " ", count: maxColumnWidth + 2)
        }
        
        entry += option.description
        
        if option.hasDefaultValue {
            entry += " (default: \(option._defaultValue_description!))"
        }
        entry += "\n"
        message += entry
    }
    
    print(message)
    exit(EXIT_SUCCESS)

}

// This is the only exception where we allow a flag w/ 2 dashes
if CLI.arguments.contains("--help") || CLI.hasFlag(.help) {
    printHelpAndExit()
}


//guard let filename = CLI.arguments[safe: 1] else {
//    printHelpAndExit()
//}
let filename = CLI.arguments.last!


let filepath: String = {
    if filename.hasPrefix("/") {
        return filename
    } else if filename.hasPrefix("~") {
        return filename.ns.expandingTildeInPath
    } else {
        return FileManager.default.currentDirectoryPath.appending(pathComponent: filename)
    }
}()

guard FileManager.default.fileExists(atPath: filepath) else {
    fatalError("input file does not exist")
}

let heapSize = CLI.value(of: .heapSize)
exit(try Int32(yo.run(atPath: filepath, heapSize: heapSize)))

