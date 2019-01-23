//
//  CLI+Flags.swift
//  yo
//
//  Created by Lukas Kollmer on 2019-01-23.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

extension CLIOptions {
    static let help = CLIOption<Void>(
        flag: "--help",
        description: "Print this message"
    )
    
    static let verbose = CLIOption<Void>(
        flag: "-v",
        description: "Verbose output"
    )
    
    static let checkHeapEmpty = CLIOption<Void>(
        flag: "--check-heap-empty",
        description: "After running the program, checks whether the heap is empty and - if not - prints all remaining allocations"
    )
    
    static let printInstructions = CLIOption<Void>(
        flag: "--print-instructions",
        description: "Print pre-finalized instructions to stdout before passing them to the interpreter"
    )
    
    static let printHeap = CLIOption<Void>(
        flag: "--print-heap",
        description: "Print the contents of the heap after running the program"
    )
    
    static let heapSize = CLIOption<Int>(
        flag: "--heap-size",
        defaultValue: Int(1 << 12),
        description: "Specify the size of the heap"
    )
    
    static let stdlibPath = CLIOption<String>(
        flag: "--stdlib-path",
        defaultValue: "/Users/lukas/Developer/yo/stdlib",
        description: "Path of the standard library"
    )
    
    static let printAST = CLIOption<Void>(
        flag: "--print-ast",
        description: "Print the parsed AST"
    )
    
    static let logAllCalls = CLIOption<Void>(
        flag: "--log-calls",
        description: "Log all method calls"
    )
    
    static let recordCallStats = CLIOption<Void>(
        flag: "--record-call-stats",
        description: "Records all method invocations as a text file (similar to NSObjCMessageLoggingEnabled in Objective-C)"
    )
    
    static let emitSignposts = CLIOption<Void>(
        flag: "--emit-signposts",
        description: "(internal) Call `kdebug_signpost` et al to mark points of interest"
    )
    
    static let optimize = CLIOption<Void>(
        flag: "--optimize",
        description: "Apply some optimizations"
    )
    
    static let all: [CLIOptions] = [.help, .verbose, .checkHeapEmpty, .printInstructions, .printHeap, .heapSize, .stdlibPath, .printAST, .logAllCalls, .recordCallStats, .emitSignposts, .optimize]
}

