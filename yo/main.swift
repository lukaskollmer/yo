//
//  main.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

func printHelpAndExit() -> Never {
    let message =
    """
yo

    Options:
    --verbose
    
    Flags:
    -heap-size (Default: 1024)
"""
    
    print(message)
    exit(EXIT_SUCCESS)

}

if CLI.hasArgument("--help") {
    printHelpAndExit()
}


guard let filename = CLI.arguments[safe: 1] else {
    printHelpAndExit()
}


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

let heapSize = CLI.value(ofFlag: "-heap-size", type: Int.self) ?? 1 << 10
exit(try Int32(yo.run(atPath: filepath, heapSize: heapSize)))

