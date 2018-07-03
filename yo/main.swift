//
//  main.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

if CLI.hasArgument("--help") {
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



// TODO set this dynamically!
yo.workingDirectory = "/Users/lukas/Developer/yo"

let filename: String = {
    if ProcessInfo.processInfo.arguments.count > 1 {
        return ProcessInfo.processInfo.arguments[1]
    } else {
        return "main.yo"
    }
}()

let filepath: String = {
    if filename.hasPrefix("/") {
        return filename
    } else if filename.hasPrefix("~") {
        return NSString(string: filename).expandingTildeInPath
    } else {
        return yo.workingDirectory.appending(pathComponent: filename)
    }
}()

guard FileManager.default.fileExists(atPath: filepath) else {
    fatalError("input file does not exist")
}

let heapSize = CLI.value(ofFlag: "-heap-size", type: Int.self) ?? 1 << 10
exit(try Int32(yo.run(atPath: filepath, heapSize: heapSize)))

