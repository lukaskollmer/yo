//
//  main.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

let arguments = ProcessInfo.processInfo.arguments

if arguments.count > 1 && arguments[1] == "--help" {
    let message =
"""
yo
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

exit(try Int32(yo.run(atPath: filepath)))

