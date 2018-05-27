//
//  main.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


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

Log.info("Input file: \(filepath)")

guard FileManager.default.fileExists(atPath: filepath) else {
    fatalError("input file does not exist")
}

let retval = try yo.run(atPath: filepath)
Log.info("main returned with exit code \(retval)")

