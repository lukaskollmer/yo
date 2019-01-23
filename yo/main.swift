//
//  main.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

let VERSION = "0.0.1"


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

if CLI.hasFlag(.help) {
    printHelpAndExit()
}


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
