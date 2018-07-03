//
//  CLI.swift
//  yo
//
//  Created by Lukas Kollmer on 03.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class CLI {
    static let arguments = ProcessInfo.processInfo.arguments
    
    static let isVerbose = hasArgument("--verbose")
    
    static func hasArgument(_ argument: String) -> Bool {
        return arguments.contains(argument)
    }
    
    static func value<T>(ofFlag flag: String, type: T.Type) -> T? {
        guard let index = arguments.index(of: flag)?.advanced(by: 1) else {
            return nil
        }
        let value = arguments[index]
        
        if type == Int.self {
            return Int(value) as? T
        } else if type == String.self {
            return value as? T
        }
        
        return nil
    }
}
