//
//  CLI.swift
//  yo
//
//  Created by Lukas Kollmer on 03.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


protocol CLIOptionExpressible {
    static func convert(stringValue: String) -> Self
}

extension String: CLIOptionExpressible {
    static func convert(stringValue: String) -> String {
        return stringValue
    }
}

extension Int: CLIOptionExpressible {
    static func convert(stringValue: String) -> Int {
        return Int(stringValue)!
    }
}


// Why such a weird setup (names, description in superclass, _defaultValue_description which is nil by default, etc)?
// We generate the `--help` message from an `Array<CLIFlags>`, meaning that we have to somehow work around the generic specializations

class CLIOptions {
    let flag: String
    let description: String
    
    fileprivate init(flag: String, description: String) {
        self.flag = flag
        self.description = description
    }
    
    var _defaultValue_description: String? {
        return nil
    }
    
    var hasDefaultValue: Bool {
        return _defaultValue_description != nil
    }
}

// Void argument: the presence of the argument is significant
// Other type: the next argument should be a value of the specified type
class CLIOption<T>: CLIOptions {
    let defaultValue: T!
    
    init(flag: String, defaultValue: T! = nil, description: String) {
        self.defaultValue = defaultValue
        super.init(flag: flag, description: description)
    }
    
    override var _defaultValue_description: String? {
        return defaultValue == nil ? nil : String(describing: defaultValue!)
    }
}


class CLI {
    
    static let arguments = ProcessInfo.processInfo.arguments
    
    static func hasFlag<T>(_ option: CLIOption<T>) -> Bool {
        return arguments.contains(option.flag)
    }
    
    static func value<T: CLIOptionExpressible>(of option: CLIOption<T>) -> T {
        guard let index = arguments.index(of: option.flag)?.advanced(by: 1) else {
            return option.defaultValue
        }
        let value = arguments[index]
        
        return T.convert(stringValue: value)
    }
}


class LKYOCLI: NSObject {
    @objc class func hasFlag(_ name: String) -> Bool {
        return CLI.arguments.contains(name)
    }
    
    @objc class func emitSignpostsFlag() -> String {
        return CLIOptions.emitSignposts.flag
    }
}
