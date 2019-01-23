//
//  Log.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

var LogLevel: _LogLevel = .info

enum _LogLevel: UInt8, Comparable {
    case verbose    // Only if the verbose flag is set (-v)
    case info       // Default level
    // TODO add a debug option that only prints if this is a debug build
    
    static func < (lhs: _LogLevel, rhs: _LogLevel) -> Bool {
        return lhs.rawValue < rhs.rawValue
    }
}


private struct StdoutOutputStream: TextOutputStream {
    mutating func write(_ string: String) {
        string.utf8CString.withUnsafeBytes { ptr in
            _ = Darwin.write(STDOUT_FILENO, ptr.baseAddress!, string.count)
        }
    }
}

func log(_ level: _LogLevel, _ items: Any..., separator: String = " ", terminator: String = "\n") {
    guard level >= LogLevel else { return }
    
    var stdout = StdoutOutputStream()
    
    for (index, item) in items.enumerated() {
        let hasNext = index < items.endIndex.advanced(by: -1)
        stdout.write("[yo] ")
        Swift.print(item, terminator: hasNext ? separator : terminator, to: &stdout)
    }
}


// Make sure the current module can't use the print function
@available(*, unavailable, message: "Use the `log(_:_:`) function instead!", renamed: "log")
internal func print(_ items: Any...) {}
