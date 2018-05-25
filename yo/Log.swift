//
//  Log.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

private enum Color : String {
    case yellow = "33"
    case red = "31"
}

struct Log {
    private init() {}
    
    static private func _log(label: String, message: String) {
        print("[yo:\(label)] \(message)")
    }
    
    static func info(_ message: String) {
        _log(label: "info", message: message)
    }
    
    static func error(_ message: String) {
        _log(label: "error", message: message)
        
        // TODO https://stackoverflow.com/a/27808423/2513803
        //print("\u{001B}[0;31m\(message)")
    }
}
