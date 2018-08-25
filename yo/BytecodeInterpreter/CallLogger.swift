//
//  CallLogger.swift
//  yo
//
//  Created by Lukas Kollmer on 25.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// Logs function entry and exit events with automatic indentation
class CallLogger {
    private var level = 0
    
    func didEnterFunction(withName name: String) {
        print(getIndentString(), "CALL", name)
        level += 1
    }
    
    func didExitFunction(withName name: String) {
        level -= 1
        print(getIndentString(), "RET ", name)
    }
    
    
    private func getIndentString() -> String {
        return String.init(repeating: "    ", count: level)
    }
}
