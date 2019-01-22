//
//  ASTIdentifier.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTIdentifier: ASTExpression, Hashable, ExpressibleByStringLiteral, CustomStringConvertible {
    let value: String
    let isBuiltin: Bool
    
    
    init(value: String, isBuiltin: Bool = false) {
        self.value = value
        self.isBuiltin = isBuiltin
    }
    
    convenience init(_ value: String) {
        self.init(value: value, isBuiltin: false)
    }
    
    convenience init(builtin value: String) {
        self.init(value: value, isBuiltin: true)
    }
    
    convenience required init(stringLiteral value: String) {
        self.init(value: value, isBuiltin: false)
    }
    
    var description: String {
        return "<ASTIdentifier name='\(isBuiltin ? "#" : "")\(value)'>"
    }
    
    var hashValue: Int {
        return value.hashValue ^ isBuiltin.hashValue
    }
    
    static func == (lhs: ASTIdentifier, rhs: ASTIdentifier) -> Bool {
        return lhs.value == rhs.value && lhs.isBuiltin == rhs.isBuiltin
    }
}

extension ASTIdentifier {
    static let unknown: ASTIdentifier = ""
}

