//
//  ASTType.swift
//  yo
//
//  Created by Lukas Kollmer on 08.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


/// An `ASTType` describes the type of an identifier: whether it is primitive, complex or a function pointer
indirect enum ASTType: Equatable, CustomStringConvertible {
    case primitive(name: String)    // int/void
    case complex(name: String)      // something declared w/ the `type` keyword
    case function(returnType: ASTType, parameterTypes: [ASTType])   // a function pointer
    case unresolved
    
    static let `int` = ASTType.primitive(name: "int")
    static let `any` = ASTType.primitive(name: "any")
    static let void = ASTType.primitive(name: "void")
    
    var description: String {
        switch self {
        case .primitive(let name):
            return name
        case .complex(let name):
            return name
        case .function(let returnType, let parameterTypes):
            return "fn<(\(parameterTypes)): \(returnType)>"
        case .unresolved:
            return "unresolved"
        }
    }
    
    
    func isCompatible(with other: ASTType) -> Bool {
        return self == other || [self, other].contains(.any)
    }
}
