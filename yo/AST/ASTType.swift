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
    static let id    = ASTType.complex(name: "id")
    static let void  = ASTType.primitive(name: "void")
    
    static let `Self` = ASTType.complex(name: "Self")
    
    static let Array  = ASTType.complex(name: "Array")
    static let String = ASTType.complex(name: "String")
    
    var description: String {
        switch self {
        case .primitive(let name):
            return "ASTType.primitive(\(name))"
        case .complex(let name):
            return "ASTType.complex(\(name))"
        case .function(let returnType, let parameterTypes):
            return "ASTType.function<(\(parameterTypes)): \(returnType)>"
        case .unresolved:
            return "unresolved"
        }
    }
    
    var typename: String {
        switch self {
        case .primitive(let name):
            return name
        case .complex(let name):
            return name
        case .function(let returnType, let parameterTypes):
            let params: String = parameterTypes.enumerated().reduce(into: "") {
                $0 += $1.element.typename
                if $1.offset < parameterTypes.count - 1 {
                    $0 += ", "
                }
            }
            return "fn<(\(params)): \(returnType.typename)>"
        case .unresolved:
            return "unresolved"
        }
    }
    
    var isComplex: Bool {
        if case .complex(_) = self {
            return true
        }
        return false
    }
    
    
    func isCompatible(with other: ASTType) -> Bool {
        let both = [self, other]
        
        if self == other || both.contains(.any) {
            return true
        }
        
        if both.contains(.id) && both.all({ $0.isComplex }) {
            return true
        }
        
        if case .function(let retval1, let parameterTypes1) = self, case .function(let retval2, let parameterTypes2) = other {
            return
                retval1.isCompatible(with: retval2)
                && parameterTypes1.count == parameterTypes2.count
                && parameterTypes1.enumerated().reduce(true) { $0 && $1.element.isCompatible(with: parameterTypes2[$1.offset]) }
        }
        
        return false // TODO is that the right decision?
    }
}
