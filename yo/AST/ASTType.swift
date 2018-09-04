//
//  ASTType.swift
//  yo
//
//  Created by Lukas Kollmer on 08.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

func sizeof(_ type: ASTType) -> Int {
    return type.size
}


/// An `ASTType` describes the type of an identifier: whether it is primitive, complex or a function pointer
indirect enum ASTType: Equatable, CustomStringConvertible {
    case primitive(name: String)    // int/void
    case complex(name: String)      // something declared w/ the `type` or `struct` keyword
    case function(returnType: ASTType, parameterTypes: [ASTType])   // a function pointer
    case ref(ASTType)
    case _enum(String)
    case unresolved
    
    static let primitives: [ASTType] = [.bool, .i8, .i16, .i32, .i64, .int, .double, .any, .void]
    static let primitiveTypenames = ASTType.primitives.map { $0.typename }
    static let intTypes = [ASTType.i8, .i16, .i32, .i64, .int]
    
    static let bool   = ASTType.primitive(name: "bool")
    
    static let i8     = ASTType.primitive(name: "i8")
    static let i16    = ASTType.primitive(name: "i16")
    static let i32    = ASTType.primitive(name: "i32")
    static let i64    = ASTType.primitive(name: "i64")
    
    static let double = ASTType.primitive(name: "double")
    
    static let any    = ASTType.primitive(name: "any")
    static let id     = ASTType.complex(name: "id")
    static let void   = ASTType.primitive(name: "void")
    
    static let `Self` = ASTType.complex(name: "Self")
    
    static let Array  = ASTType.complex(name: "Array")
    static let String = ASTType.complex(name: "String")
    
    //static let int    = ASTType.i64
    //static let intptr = ASTType.i64
    static let int = ASTType.primitive(name: "int")
    
    
    // size, in bytes
    var size: Int {
        switch self {
        case .i8, .bool:
            return 1
        case .i16:
            return 2
        case .i32:
            return 4
        case .i64, .int, .double, .complex(_), .any, .function(_, _), ._enum(_), .ref(_):
            return 8
        default:
            fatalError("Unable to determine size of \(self.typename)")
        }
    }
    
    
    var description: String {
        switch self {
        case .primitive(let name):
            return "ASTType.primitive(\(name))"
        case .complex(let name):
            return "ASTType.complex(\(name))"
        case .function(let returnType, let parameterTypes):
            return "ASTType.function<(\(parameterTypes)): \(returnType)>"
        case .ref(let type):
            return "ASTType.ref(\(type))"
        case ._enum(let name):
            return "ASTType._enum(\(name))"
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
        case .ref(let type):
            return "ref " + type.typename
        case ._enum(let name):
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
    
    var isFunction: Bool {
        if case .function(_) = self {
            return true
        }
        return false
    }
    
    var supportsReferenceCounting: Bool {
        return isComplex || isFunction
    }
    
    
    func isCompatible(with other: ASTType) -> Bool {
        let both = [self, other]
        
        if self == other || both.contains(.any) {
            return true
        }
        
        if both.contains(.id) && both.all({ $0.isComplex }) {
            return true
        }
        
        if both.all(ASTType.intTypes.contains) && self.size <= other.size {
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
