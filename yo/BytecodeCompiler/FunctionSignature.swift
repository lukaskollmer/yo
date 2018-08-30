//
//  FunctionSignature.swift
//  yo
//
//  Created by Lukas Kollmer on 30.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


protocol FunctionSignature {
    var parameterTypes: [ASTType] { get }
    var returnType: ASTType { get }
    var annotations: [ASTAnnotation.Element] { get }
    
    var kind: FunctionKind { get }
}

extension FunctionSignature {
    var argc: Int {
        return parameterTypes.count
    }
    
    var isVariadic: Bool {
        return annotations.contains(.variadic)
    }
    
    var kind: FunctionKind {
        return .global
    }
}


enum FunctionKind: Equatable {
    case global              // a global function
    case impl(String)        // instance function for some type
    case staticImpl(String)  // static function for some type
    
    func withTypename(_ typename: String) -> FunctionKind {
        switch self {
        case .global:
            return .global
        case .impl(_):
            return .impl(typename)
        case .staticImpl(_):
            return .staticImpl(typename)
        }
    }
}
