//
//  ASTFunctionDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

class ASTFunctionDeclaration: ASTStatement, ASTTypeThatCanBeAnnotated, FunctionSignature {
    enum Kind: Equatable {
        case global              // a global function
        case impl(String)        // instance function for some type
        case staticImpl(String)  // static function for some type
        
        func withTypename(_ typename: String) -> Kind {
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
    
    let name: ASTIdentifier
    let parameters: [ASTVariableDeclaration]
    var returnType: ASTType
    var kind: Kind
    let body: ASTComposite
    var annotations: [ASTAnnotation.Element]
    
    init(name: ASTIdentifier, parameters: [ASTVariableDeclaration], returnType: ASTType, kind: Kind, annotations: [ASTAnnotation.Element] = [], body: ASTComposite) {
        self.name = name
        self.parameters = parameters
        self.returnType = returnType
        self.kind = kind
        self.body = body
        self.annotations = annotations
    }
    
    var isVariadic: Bool {
        return self.hasAnnotation(.variadic)
    }
    
    var parameterTypes: [ASTType] {
        return parameters.map { $0.type }
    }
    
    
    var mangledName: String {
        switch kind {
        case .global:
            return name.value
        case .impl(let typename):
            return SymbolMangling.mangleInstanceMember(ofType: typename, memberName: name.value)
        case .staticImpl(let typename):
            return SymbolMangling.mangleStaticMember(ofType: typename, memberName: name.value)
        }
    }
}
