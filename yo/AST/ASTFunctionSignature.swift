//
//  ASTFunctionSignature.swift
//  yo
//
//  Created by Lukas Kollmer on 30.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTFunctionSignature: ASTStatement, FunctionSignature {
    let name: ASTIdentifier
    let parameters: [ASTVariableDeclaration]
    let returnType: ASTType
    let annotations: [ASTAnnotation.Element]
    let isUnsafe: Bool
    let kind: FunctionKind
    
    var parameterTypes: [ASTType] {
        return parameters.map { $0.type }
    }
    
    
    init(name: ASTIdentifier, kind: FunctionKind, parameters: [ASTVariableDeclaration], returnType: ASTType, annotations: [ASTAnnotation.Element], isUnsafe: Bool = false) {
        self.name = name
        self.kind = kind
        self.parameters = parameters
        self.returnType = returnType
        self.annotations = annotations
        self.isUnsafe = isUnsafe
    }
    
    init(parameterTypes: [ASTType], returnType: ASTType, annotations: [ASTAnnotation.Element] = [], isUnsafe: Bool = false) {
        self.name = .unknown
        self.kind = .global
        self.parameters = parameterTypes.map { ASTVariableDeclaration(identifier: .unknown, type: $0) }
        self.returnType = returnType
        self.annotations = annotations
        self.isUnsafe = isUnsafe
    }
    
}
