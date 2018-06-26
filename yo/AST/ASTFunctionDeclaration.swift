//
//  ASTFunctionDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

class ASTFunctionDeclaration: ASTStatement {
    enum Kind: Equatable {
        case global              // a global function
        case impl(String)        // instance function for some type
        case staticImpl(String)  // static function for some type
    }
    
    let name: ASTIdentifier
    let parameters: [ASTVariableDeclaration]
    let returnType: ASTType
    let kind: Kind
    let body: ASTComposite
    let annotations: [ASTAnnotation.Element]
    
    init(name: ASTIdentifier, parameters: [ASTVariableDeclaration], returnType: ASTType, kind: Kind, body: ASTComposite, annotations: [ASTAnnotation.Element] = []) {
        self.name = name
        self.parameters = parameters
        self.returnType = returnType
        self.kind = kind
        self.body = body
        self.annotations = annotations
    }
    
    
    var mangledName: String {
        switch kind {
        case .global:
            return name.name
        case .impl(let typename):
            return SymbolMangling.mangleInstanceMember(ofType: typename, memberName: name.name)
        case .staticImpl(let typename):
            return SymbolMangling.mangleStaticMember(ofType: typename, memberName: name.name)
        }
    }
}
