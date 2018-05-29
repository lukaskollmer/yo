//
//  ASTFunctionDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

struct ASTFunctionDeclaration: ASTStatement {
    enum Kind {
        case global              // a global function
        case impl(String)        // instance function for some type
        case staticImpl(String)  // static function for some type
    }
    
    let name: ASTIdentifier
    let parameters: [ASTVariableDeclaration]
    let returnType: ASTIdentifier
    let body: [ASTStatement]
    let kind: Kind
    
    
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
    
    var localVariables: [ASTVariableDeclaration] {
        return body.localVariables
    }
}
