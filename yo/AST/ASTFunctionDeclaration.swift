//
//  ASTFunctionDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

class ASTFunctionDeclaration: ASTStatement {
    let signature: ASTFunctionSignature
    let body: ASTComposite
    
    
    init(signature: ASTFunctionSignature, body: ASTComposite) {
        self.signature = signature
        self.body = body
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        fatalError() // Not really useful, right?
    }
    
    var mangledName: String {
        let name = signature.name.value
        switch signature.kind {
        case .global:
            return SymbolMangling.mangleGlobalFunction(name: name)
        case .impl(let typename):
            return SymbolMangling.mangleInstanceMember(ofType: typename, memberName: name)
        case .staticImpl(let typename):
            return SymbolMangling.mangleStaticMember(ofType: typename, memberName: name)
        }
    }
}
