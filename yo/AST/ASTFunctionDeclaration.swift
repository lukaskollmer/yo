//
//  ASTFunctionDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

class ASTFunctionDeclaration: ASTStatement {
    /*enum Kind: Equatable {
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
    }*/
    
    
    let signature: ASTFunctionSignature
    //var kind: Kind
    let body: ASTComposite
    
    
    init(signature: ASTFunctionSignature,/* kind: Kind,*/ body: ASTComposite) {
        self.signature = signature
        //self.kind = kind
        self.body = body
    }
    
    init(name: ASTIdentifier, parameters: [ASTVariableDeclaration], returnType: ASTType, kind: FunctionKind, annotations: [ASTAnnotation.Element] = [], body: ASTComposite) {
        self.signature = ASTFunctionSignature(name: name, kind: kind, parameters: parameters, returnType: returnType, annotations: annotations)
        //self.kind = kind
        self.body = body
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
