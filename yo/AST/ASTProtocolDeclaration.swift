//
//  ASTProtocolDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 01.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTProtocolDeclaration: ASTStatement, ASTTypeThatCanBeAnnotated {
    
    let name: ASTIdentifier
    let functions: [ASTFunctionDeclaration]
    let functionsWithoutDefaultImplementation: [ASTFunctionSignature]
    var annotations: [ASTAnnotation.Element]
    
    init(name: ASTIdentifier, functions: [ASTFunctionDeclaration], functionsWithoutDefaultImplementation: [ASTFunctionSignature], annotations: [ASTAnnotation.Element]) {
        self.name = name
        self.functions = functions
        self.functionsWithoutDefaultImplementation = functionsWithoutDefaultImplementation
        self.annotations = annotations
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return []
    }
}

extension ASTProtocolDeclaration: Hashable {
    var hashValue: Int {
        return self.name.hashValue
    }
    
    static func == (lhs: ASTProtocolDeclaration, rhs: ASTProtocolDeclaration) -> Bool {
        return lhs.name == rhs.name
    }
    
    
}
