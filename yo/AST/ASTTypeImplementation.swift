//
//  ASTTypeImplementation.swift
//  yo
//
//  Created by Lukas Kollmer on 27.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTTypeImplementation: ASTStatement {
    let typename: ASTIdentifier
    let protocols: [ASTIdentifier]
    var functions: [ASTFunctionDeclaration]
    
    init(typename: ASTIdentifier, protocols: [ASTIdentifier] = [], functions: [ASTFunctionDeclaration]) {
        self.typename = typename
        self.protocols = protocols
        self.functions = functions
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return []
    }
}
