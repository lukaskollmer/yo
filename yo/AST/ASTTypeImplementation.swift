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
    var functions: [ASTFunctionDeclaration]
    
    init(typename: ASTIdentifier, functions: [ASTFunctionDeclaration]) {
        self.typename = typename
        self.functions = functions
    }
}
