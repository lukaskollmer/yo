//
//  ASTProtocolDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 01.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTProtocolDeclaration: ASTStatement {
    let name: ASTIdentifier
    let functions: [ASTFunctionDeclaration]
    
    init(name: ASTIdentifier, functions: [ASTFunctionDeclaration]) {
        self.name = name
        self.functions = functions
    }
}
