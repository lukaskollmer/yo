//
//  ASTFunctionDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

struct ASTFunctionDeclaration: ASTNode {
    let name: ASTIdentifier
    let parameters: [ASTIdentifier]
    let localVariables: [ASTIdentifier]
    let body: [ASTNode]
    
    
    var mangledName: String {
        return name.name
    }
}
