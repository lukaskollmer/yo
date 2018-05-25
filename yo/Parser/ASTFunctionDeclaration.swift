//
//  ASTFunctionDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

struct ASTFunctionDeclaration: ASTStatement {
    let name: ASTIdentifier
    let parameters: [ASTIdentifier]
    let localVariables: [ASTIdentifier]
    let body: [ASTStatement]
    
    
    var mangledName: String {
        return name.name
    }
}
