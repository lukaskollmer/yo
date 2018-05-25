//
//  ASTFunctionDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

struct ASTFunctionDeclaration: ASTNode {
    let name: String
    let arguments: [String]
    let body: [ASTNode]
    
    
    var mangledName: String {
        return name
    }
}
