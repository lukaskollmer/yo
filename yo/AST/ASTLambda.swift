//
//  ASTLambda.swift
//  yo
//
//  Created by Lukas Kollmer on 18.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// An anonymous function
class ASTLambda: ASTExpression {
    var signature: ASTType // is always .function
    let parameters: [ASTVariableDeclaration]
    let body: ASTComposite
    
    init(signature: ASTType, parameters: [ASTVariableDeclaration], body: ASTComposite) {
        self.signature = signature
        self.parameters = parameters
        self.body = body
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        fatalError() // This won't be of much use for lambdas
    }
    
    
    // This is a bit special for lambdas, since lambdas are the only expressions that can declare variables
    // `ASTLambda.accessedIdentifiers` returns all identifiers that aren't declared within the lambda's body
    var accessedIdentifiersFromOutsideScope: [ASTIdentifier] {
        var localVariables = [ASTIdentifier]()
        localVariables.append(contentsOf: parameters.map { $0.identifier })
        localVariables.append(contentsOf: body.getLocalVariables(recursive: true).map { $0.identifier })
        
        var accessed = body.accessedIdentifiers
        accessed.remove { localVariables.contains($0) }
        accessed.removeDuplicates()
        
        return accessed
    }
}
