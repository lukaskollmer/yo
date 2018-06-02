//
//  ASTComposite.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTComposite: ASTStatement {
    let statements: [ASTStatement]
    
    // Whether variables declared within the composite should be available outside the composite
    // Why do we need this? Variable declarations w/ an initial value are represented as a composite (declaration and assignment)
    // however, the variable should be available outside that composite as well
    // if `introducesNewScope` is false, variables declared in the composite are hoisted into the next higher scope
    // NOTE: this only works reliably as long as we're the only ones who create and initialize variables in a composite!
    var introducesNewScope: Bool {
        guard
            statements.count == 2,
            statements[0] is ASTVariableDeclaration,
            statements[1] is ASTAssignment,
            let declaredIdentifier = (statements[0] as? ASTVariableDeclaration)?.identifier,
            let assignedIdentifier = ((statements[1] as? ASTAssignment)?.target as? ASTIdentifier)
        else { return true }
        
        return !(declaredIdentifier == assignedIdentifier)
    }
}
