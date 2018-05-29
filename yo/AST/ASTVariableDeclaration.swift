//
//  ASTVariableDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTVariableDeclaration: ASTStatement, Equatable {
    let identifier: ASTIdentifier
    let typename: ASTIdentifier
}


// Array<ASTStatement> + local variables
extension Array where Element == ASTStatement {
    var localVariables: [ASTVariableDeclaration] {
        return self.lk_flatMap { (statement: ASTStatement) in
            if let decl = statement as? ASTVariableDeclaration {
                return [decl]
            } else if let composite = statement as? ASTComposite {
                return composite.statements.localVariables
            } else if let conditional = statement as? ASTConditionalStatement {
                var conditionalVariableDecls = conditional.body.statements.localVariables
                if case .if(let elseBranch) = conditional.kind, elseBranch != nil {
                    conditionalVariableDecls.append(contentsOf: elseBranch!.statements.localVariables)
                }
                return conditionalVariableDecls
            }
            return []
        }
    }
}
