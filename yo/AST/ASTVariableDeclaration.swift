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
    
    func getLocalVariables(recursive: Bool) -> [ASTVariableDeclaration] {
        if !recursive {
            // TODO
            // This currently doesn't support local variable declarations w/ an initial value,
            // since these are parsed into composites w/ 2 statements (declaration and assignment)
            return self.compactMap { $0 as? ASTVariableDeclaration }
        }
        
        return self.lk_flatMap { (statement: ASTStatement) in
            if let decl = statement as? ASTVariableDeclaration {
                return [decl]
                
            } else if let composite = statement as? ASTComposite {
                return composite.statements.getLocalVariables(recursive: true)
                
            } else if let conditional = statement as? ASTConditionalStatement {
                var conditionalVariableDecls = conditional.body.statements.getLocalVariables(recursive: true)
                
                if case .if(let elseBranch) = conditional.kind, elseBranch != nil {
                    conditionalVariableDecls.append(contentsOf: elseBranch!.statements.getLocalVariables(recursive: true))
                }
                
                return conditionalVariableDecls
            }
            
            return []
        }
    }
}
