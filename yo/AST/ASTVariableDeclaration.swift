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
    let type: ASTType
}


// Array<ASTStatement> + local variables
extension Array where Element == ASTStatement {
    
    // TODO we probably can get rid of the recursive option
    func getLocalVariables(recursive: Bool) -> [ASTVariableDeclaration] {
        if !recursive {
            return self.lk_flatMap { stmt in
                if let decl = stmt as? ASTVariableDeclaration {
                    return [decl]
                
                } else if let composite = stmt as? ASTComposite, !composite.introducesNewScope {
                    return composite.statements.getLocalVariables(recursive: false) // no need to make this recursive since we're dealing w/ an assignment composite
                }
                return []
            }
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
