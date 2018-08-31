//
//  ASTVariableDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTVariableDeclaration: ASTStatement, Equatable {
    let identifier: ASTIdentifier
    var type: ASTType
    let initialValue: ASTExpression?
    let isStatic: Bool
    
    init(identifier: ASTIdentifier, type: ASTType, initialValue: ASTExpression? = nil, isStatic: Bool = false) {
        self.identifier = identifier
        self.type = type
        self.initialValue = initialValue
        self.isStatic = isStatic
    }
    
    // TODO there's a case to be made that this should look only at the identifier and ignore the type
    static func == (lhs: ASTVariableDeclaration, rhs: ASTVariableDeclaration) -> Bool {
        return lhs.identifier == rhs.identifier && lhs.type == rhs.type
    }
}


// Array<ASTStatement> + local variables
extension Array where Element == ASTStatement {
    
    // TODO we probably can get rid of the recursive option
    func getLocalVariables(recursive: Bool) -> [ASTVariableDeclaration] {
        if !recursive {
            return self.lk_flatMap { stmt in
                if let decl = stmt as? ASTVariableDeclaration {
                    return [decl]
                }
                return []
            }
        }
        
        // func bc closures can't be recursive
        func fn(statement: ASTStatement) -> [ASTVariableDeclaration] {
            if let decl = statement as? ASTVariableDeclaration {
                return [decl]
                
            } else if let composite = statement as? ASTComposite {
                return composite.statements.getLocalVariables(recursive: true)
                
            } else if let conditional = statement as? ASTConditionalStatement {
                var conditionalVariableDecls = conditional.body.statements.getLocalVariables(recursive: true)
                
                if case .if(let elseBranch) = conditional.kind, elseBranch != nil {
                    conditionalVariableDecls.append(contentsOf: fn(statement: elseBranch!))
                }
                
                return conditionalVariableDecls
            }
            
            return []
        }
        
        return self.lk_flatMap(fn)
    }
}
