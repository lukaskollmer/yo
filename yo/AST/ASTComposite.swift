//
//  ASTComposite.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTComposite: ASTStatement, ExpressibleByArrayLiteral {
    let statements: [ASTStatement]
    var isUnsafe: Bool
    
    init(statements: [ASTStatement], isUnsafe: Bool = false) { // TODO remove the default value?
        self.statements = statements
        self.isUnsafe = isUnsafe
    }
    
    convenience required init(arrayLiteral elements: ASTStatement...) {
        self.init(statements: elements)
    }
    
    
    func appending(statements otherStatements: [ASTStatement]) -> ASTComposite {
        return ASTComposite(statements: statements + otherStatements, isUnsafe: self.isUnsafe)
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return statements.accessedIdentifiers
    }
    
    
    // Get a list of all local variables declared by this node (and optionally its child nodes)
    func getLocalVariables(recursive: Bool) -> [ASTVariableDeclaration] {
        if !recursive {
            return statements.lk_flatMap { stmt in
                if let decl = stmt as? ASTVariableDeclaration {
                    return [decl]
                }
                return []
            }
        }
        
        return statements.lk_flatMap { statement in
            if let decl = statement as? ASTVariableDeclaration {
                return [decl]
                
            } else if let composite = statement as? ASTComposite {
                return composite.getLocalVariables(recursive: recursive)
                
            } else if let whileStatement = statement as? ASTWhileStatement {
                return whileStatement.body.getLocalVariables(recursive: recursive)
                
            } else if let forLoop = statement as? ASTForLoop {
                let iteratorElement = ASTVariableDeclaration(identifier: forLoop.identifier, type: forLoop.type ?? .unresolved) // TODO is unresolved the best idea here?
                return [iteratorElement] + forLoop.body.getLocalVariables(recursive: recursive)
            
            } else if let ifStatement = statement as? ASTIfStatement {
                return ifStatement.branches.lk_flatMap {
                    switch $0 {
                    case ._if(_, let body), ._else_if(_, let body), ._else(let body):
                        return body.getLocalVariables(recursive: recursive)
                    }
                }
            }
            
            return []
        }
    }

}


