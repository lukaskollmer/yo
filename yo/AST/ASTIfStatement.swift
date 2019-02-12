//
//  ASTIfStatement.swift
//  yo
//
//  Created by Lukas Kollmer on 12.09.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTIfStatement: ASTStatement {
    enum Branch {
        case _if(ASTCondition, body: ASTComposite)
        case _else_if(ASTCondition, body: ASTComposite)
        case _else(ASTComposite)
    }
    
    let branches: [ASTIfStatement.Branch]
    
    init(branches: [ASTIfStatement.Branch]) {
        self.branches = branches
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return branches.lk_flatMap {
            switch $0 {
            case ._if(let condition, let body), ._else_if(let condition, let body):
                return body.accessedIdentifiers + condition.accessedIdentifiers
            case ._else(let body):
                return body.accessedIdentifiers
            }
        }
    }
}
