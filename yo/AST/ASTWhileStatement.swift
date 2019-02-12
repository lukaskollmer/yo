//
//  ASTWhileStatement.swift
//  yo
//
//  Created by Lukas Kollmer on 2019-02-11.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTWhileStatement: ASTStatement {
    let condition: ASTCondition
    let body: ASTComposite
    
    init(condition: ASTCondition, body: ASTComposite) {
        self.condition = condition
        self.body = body
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return condition.accessedIdentifiers + body.accessedIdentifiers
    }
}
