//
//  ASTForLoop.swift
//  yo
//
//  Created by Lukas Kollmer on 29.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTForLoop: ASTStatement {
    let identifier: ASTIdentifier
    let type: ASTType?
    let target: ASTExpression
    let body: ASTComposite
    
    init(identifier: ASTIdentifier, type: ASTType?, target: ASTExpression, body: ASTComposite) {
        self.identifier = identifier
        self.type = type
        self.target = target
        self.body = body
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return body.accessedIdentifiers
    }
}
