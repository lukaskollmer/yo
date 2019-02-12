//
//  ASTDeferStatement.swift
//  yo
//
//  Created by Lukas Kollmer on 11.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTDeferStatement: ASTStatement {
    let body: ASTComposite
    
    init(body: ASTComposite) {
        self.body = body
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return body.accessedIdentifiers
    }
}
