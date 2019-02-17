//
//  ASTAssignment.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTAssignment: ASTStatement {
    let target: ASTExpression
    let value: ASTExpression
    
    init(target: ASTExpression, value: ASTExpression) {
        self.target = target
        self.value = value
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return [target, value].accessedIdentifiers
    }
}
