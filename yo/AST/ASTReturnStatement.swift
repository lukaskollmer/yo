//
//  ASTReturnStatement.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTReturnStatement: ASTStatement {
    let expression: ASTExpression
    
    init(expression: ASTExpression) {
        self.expression = expression
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return expression.accessedIdentifiers
    }
}
