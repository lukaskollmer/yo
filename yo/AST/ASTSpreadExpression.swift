//
//  ASTSpreadExpression.swift
//  yo
//
//  Created by Lukas Kollmer on 2019-02-12.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTSpreadExpression: ASTExpression {
    let expression: ASTExpression
    
    init(expression: ASTExpression) {
        self.expression = expression
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return expression.accessedIdentifiers
    }
}
