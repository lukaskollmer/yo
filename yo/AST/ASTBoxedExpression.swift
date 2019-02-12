//
//  ASTBoxedExpression.swift
//  yo
//
//  Created by Lukas Kollmer on 01.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTBoxedExpression: ASTExpression {
    let expression: ASTExpression
    
    init(expression: ASTExpression) {
        self.expression = expression
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return expression.accessedIdentifiers
    }
}
