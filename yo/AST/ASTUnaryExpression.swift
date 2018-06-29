//
//  ASTUnaryExpression.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTUnaryExpression: ASTExpression {
    enum Operator {
        case negate
        case bitwiseNot
    }
    
    let `operator`: ASTUnaryExpression.Operator
    let expression: ASTExpression
    
    init(expression: ASTExpression, operator: ASTUnaryExpression.Operator) {
        self.expression = expression
        self.operator = `operator`
    }
}
