//
//  ASTCondition.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// TODO is the conformance to ASTExpression really necessary?
protocol ASTCondition: ASTExpression {}



class ASTBinaryCondition: ASTCondition {
    // TODO rename to Operation?
    enum Operator {
        case and
        case or
    }
    
    let lhs: ASTCondition
    let `operator`: ASTBinaryCondition.Operator
    let rhs: ASTCondition
    
    init(lhs: ASTCondition, operator: ASTBinaryCondition.Operator, rhs: ASTCondition) {
        self.lhs = lhs
        self.operator = `operator`
        self.rhs = rhs
    }
    
    var accessedIdentifiers: [ASTIdentifier] {
        return [lhs, rhs].accessedIdentifiers
    }
}


class ASTComparison: ASTCondition {
    enum Operation {
        case equal
        case notEqual
        case less
        case greater
        case lessEqual
        case greaterEqual
    }
    
    let lhs: ASTExpression
    let operation: ASTComparison.Operation
    let rhs: ASTExpression
    
    init(lhs: ASTExpression, operation: ASTComparison.Operation, rhs: ASTExpression) {
        self.lhs = lhs
        self.operation = operation
        self.rhs = rhs
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return [lhs, rhs].accessedIdentifiers
    }
}


// A condition where some expression is implicitly compared against 0
// ie:
// ```
// if foo { }
// if !bar { }
// ```
class ASTImplicitNonZeroComparison: ASTCondition {
    let expression: ASTExpression
    
    init(expression: ASTExpression) {
        self.expression = expression
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return expression.accessedIdentifiers
    }
}
