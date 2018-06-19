//
//  ASTCondition.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


protocol ASTCondition: ASTExpression {}



class ASTBinaryCondition: ASTCondition {
    enum Operator {
        case and
        case or
        
        init?(tokenTypes tokenA: TokenType, _ tokenB: TokenType) {
            switch (tokenA, tokenB) {
            case (.ampersand, .ampersand):
                self = .and
            case (.pipe, .pipe):
                self = .or
            default:
                return nil
            }
        }
    }
    
    let lhs: ASTCondition
    let `operator`: ASTBinaryCondition.Operator
    let rhs: ASTCondition
    
    init(lhs: ASTCondition, operator: ASTBinaryCondition.Operator, rhs: ASTCondition) {
        self.lhs = lhs
        self.operator = `operator`
        self.rhs = rhs
    }
}


class ASTComparison: ASTCondition {
    enum Operator {
        case equal
        case notEqual
        case less
        case greater
        case lessEqual
        case greaterEqual
    }
    
    let lhs: ASTExpression
    let `operator`: ASTComparison.Operator
    let rhs: ASTExpression
    
    init(lhs: ASTExpression, operator: ASTComparison.Operator, rhs: ASTExpression) {
        self.lhs = lhs
        self.operator = `operator`
        self.rhs = rhs
    }
}

// TODO ASTUnaryCondition?
