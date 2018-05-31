//
//  ASTCondition.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


protocol ASTCondition: ASTExpression {}



struct ASTBinaryCondition: ASTCondition {
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
}


struct ASTComparison: ASTCondition {
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
}

// TODO ASTUnaryCondition?
