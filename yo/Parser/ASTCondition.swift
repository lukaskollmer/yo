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
        
        init(tokenType: TokenType) {
            let mapping: [TokenType: Operator] = [
                .equal:        .equal,
                .notEqual:     .notEqual,
                .less:         .less,
                .greater:      .greater,
                .lessEqual:    .lessEqual,
                .greaterEqual: .greaterEqual
            ]
            
            self = mapping[tokenType]!
        }
    }
    
    let lhs: ASTExpression
    let `operator`: ASTComparison.Operator
    let rhs: ASTExpression
}

// TODO ASTUnaryCondition?
