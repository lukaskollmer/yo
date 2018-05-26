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
        
        init?(tokenType: TokenType) {
            switch tokenType {
            case .doubleAmpersand:
                self = .and
            case .doublePipe:
                self = .or
            default: return nil
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
        
        init?(tokenType: TokenType) {
            
            switch tokenType {
            case .equals:       self = .equal
            case .notEqual:     self = .notEqual
            case.less:          self = .less
            case.greater:       self = .greater
            case.lessEqual:     self = .lessEqual
            case.greaterEqual:  self = .greaterEqual
            default:            return nil
            }
        }
    }
    
    let lhs: ASTExpression
    let `operator`: ASTComparison.Operator
    let rhs: ASTExpression
}

// TODO ASTUnaryCondition?
