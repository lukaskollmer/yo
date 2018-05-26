//
//  ASTBinop.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTBinop: ASTExpression {
    
    enum Operator {
        case add
        case sub
        case mul
        case div
        case mod
        
        private static let tokenMapping: [TokenType: Operator] = [
            .plus: .add,
            .minus: .sub,
            .asterik: .mul,
            .forwardSlash: .div,
            .percentageSign: .mul
        ]
        
        init?(tokenType: TokenType) {
            // TODO this code is ugly af
            guard let token = Operator.tokenMapping[tokenType] else {
                return nil
            }
            self = token
        }
        
        var operation: Operation {
            switch self {
            case .add: return .add
            case .sub: return .sub
            case .mul: return .mul
            case .div: return .div
            case .mod: return .mod
            }
        }
    }
    
    let lhs: ASTExpression
    let `operator`: Operator
    let rhs: ASTExpression
}
