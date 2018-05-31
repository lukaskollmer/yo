//
//  ASTBinaryOperation.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTBinaryOperation: ASTExpression {
    
    // We can't use the name `Operation` bc that would shadow the `Operation` enum defining the different stack operations
    // TODO rename Instruction (the Int typealias) to something like CompiledInstruction or RawInstruction, rename Operation to Instruction and rename the enum below to Operation
    enum BinopOperation {
        case add
        case sub
        case mul
        case div
        case mod
        
        case and
        case or
        case xor
        case shl // left shift
        case shr // right shift
        
//        private static let tokenMapping: [TokenType: Operator] = [
//            .plus: .add,
//            .minus: .sub,
//            .asterik: .mul,
//            .forwardSlash: .div,
//            .percentageSign: .mul
//        ]
//
//        init?(tokenType: TokenType) {
//            // TODO this code is ugly af
//            guard let token = Operator.tokenMapping[tokenType] else {
//                return nil
//            }
//            self = token
//        }
//
        var operation: Operation {
            switch self {
            case .add: return .add
            case .sub: return .sub
            case .mul: return .mul
            case .div: return .div
            case .mod: return .mod
            case .and: return .and
            case .or:  return .or
            case .xor: return .xor
            case .shl: return .shl
            case .shr: return .shr
            }
        }
    }
    
    let lhs: ASTExpression
    let operation: BinopOperation
    let rhs: ASTExpression
}
