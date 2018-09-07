//
//  Optimizer+Expression.swift
//  yo
//
//  Created by Lukas Kollmer on 05.09.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

extension Optimizer {
    func optimize(unaryExpression: ASTUnaryExpression) -> ASTExpression {
        let optimizedExpression = optimize(unaryExpression.expression)
        
        if unaryExpression.operator == .logicalNegation, let booleanLiteral = optimizedExpression as? ASTBooleanLiteral {
            return ASTBooleanLiteral(value: !booleanLiteral.value)
        }
        
        if let numberLiteral = optimizedExpression as? ASTNumberLiteral {
            let value = numberLiteral.value
            
            switch unaryExpression.operator {
            case .negate:
                if numberLiteral.type.isIntegerType {
                    return ASTNumberLiteral(value: -value)
                } else if numberLiteral.type == .double {
                    return ASTNumberLiteral(value: -value.unsafe_loadAsDouble)
                } else {
                    ShouldNeverReachHere("invalid number literal type")
                }
                
            case .bitwiseNot:
                guard numberLiteral.type.isIntegerType else {
                    ShouldNeverReachHere("invalid number literal type")
                }
                return ASTNumberLiteral(value: ~value)
                
            case .logicalNegation:
                ShouldNeverReachHere("logical negation only works w/ boolean types")
            }
        }
        
        return ASTUnaryExpression(expression: optimizedExpression, operator: unaryExpression.operator)
    }
    
    
    func optimize(binop: ASTBinaryOperation) -> ASTExpression {
        
        let lhsOptimized = optimize(binop.lhs)
        let rhsOptimized = optimize(binop.rhs)
        
        if let lhsAsNumber = lhsOptimized as? ASTNumberLiteral, let rhsAsNumber = rhsOptimized as? ASTNumberLiteral, lhsAsNumber.type.isCompatible(with: rhsAsNumber.type) {
            let lhsValue = lhsAsNumber.value
            let rhsValue = rhsAsNumber.value
            
            var result: Int!
            
            let isInt = ASTType.intTypes.contains(lhsAsNumber.type)
            
            func eval<T>(_ fn: (T, T) -> T, _ type: T.Type) {
                result = lk_eval_binop(lhs: lhsValue, rhs: rhsValue, type: type, fn: fn)
            }
            
            let guard_isInt = {
                if !isInt {
                    fatalError("[Optimizer.optimize(binop:)]: '\(binop.operation)' requires an integer")
                }
            }
            
            switch binop.operation {
            case .add:
                if isInt {
                    eval(+, Int.self)
                } else {
                    eval(+, Double.self)
                }
                
            case .sub:
                if isInt {
                    eval(-, Int.self)
                } else {
                    eval(-, Double.self)
                }
                
            case .mul:
                if isInt {
                    eval(*, Int.self)
                } else {
                    eval(*, Double.self)
                }
                
            case .div:
                if isInt {
                    eval(/, Int.self)
                } else {
                    eval(/, Double.self)
                }
                
            // the following operations are only defined for integers
            case .mod:
                guard_isInt()
                eval(%, Int.self)
                
            case .and:
                guard_isInt()
                eval(&, Int.self)
                
            case .or:
                guard_isInt()
                eval(|, Int.self)
                
            case .xor:
                guard_isInt()
                eval(^, Int.self)
                
            case .shl:
                guard_isInt()
                eval(<<, Int.self)
                
            case .shr:
                guard_isInt()
                eval(>>, Int.self)
                
            }
            
            guard result != nil else {
                fatalError()
            }
            
            return ASTNumberLiteral(value: result, type: lhsAsNumber.type)
            
        } else {
            return ASTBinaryOperation(lhs: lhsOptimized, operation: binop.operation, rhs: rhsOptimized)
        }
    }
}
