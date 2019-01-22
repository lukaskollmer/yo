//
//  ASTLambda.swift
//  yo
//
//  Created by Lukas Kollmer on 18.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// An anonymous function
class ASTLambda: ASTExpression {
    var signature: ASTType // is always .function
    let parameters: [ASTVariableDeclaration]
    let body: ASTComposite
    
    init(signature: ASTType, parameters: [ASTVariableDeclaration], body: ASTComposite) {
        self.signature = signature
        self.parameters = parameters
        self.body = body
    }
    
    
    var accessedIdentifiersFromOutsideScope: [ASTIdentifier] {
        var localVariables = [ASTIdentifier]()
        localVariables.append(contentsOf: parameters.map { $0.identifier })
        localVariables.append(contentsOf: body.statements.getLocalVariables(recursive: true).map { $0.identifier })
        
        var accessed = body.getAccessedIdentifiers()
        accessed.remove { localVariables.contains($0) }
        accessed.removeDuplicates()
        
        return accessed
    }
}


extension ASTNode {
    func getAccessedIdentifiers() -> [ASTIdentifier] {
        if let identifier = self as? ASTIdentifier {
            return [identifier]
            
        } else if self is ASTNumberLiteral {
            return []
            
        } else if self is ASTStringLiteral {
            return []
            
        } else if self is ASTVariableDeclaration {
            return []
            
        } else if self is ASTBooleanLiteral {
            return []
            
        } else if self is ASTRawUnresolvedInstruction {
            return []
            
        } else if let arrayLiteral = self as? ASTArrayLiteral {
            return arrayLiteral.elements.lk_flatMap { $0.getAccessedIdentifiers() }
        
        } else if let composite = self as? ASTComposite {
            return composite.statements.lk_flatMap { $0.getAccessedIdentifiers() }
        
        } else if let ret = self as? ASTReturnStatement {
            return ret.expression.getAccessedIdentifiers()
            
        } else if let assignment = self as? ASTAssignment {
            return assignment.target.getAccessedIdentifiers() + assignment.value.getAccessedIdentifiers()
        
        } else if let conditionalStmt = self as? ASTConditionalStatement {
            var retval = [ASTIdentifier]()
            retval.append(contentsOf: conditionalStmt.condition.getAccessedIdentifiers())
            retval.append(contentsOf: conditionalStmt.body.getAccessedIdentifiers())
            
            if case ASTConditionalStatement.Kind.if(let elseBranch) = conditionalStmt.kind, elseBranch != nil {
                retval.append(contentsOf: elseBranch!.getAccessedIdentifiers())
            }
            
            return retval
        
        } else if let functionCall = self as? ASTFunctionCall {
            // TODO(too_many_potential_lambdas_retained)
            // What if the function being called isn't a lambda, but some "normal" global function instead?
            // We still generate retain/release calls for these functions, even though these calls will always fail
            // This isn't really a giant issue, since all pure lambdas become global functions anyway, but there's room for improvement
            // As long as functions are guaranteed to have odd addresses, this isn't a breaking issue // TODO are they still?
            return functionCall.arguments.reduce(into: [ASTIdentifier(value: functionCall.functionName)], { $0.append(contentsOf: $1.getAccessedIdentifiers()) })
        
        } else if let memberAccess = self as? ASTMemberAccess {
            return memberAccess.members.lk_flatMap { member in
                switch member {
                case .initial_identifier(let identifier):
                    return [identifier]
                case .functionCall(name: _, let arguments, unusedReturnValue: _):
                    return arguments.lk_flatMap { $0.getAccessedIdentifiers() }
                case .attribute(name: _):
                    return []
                case .initial_functionCall(_):
                    return []
                }
            }
        
        } else if let comparison = self as? ASTComparison {
            return comparison.lhs.getAccessedIdentifiers() + comparison.rhs.getAccessedIdentifiers()
        
        } else if let binop = self as? ASTBinaryOperation {
            return binop.lhs.getAccessedIdentifiers() + binop.rhs.getAccessedIdentifiers()
        
        } else if let typecast = self as? ASTTypecast {
            return typecast.expression.getAccessedIdentifiers()
        
        } else if let boxedExpression = self as? ASTBoxedExpression {
            return boxedExpression.expression.getAccessedIdentifiers()
        
        } else if let lambda = self as? ASTLambda {
            return lambda.accessedIdentifiersFromOutsideScope
        
        } else if let unaryExpression = self as? ASTUnaryExpression {
            return unaryExpression.expression.getAccessedIdentifiers()
            
        } else if let arrayGetter = self as? ASTArrayGetter {
            return arrayGetter.target.getAccessedIdentifiers() + arrayGetter.offset.getAccessedIdentifiers()
        
        } else if let arraySetter = self as? ASTArraySetter {
            return arraySetter.target.getAccessedIdentifiers()
                + arraySetter.offset.getAccessedIdentifiers()
                + arraySetter.value.getAccessedIdentifiers()
            
        } else if let implicitNonZeroComparison = self as? ASTImplicitNonZeroComparison {
            return implicitNonZeroComparison.expression.getAccessedIdentifiers()
        
        } else if let ifStatement = self as? ASTIfStatement {
            return ifStatement.branches.lk_flatMap { branch in
                switch branch {
                // TODO refactor the first two into a single case once the swift compiler supports matching protocol values in different patterns
                case ._if(let condition, let body):
                    return condition.getAccessedIdentifiers() + body.getAccessedIdentifiers()
                case ._else_if(let condition, let body):
                    return condition.getAccessedIdentifiers() + body.getAccessedIdentifiers()
                case ._else(let body):
                    return body.getAccessedIdentifiers()
                }
            }
        }
        
        fatalError("unhandled node \(self)")
    }
}
