//
//  ASTLambda.swift
//  yo
//
//  Created by Lukas Kollmer on 18.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// An anonymous function
// Doesn't (yet) support captured variables // TODO
class ASTLambda: ASTExpression {
    var signature: ASTType // is always .function
    let parameterNames: [ASTIdentifier]
    let body: ASTComposite
    
    init(signature: ASTType, parameterNames: [ASTIdentifier], body: ASTComposite) {
        
        // TODO
        //guard case .function(_) = signature else {
        //    fatalError() // TODO throw instead
        //}
        self.signature = signature
        self.parameterNames = parameterNames
        self.body = body
    }
    
    
    var accessedIdentifiersFromOutsideScope: [ASTIdentifier] {
        var localVariables = [ASTIdentifier]()
        localVariables.append(contentsOf: parameterNames)
        localVariables.append(contentsOf: body.statements.getLocalVariables(recursive: true).map { $0.identifier })
        
        var accessed = body.getAccessedIdentifiers()
        accessed.remove { localVariables.contains($0) }
        
        return accessed
    }
}


extension ASTNode {
    func getAccessedIdentifiers() -> [ASTIdentifier] {
        
        if let identifier = self as? ASTIdentifier {
            return [identifier]
            
        } else if self is ASTNumberLiteral {
            return []
            
        } else if self is ASTVariableDeclaration {
            return []
        
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
            return functionCall.arguments.reduce(into: [], { $0.append(contentsOf: $1.getAccessedIdentifiers()) })
        
        } else if let memberAccess = self as? ASTMemberAccess {
            // we only look at the very first one
            if case .attribute(let identifier)? = memberAccess.members.first {
                return [identifier]
            }
            return []
        
        } else if let comparison = self as? ASTComparison {
            return comparison.lhs.getAccessedIdentifiers() + comparison.rhs.getAccessedIdentifiers()
        
        } else if let binop = self as? ASTBinaryOperation {
            return binop.lhs.getAccessedIdentifiers() + binop.rhs.getAccessedIdentifiers()
        }
        
        fatalError("unhandled node \(self)")
    }
}