//
//  Optimizer.swift
//  yo
//
//  Created by Lukas Kollmer on 26.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

//private extension AssociatedObjectKeys {
//    static let didOptimize = AssociatedObjectKey<Bool>("_didOptimize")
//}

//extension ASTNode {
//    fileprivate(set) var didOptimize: Bool {
//        get { return self.getAssociatedObject(forKey: .didOptimize) ?? false }
//        set { self.setAssociatedObject(forKey: .didOptimize, value: true) }
//    }
//}

// TODO have them ordered by which node type is used most often to speed up `contains` calls?
private let IgnoredAstNodeTypes: [AnyClass] = [
    // top level statements
    ASTStructDeclaration.self,
    ASTEnumDeclaration.self,
    
    // expressions
    ASTIdentifier.self,
    ASTMemberAccess.self,
    
    // literals
    ASTNumberLiteral.self, ASTStringLiteral.self, ASTBooleanLiteral.self,
    
    // TODO: implement these
    ASTConditionalStatement.self
]

private func shouldIgnoreNode(_ node: ASTNode) -> Bool {
    return IgnoredAstNodeTypes.contains { $0 == type(of: node) }
}


// GIANT ASS TODO: the entire didOptimize thing is completely unnecessary !!!

// How does this work?
// We have one key function: Optimizer.optimize(:)
// This is the function that should be called to optimize a specific node
// The optimize(:) function then "forwards" the node to the correct function for handling it
// It's key for this to work that the optimize(:) function is *the only* function that calls any of the other overloads

class Optimizer {
    func optimize(ast: AST) -> AST {
        return ast.map(optimize)
    }
    
    // ideally, this would be `<T: ASTNode>` instead, but for some reason the compiler refuses to pass some ast types
    func optimize<T>(_ _node: T) -> T {
        print("optimize", _node)
        guard let node = _node as? ASTNode else {
            fatalError("not an ASTNode")
        }
        
        if IgnoredAstNodeTypes.contains(where: { $0 == type(of: node) }) {
            return node as! T
        }
        
        // we use the retval variable to avoid having to end each returning line w/ `as! T`
        let retval: ASTNode
        
        
        // Top level statements
        
        if let functionDeclaration = node as? ASTFunctionDeclaration {
            retval = ASTFunctionDeclaration(
                signature: functionDeclaration.signature,
                body: optimize(functionDeclaration.body)
            )
            
        } else if let protocolDeclaration = node as? ASTProtocolDeclaration {
            retval = ASTProtocolDeclaration(
                name: protocolDeclaration.name,
                functions: protocolDeclaration.functions.map(optimize),
                functionsWithoutDefaultImplementation: protocolDeclaration.functionsWithoutDefaultImplementation,
                annotations: protocolDeclaration.annotations
            )
            
        } else if let typeImplementation = node as? ASTTypeImplementation {
            retval = ASTTypeImplementation(
                typename: typeImplementation.typename,
                protocols: typeImplementation.protocols,
                functions: typeImplementation.functions.map(optimize)
            )
        
        
        // Statements
        
        } else if let composite = node as? ASTComposite {
            retval = ASTComposite(
                statements: composite.statements.map { optimize($0) },
                isUnsafe: composite.isUnsafe
            )
        
        } else if let variableDeclaration = node as? ASTVariableDeclaration {
            if let initialValue = variableDeclaration.initialValue {
                retval = ASTVariableDeclaration(
                    identifier: variableDeclaration.identifier,
                    type: variableDeclaration.type,
                    initialValue: optimize(initialValue),
                    isStatic: variableDeclaration.isStatic
                )
            } else {
                // no initial value -> nothing to optimize
                retval = variableDeclaration
            }
            
        } else if let functionCall = node as? ASTFunctionCall {
            retval = ASTFunctionCall(
                functionName: functionCall.functionName,
                arguments: functionCall.arguments.map(optimize),
                unusedReturnValue: functionCall.unusedReturnValue
            )
            
        } else if let assignment = node as? ASTAssignment {
            // TODO is there ever a case where it'd make sense to optimize the target?
            retval = ASTAssignment(
                target: optimize(assignment.target),
                value: optimize(assignment.value)
            )
            
        } else if let arraySetter = node as? ASTArraySetter {
            retval = ASTArraySetter(
                target: optimize(arraySetter.target),
                offset: optimize(arraySetter.offset),
                value: optimize(arraySetter.value)
            )
            
        } else if let forLoop = node as? ASTForLoop {
            retval = ASTForLoop(
                identifier: forLoop.identifier,
                type: forLoop.type,
                target: optimize(forLoop.target),
                body: optimize(forLoop.body)
            )
            
        } else if let returnStatement = node as? ASTReturnStatement { // TODO check that the expression is un-optimized?
            retval = ASTReturnStatement(expression: optimize(returnStatement.expression))
        
        
        // Expressions
            
        // TODO this seems to ignore lambdas as function call parameters?
        
        } else if let binop = node as? ASTBinaryOperation {
            retval = optimize(binop: binop)
            
        } else if let boxedExpression = node as? ASTBoxedExpression {
            retval = ASTBoxedExpression(expression: optimize(boxedExpression.expression))
            
        } else if let typecast = node as? ASTTypecast {
            retval = ASTTypecast(
                expression: optimize(typecast.expression),
                type: typecast.type
            )
            
        } else if let arrayGetter = node as? ASTArrayGetter {
            retval = ASTArrayGetter(
                target: optimize(arrayGetter.target),
                offset: optimize(arrayGetter.offset),
                typeOfAccessedField: arrayGetter.typeOfAccessedField
            )
            
        } else if let rangeLiteral = node as? ASTRangeLiteral {
            retval = ASTRangeLiteral(
                start: optimize(rangeLiteral.start),
                end: optimize(rangeLiteral.end),
                kind: rangeLiteral.kind
            )
            
        } else if let unaryExpression = node as? ASTUnaryExpression {
            retval = optimize(unaryExpression: unaryExpression)
            
        } else if let arrayLiteral = node as? ASTArrayLiteral {
            retval = ASTArrayLiteral(
                elements: arrayLiteral.elements.map(optimize),
                kind: arrayLiteral.kind
            )
            
        } else if let lambda = node as? ASTLambda {
            retval = lambda // TODO
            
        } else {
            fatalError("[Optimizer] unhandled node \(node)")
        }
            
        
        // Expressions
        
        return retval as! T
    }
}
