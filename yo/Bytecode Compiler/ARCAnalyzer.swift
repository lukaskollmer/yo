//
//  ARCAnalyzer.swift
//  yo
//
//  Created by Lukas Kollmer on 01.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct Counter {
    private var _counter = 0
    
    mutating func get() -> Int {
        _counter += 1
        return _counter
    }
}


/// The `ARCAnalyzer` class tries to insert `runtime::retain` and `runtime::release` calls into an AST
/// TODO
class ARCAnalyzer {
    private var counter = Counter()
    private let semanticAnalysis: SemanticAnalyzer.Result
    
    init(semanticAnalysis: SemanticAnalyzer.Result) {
        self.semanticAnalysis = semanticAnalysis
    }
    
    
    func foo(ast: [ASTNode]) -> [ASTNode] {
        return ast.map { topLevelNode in
            if let functionDecl = topLevelNode as? ASTFunctionDeclaration {
                return handle(functionDeclaration: functionDecl)
                
                
                
            } else {
                return topLevelNode
            }
        }
    }
    
    
    
    
    
    func handle(functionDeclaration: ASTFunctionDeclaration) -> ASTFunctionDeclaration {
        
        let newBody = handleStringLiteralsInFunctionCalls(in: functionDeclaration.body.statements, inFunction: functionDeclaration)
        
        return ASTFunctionDeclaration(
            name: functionDeclaration.name,
            parameters: functionDeclaration.parameters,
            returnType: functionDeclaration.returnType,
            kind: functionDeclaration.kind,
            body: ASTComposite(statements: newBody)
        )
    }
    
    
    
    
    
    // implement arc for string literals in function calls
    // if some of the function call's arguments are string literals
    // we now extract these literals from the function call, and
    // 1. declare them as local variables
    // 2. assign the local variable to the string literal
    // 3. retain the local variable
    // 4. perform the function call, passing the local variable instead of the string literal
    // 5. release the local variable
    func handleStringLiteralsInFunctionCalls(in statements: [ASTStatement], inFunction function: ASTFunctionDeclaration) -> [ASTStatement] {
        return statements.lk_flatMap { statement in
            
            if let conditionalStatement = statement as? ASTConditionalStatement {
                
                let kind: ASTConditionalStatement.Kind
                if case .if(let elseBody) = conditionalStatement.kind {
                    if let elseBody = elseBody {
                        kind = .if(ASTComposite(statements: handleStringLiteralsInFunctionCalls(in: elseBody.statements, inFunction: function)))
                    } else {
                        kind = .if(nil)
                    }
                } else {
                    kind = .while
                }
                
                return [ASTConditionalStatement(
                    condition: conditionalStatement.condition,
                    body: ASTComposite(statements: handleStringLiteralsInFunctionCalls(in: conditionalStatement.body.statements, inFunction: function)),
                    kind: kind
                )]
                
            } else if let composite = statement as? ASTComposite {
                return [
                    ASTComposite(
                        statements: handleStringLiteralsInFunctionCalls(in: composite.statements, inFunction: function),
                        introducesNewScope: composite.introducesNewScope
                    )
                ]
                
            } else if let functionCall = statement as? ASTFunctionCall {
                
                let stringLiteralArguments = functionCall.arguments
                    .enumerated()
                    .filter { $0.element is ASTStringLiteral }
                
                if !stringLiteralArguments.isEmpty {
                    
                    // fetch all string literal arguments
                    
                    let identifiers = stringLiteralArguments.reduce(into: [Int: ASTIdentifier]()) { result, element in
                        result[element.offset] = ASTIdentifier(name: "__string_literal_\(function.mangledName)_\(counter.get())")
                    }
                    
                    var newStatements = [ASTStatement]()
                    
                    
                    // iterate over all string literal arguments to create local variables for them
                    
                    for (index, literal) in stringLiteralArguments {
                        let identifier = identifiers[index]! // TODO force unwrap
                        newStatements.append(
                            ASTVariableDeclaration(
                                identifier: identifier,
                                typename: ASTIdentifier(name: "String")
                            )
                        )
                        
                        newStatements.append(
                            ASTAssignment(
                                target: identifier,
                                value: literal
                            )
                        )
                        
                        newStatements.append(
                            ASTFunctionCall(
                                functionName: SymbolMangling.mangleStaticMember(ofType: "runtime", memberName: "retain"),
                                arguments: [identifier],
                                unusedReturnValue: true
                            )
                        )
                    }
                    
                    
                    // perform the function call
                    
                    let newArguments: [ASTExpression] = functionCall.arguments.enumerated().map { index, element in
                        if let identifier = identifiers[index] {
                            return identifier
                        }
                        return element
                    }
                    
                    newStatements.append(
                        ASTFunctionCall(
                            functionName: functionCall.functionName,
                            arguments: newArguments,
                            unusedReturnValue: functionCall.unusedReturnValue
                        )
                    )
                    
                    
                    // iterate over the string literals once more to release them
                    
                    for (index, _) in stringLiteralArguments {
                        let identifier = identifiers[index]!
                        
                        newStatements.append(
                            ASTFunctionCall(
                                functionName: SymbolMangling.mangleStaticMember(ofType: "runtime", memberName: "release"),
                                arguments: [identifier],
                                unusedReturnValue: true
                            )
                        )
                    }
                    
                    return newStatements
                    
                }
            }
            
            return [statement]
        }
    }
}
