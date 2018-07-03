//
//  SemanticAnalyzer.swift
//  yo
//
//  Created by Lukas Kollmer on 30.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class SemanticAnalyzer {
    
    // TODO make this a struc (w/ an ASTFunctionDeclaration initializer)
    typealias FunctionInfo = (argc: Int, parameterTypes: [ASTType], returnType: ASTType, annotations: [ASTAnnotation.Element])
    
    struct Result {
        let globalFunctions: [String: FunctionInfo]
        let types: [ASTTypeDeclaration]
        let globals: [ASTStaticVariableDeclaration]
    }
    
    
    
    func analyze(ast: [ASTNode]) -> SemanticAnalyzer.Result {
        var types = [ASTTypeDeclaration]()
        var functions = [String: FunctionInfo]()
        let globals = ast.compactMap { $0 as? ASTStaticVariableDeclaration }
        
        let handleFunction = { (functionDecl: ASTFunctionDeclaration) -> Void in
            functions[functionDecl.mangledName] = (
                argc: functionDecl.parameters.count,
                parameterTypes: functionDecl.parameters.map { $0.type },
                returnType: functionDecl.returnType,
                annotations: functionDecl.annotations
            )
        }
        
        for node in ast {
            if let typeDecl = node as? ASTTypeDeclaration {
                types.append(typeDecl)
                
            } else if let functionDecl = node as? ASTFunctionDeclaration {
                handleFunction(functionDecl)
                
            } else if let impl = node as? ASTTypeImplementation {
                impl.functions.forEach(handleFunction)
            }
        }
        
        return SemanticAnalyzer.Result(globalFunctions: functions, types: types, globals: globals)
    }
}
