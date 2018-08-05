//
//  SemanticAnalyzer.swift
//  yo
//
//  Created by Lukas Kollmer on 30.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

extension Dictionary where Key == String, Value == SemanticAnalyzer.FunctionInfo {
    mutating func insert(functionDeclaration: ASTFunctionDeclaration) {
        self[functionDeclaration.mangledName] = SemanticAnalyzer.FunctionInfo(functionDeclaration: functionDeclaration)
    }
}


class SemanticAnalyzer {
    
    struct FunctionInfo {
        var argc: Int { return parameterTypes.count }
        let parameterTypes: [ASTType]
        let returnType: ASTType
        let annotations: [ASTAnnotation.Element]
        
        init(parameterTypes: [ASTType], returnType: ASTType, annotations: [ASTAnnotation.Element]) {
            self.parameterTypes = parameterTypes
            self.returnType = returnType
            self.annotations = annotations
        }
        
        init(functionDeclaration: ASTFunctionDeclaration) {
            self.init(
                parameterTypes: functionDeclaration.parameters.map { $0.type },
                returnType:     functionDeclaration.returnType,
                annotations:    functionDeclaration.annotations
            )
        }
    }
    
    struct Result {
        let globalFunctions: [String: FunctionInfo]
        let types: [ASTTypeDeclaration]
        let globals: [ASTStaticVariableDeclaration]
    }
    
    
    
    func analyze(ast: AST) -> SemanticAnalyzer.Result {
        var types = [ASTTypeDeclaration]()
        var functions = [String: FunctionInfo]()
        let globals = ast.compactMap { $0 as? ASTStaticVariableDeclaration }
        
        let handleFunction: (ASTFunctionDeclaration) -> Void = {
            functions.insert(functionDeclaration: $0)
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
