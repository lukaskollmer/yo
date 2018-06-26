//
//  SemanticAnalyzer.swift
//  yo
//
//  Created by Lukas Kollmer on 30.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class SemanticAnalyzer {
    
    typealias FunctionInfo = (argc: Int, parameterTypes: [ASTType], returnType: ASTType, annotations: [ASTAnnotation.Element])
    
    struct Result {
        
        let globalFunctions: [String: FunctionInfo]
        let types: [ASTTypeDeclaration]
    }
    
    
    
    func analyze(ast: [ASTNode]) -> SemanticAnalyzer.Result {
        var types = [ASTTypeDeclaration]()
        var functions = [String: FunctionInfo]()
        
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
                functions[SymbolMangling.mangleInitializer(forType: typeDecl.name.name)] = (
                    argc: typeDecl.attributes.count,
                    parameterTypes: typeDecl.attributes.map { $0.type },
                    returnType: .complex(name: typeDecl.name.name),
                    annotations: []
                )
                
            } else if let functionDecl = node as? ASTFunctionDeclaration {
                handleFunction(functionDecl)
                
            } else if let impl = node as? ASTTypeImplementation {
                impl.functions.forEach(handleFunction)
            }
        }
        
        return SemanticAnalyzer.Result(globalFunctions: functions, types: types)
    }
}

