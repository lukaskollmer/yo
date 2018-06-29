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
                let typename = typeDecl.name.name
                let type = ASTType.complex(name: typename)
                
                functions[SymbolMangling.mangleInitializer(forType: typename)] = (
                    argc: typeDecl.attributes.count,
                    parameterTypes: typeDecl.attributes.map { $0.type },
                    returnType: type,
                    annotations: []
                )
                
                typeDecl.attributes.filter({ !$0.identifier.name.hasPrefix("_") }).forEach { attribute in
                    // getter
                    functions[SymbolMangling.mangleGetter(forType: typename, attributeName: attribute.identifier.name)] = (
                        argc: 1,
                        parameterTypes: [type],
                        returnType: attribute.type,
                        annotations: ["disable_arc"]
                    )
                    
                    // setter
                    functions[SymbolMangling.mangleSetter(forType: typename, attributeName: attribute.identifier.name)] = (
                        argc: 2,
                        parameterTypes: [type, attribute.type],
                        returnType: .void,
                        annotations: ["disable_arc"]
                    )
                }
                
                
                
            } else if let functionDecl = node as? ASTFunctionDeclaration {
                handleFunction(functionDecl)
                
            } else if let impl = node as? ASTTypeImplementation {
                impl.functions.forEach(handleFunction)
            }
        }
        
        return SemanticAnalyzer.Result(globalFunctions: functions, types: types)
    }
}
