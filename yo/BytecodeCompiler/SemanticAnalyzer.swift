//
//  SemanticAnalyzer.swift
//  yo
//
//  Created by Lukas Kollmer on 30.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

extension Dictionary where Key == String, Value == FunctionSignature {
    func contains(_ functionDeclaration: ASTFunctionDeclaration) -> Bool {
        return self.keys.contains(functionDeclaration.mangledName)
    }
    
    mutating func insert(functionDeclaration: ASTFunctionDeclaration) {
        self[functionDeclaration.mangledName] = functionDeclaration.signature
    }
}


class SemanticAnalyzer {
    struct Result {
        let globalFunctions: GlobalFunctions
        let types: [ASTTypeDeclaration]
        let globals: [ASTStaticVariableDeclaration]
        let enums: [ASTEnumDeclaration]
    }
    
    func analyze(ast: AST) -> SemanticAnalyzer.Result {
        let enums   = ast.compactMap { $0 as? ASTEnumDeclaration }
        let types   = ast.compactMap { $0 as? ASTTypeDeclaration }
        let globals = ast.compactMap { $0 as? ASTStaticVariableDeclaration }
        
        // TODO check that there aren't type &/ enum decls using the same name
        
        var functions = GlobalFunctions()
        
        // insert global functions
        ast
            .compactMap { $0 as? ASTFunctionDeclaration }
            .forEach { functions.insert(functionDeclaration: $0) }
        
        
        for implBlock in ast.compactMap({ $0 as? ASTTypeImplementation }) {
            if !implBlock.protocols.isEmpty {
                let type = types.first { $0.name == implBlock.typename }!
                
                // check whether the type already implements the protocol
                for protocolName in implBlock.protocols {
                    guard !type.protocols.contains(protocolName) else {
                        fatalError("Type '\(type.name)' already implements protocol '\(protocolName)'")
                    }
                    type.protocols.append(protocolName)
                }
            }
            
            for functionDecl in implBlock.functions {
                functions.insert(functionDeclaration: functionDecl)
            }
        }
        
        return SemanticAnalyzer.Result(globalFunctions: functions, types: types, globals: globals, enums: enums)
    }
}
