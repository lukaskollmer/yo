//
//  Scope.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct Scope {
    enum ScopeType {
        case global
        case function(name: String, returnType: String)
    }
    
    let type: ScopeType
    private var symbols = [String]()
    private var numberOfParameters = 0
    private var types = [String: String]()
    
    let parameters: [ASTVariableDeclaration]
    let localVariables: [ASTVariableDeclaration]
    
    
    init(type: ScopeType) {
        self.type = type
        self.parameters = []
        self.localVariables = []
    }
    
    init(type: ScopeType, parameters: [ASTVariableDeclaration], localVariables: [ASTVariableDeclaration]) {
        self.type = type
        self.parameters = parameters
        self.localVariables = localVariables
        
        numberOfParameters = parameters.count
        
        symbols.append(contentsOf: parameters.map     { $0.identifier.name })
        symbols.append(contentsOf: localVariables.map { $0.identifier.name })
        
        parameters.forEach     { types[$0.identifier.name] = $0.typename.name }
        localVariables.forEach { types[$0.identifier.name] = $0.typename.name }
    }
    
    var size: Int {
        return symbols.count
    }
    
    
    func index(of name: String) -> Int {
        let index = symbols.index(of: name)! + 1 // TODO don't force unwrap
        
        if index <= numberOfParameters {
            return -(index - 1)
        } else {
            return index - numberOfParameters
        }
    }
    
    func type(of name: String) -> String {
        return types[name]!
    }
    
    
    func withType(_ newType: ScopeType, newParameters: [ASTVariableDeclaration]) -> Scope {
        return Scope(
            type: newType,
            parameters: self.parameters + newParameters,
            localVariables: localVariables
        )
    }
    
    
    func adding(localVariables newLocals: [ASTVariableDeclaration]) -> Scope {
        return Scope(
            type: self.type,
            parameters: self.parameters,
            localVariables: self.localVariables + newLocals
        )
    }
    
    
    func isObject(identifier: String) -> Bool {
        return type(of: identifier) != "int"
    }
}
