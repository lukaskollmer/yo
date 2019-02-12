//
//  Scope.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct Scope {
    enum Error: Swift.Error {
        case unknownSymbol(String)
    }
    
    enum ScopeType: Equatable {
        case global
        case function(name: String, returnType: ASTType)
    }
    
    let type: ScopeType
    private var symbols = [String]()
    private var numberOfParameters = 0
    private var types = [String: ASTType]()
    
    var isUnsafe: Bool = false
    
    let parameters: [ASTVariableDeclaration]
    let localVariables: [ASTVariableDeclaration]
    
    
    init(type: ScopeType, parameters: [ASTVariableDeclaration] = [], localVariables: [ASTVariableDeclaration] = [], isUnsafe: Bool = false) {
        self.type = type
        self.parameters = parameters
        self.localVariables = localVariables
        self.isUnsafe = isUnsafe
        
        numberOfParameters = parameters.count
        
        symbols.append(contentsOf: parameters.map     { $0.identifier.value })
        symbols.append(contentsOf: localVariables.map { $0.identifier.value })
        
        parameters.forEach     { types[$0.identifier.value] = $0.type }
        localVariables.forEach { types[$0.identifier.value] = $0.type }
        
        
    }
    
    var size: Int {
        return symbols.count
    }
    
    
    var functionName: String? {
        switch type {
        case .global:
            return nil
        case .function(let name, _):
            return name
        }
    }
    
    
    func index(of name: String) throws -> Int {
        guard let index = symbols.index(of: name)?.advanced(by: 1) else {
            throw Error.unknownSymbol(name)
        }
        
        if index <= numberOfParameters {
            return -(index - 1)
        } else {
            return index - numberOfParameters
        }
    }
    
    func type(of name: String) throws -> ASTType {
        guard let type = types[name] else {
            throw Error.unknownSymbol(name)
        }
        return type
    }
    
    
    func adding(localVariables newLocals: [ASTVariableDeclaration]) -> Scope {
        return Scope(
            type: self.type,
            parameters: self.parameters,
            localVariables: self.localVariables + newLocals,
            isUnsafe: self.isUnsafe
        )
    }
    
    
    func isObject(identifier: String) throws -> Bool {
        if case .complex(_) = try type(of: identifier) {
            return true
        }
        return false
    }
    
    
    func contains(identifier: String) -> Bool {
        return (try? self.index(of: identifier)) == nil ? false : true
    }
}
