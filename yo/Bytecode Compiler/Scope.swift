//
//  Scope.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// TODO add support for type info?
struct Scope {
    enum ScopeType {
        case global
        case function(String)
    }
    
    let type: ScopeType
    private var symbols = [String]()
    private var numberOfParameters = 0
    
    
    init(type: ScopeType) {
        self.type = type
    }
    
    init(type: ScopeType, parameters: [ASTIdentifier], localVariables: [ASTIdentifier]) {
        self.type = type
        numberOfParameters = parameters.count
        
        symbols.append(contentsOf: parameters.map { $0.name })
        symbols.append(contentsOf: localVariables.map { $0.name })
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
}
