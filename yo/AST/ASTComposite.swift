//
//  ASTComposite.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTComposite: ASTStatement {
    let statements: [ASTStatement]
    let introducesNewScope: Bool
    
    init(statements: [ASTStatement], introducesNewScope: Bool = true) { // TODO remove the default value?
        self.statements = statements
        self.introducesNewScope = introducesNewScope
    }
    
    
    func appending(statements otherStatements: [ASTStatement]) -> ASTComposite {
        return ASTComposite(statements: statements + otherStatements, introducesNewScope: introducesNewScope)
    }
}


extension ASTComposite: ExpressibleByArrayLiteral {
    init(arrayLiteral elements: ASTStatement...) {
        self.init(statements: elements)
    }
}
