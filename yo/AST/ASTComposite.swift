//
//  ASTComposite.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTComposite: ASTStatement, ExpressibleByArrayLiteral {
    let statements: [ASTStatement]
    let introducesNewScope: Bool // TODO get rid of this
    var isUnsafe: Bool
    
    init(statements: [ASTStatement], introducesNewScope: Bool = true, isUnsafe: Bool = false) { // TODO remove the default value?
        self.statements = statements
        self.introducesNewScope = introducesNewScope
        self.isUnsafe = isUnsafe
    }
    
    convenience required init(arrayLiteral elements: ASTStatement...) {
        self.init(statements: elements)
    }
    
    
    func appending(statements otherStatements: [ASTStatement]) -> ASTComposite {
        return ASTComposite(statements: statements + otherStatements, introducesNewScope: introducesNewScope)
    }
}


