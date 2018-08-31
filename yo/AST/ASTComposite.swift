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
    var isUnsafe: Bool
    
    init(statements: [ASTStatement], isUnsafe: Bool = false) { // TODO remove the default value?
        self.statements = statements
        self.isUnsafe = isUnsafe
    }
    
    convenience required init(arrayLiteral elements: ASTStatement...) {
        self.init(statements: elements)
    }
    
    func appending(statements otherStatements: [ASTStatement]) -> ASTComposite {
        return ASTComposite(statements: statements + otherStatements, isUnsafe: self.isUnsafe)
    }
}


