//
//  ASTArrayLiteral.swift
//  yo
//
//  Created by Lukas Kollmer on 29.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTArrayLiteral: ASTExpression {
    enum Kind {
        case primitive
        case complex
    }
    
    let elements: [ASTExpression]
    let kind: Kind
    
    init(elements: [ASTExpression], kind: Kind = .complex) {
        self.elements = elements
        self.kind = kind
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return elements.accessedIdentifiers
    }
}
