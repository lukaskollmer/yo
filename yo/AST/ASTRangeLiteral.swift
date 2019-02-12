//
//  ASTRangeLiteral.swift
//  yo
//
//  Created by Lukas Kollmer on 29.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTRangeLiteral: ASTExpression {
    enum Kind {
        case inclusive
        case exclusive
    }
    
    let start: ASTExpression
    let end: ASTExpression
    let kind: Kind
    
    init(start: ASTExpression, end: ASTExpression, kind: Kind) {
        self.start = start
        self.end = end
        self.kind = kind
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return [start, end].accessedIdentifiers
    }
}
