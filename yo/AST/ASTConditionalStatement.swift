//
//  ASTConditionalStatement.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// An `ASTConditionalStatement` can be either a if(/else) or a while statement
struct ASTConditionalStatement: ASTStatement {
    enum Kind {
        case `while`
        case `if`(ASTComposite?)
    }
    
    let condition: ASTCondition
    let body: ASTComposite
    let kind: Kind
}