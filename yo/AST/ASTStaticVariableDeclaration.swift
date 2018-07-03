//
//  ASTStaticVariableDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 03.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTStaticVariableDeclaration: ASTStatement {
    let identifier: ASTIdentifier
    let type: ASTType
    let initialValue: ASTExpression?
    
    init(identifier: ASTIdentifier, type: ASTType, initialValue: ASTExpression?) {
        self.identifier = identifier
        self.type = type
        self.initialValue = initialValue
    }
}
