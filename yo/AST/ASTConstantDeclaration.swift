//
//  ASTConstantDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 2019-01-28.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTConstantDeclaration: ASTStatement {
    let identifier: ASTIdentifier
    let type: ASTType
    let value: ASTExpression
    
    init(identifier: ASTIdentifier, type: ASTType, value: ASTExpression) {
        self.identifier = identifier
        self.type = type
        self.value = value
    }
}
