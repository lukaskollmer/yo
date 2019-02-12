//
//  ASTConstantDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 2019-01-28.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTConstantDeclaration: ASTStatement {
    let annotations: [ASTAnnotation.Element]
    let identifier: ASTIdentifier
    let type: ASTType
    let value: ASTExpression
    
    init(annotations: [ASTAnnotation.Element] = [], identifier: ASTIdentifier, type: ASTType, value: ASTExpression) {
        self.annotations = annotations
        self.identifier = identifier
        self.type = type
        self.value = value
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return value.accessedIdentifiers
    }
}
