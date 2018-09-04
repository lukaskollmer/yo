//
//  ASTStructDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 04.09.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTStructDeclaration: ASTStatement, ASTTypeThatCanBeAnnotated {
    let identifier: ASTIdentifier
    let attributes: [ASTVariableDeclaration]
    var protocols: [ASTIdentifier] = []
    var annotations: [ASTAnnotation.Element]
    
    var hasArcEnabled: Bool {
        return !self.hasAnnotation(.disable_arc)
    }
    
    init(identifier: ASTIdentifier, attributes: [ASTVariableDeclaration], annotations: [ASTAnnotation] = []) {
        self.annotations = annotations.allElements
        self.identifier = identifier
        self.attributes = attributes
    }
}
