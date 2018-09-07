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
    
    // TODO get rid of this or replace it w/ `hasHeaderFieldDisabled`
    // Not disabling the header doesn't mean that arc is supported/enabled!!!
    var hasArcEnabled: Bool {
        return !self.hasAnnotation(.disable_header_field)
    }
    
    init(identifier: ASTIdentifier, attributes: [ASTVariableDeclaration], annotations: [ASTAnnotation] = []) {
        self.annotations = annotations.allElements
        self.identifier = identifier
        self.attributes = attributes
    }
}
