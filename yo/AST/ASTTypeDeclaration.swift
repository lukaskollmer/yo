//
//  ASTTypeDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTTypeDeclaration: ASTStatement, ASTTypeThatCanBeAnnotated {
    let name: ASTIdentifier
    let attributes: [ASTVariableDeclaration]
    var annotations: [ASTAnnotation.Element]
    
    init(name: ASTIdentifier, attributes: [ASTVariableDeclaration], annotations: [ASTAnnotation.Element] = []) {
        self.name = name
        self.attributes = attributes
        self.annotations = annotations
    }
}
