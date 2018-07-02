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
    var protocols: [ASTIdentifier]
    var annotations: [ASTAnnotation.Element]
    
    var isStruct: Bool {
        return annotations.contains("struct")
    }
    
    init(name: ASTIdentifier, attributes: [ASTVariableDeclaration], protocols: [ASTIdentifier] = [], annotations: [ASTAnnotation.Element] = []) {
        self.name = name
        self.attributes = attributes
        self.protocols = protocols
        self.annotations = annotations
    }
}
