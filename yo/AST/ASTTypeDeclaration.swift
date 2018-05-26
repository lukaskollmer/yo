//
//  ASTTypeDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTTypeDeclaration: ASTStatement {
    let name: ASTIdentifier
    let attributes: [ASTVariableDeclaration]
}
