//
//  ASTEnumDeclaration.swift
//  yo
//
//  Created by Lukas Kollmer on 05.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTEnumDeclaration: ASTStatement {
    let name: ASTIdentifier
    let cases: [ASTIdentifier]
    
    init(name: ASTIdentifier, cases: [ASTIdentifier]) {
        self.name = name
        self.cases = cases
    }
}
