//
//  ASTTypeImplementation.swift
//  yo
//
//  Created by Lukas Kollmer on 27.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTTypeImplementation: ASTStatement {
    let typename: ASTIdentifier
    let functions: [ASTFunctionDeclaration]
}
