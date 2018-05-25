//
//  ASTIfStatement.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTIfStatement: ASTStatement {
    let condition: ASTCondition
    let body: ASTStatement
    let elseBranch: ASTStatement?
}
