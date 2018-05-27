//
//  ASTImportStatement.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTImportStatement: ASTStatement {
    let moduleName: String
    let isInternal: Bool // as of right now, only internal modules are supported
}
