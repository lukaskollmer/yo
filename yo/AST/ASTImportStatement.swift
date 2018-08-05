//
//  ASTImportStatement.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTImportStatement: ASTStatement {
    let moduleName: String
    
    init(moduleName: String) {
        self.moduleName = moduleName
    }
}
