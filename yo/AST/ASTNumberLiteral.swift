//
//  ASTNumberLiteral.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTNumberLiteral: ASTExpression {
    let value: Int
    
    init(value: Int) {
        self.value = value
    }
}
