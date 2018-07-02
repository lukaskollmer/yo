//
//  ASTBooleanLiteral.swift
//  yo
//
//  Created by Lukas Kollmer on 02.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTBooleanLiteral: ASTExpression {
    
    let value: Bool
    
    init(value: Bool) {
        self.value = value
    }
}
