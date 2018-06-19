//
//  ASTTypecast.swift
//  yo
//
//  Created by Lukas Kollmer on 18.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTTypecast: ASTExpression {
    let expression: ASTExpression
    let type: ASTType
    
    init(expression: ASTExpression, type: ASTType) {
        self.expression = expression
        self.type = type
    }
}
