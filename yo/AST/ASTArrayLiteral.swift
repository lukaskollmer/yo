//
//  ASTArrayLiteral.swift
//  yo
//
//  Created by Lukas Kollmer on 29.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTArrayLiteral: ASTExpression {
    let elements: [ASTExpression]
    
    init(elements: [ASTExpression]) {
        self.elements = elements
    }
}
