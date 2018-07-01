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
    let type: ASTType
    
    init(value: Int, type: ASTType) {
        self.value = value
        self.type = type
    }
    
    convenience init(value: Int) {
        self.init(value: value, type: .int)
    }
    
    convenience init(value: Double) {
        var _value = value
        self.init(value: cast(&_value), type: .double)
    }
}
