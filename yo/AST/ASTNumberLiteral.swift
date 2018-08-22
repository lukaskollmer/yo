//
//  ASTNumberLiteral.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTNumberLiteral: ASTExpression, ExpressibleByIntegerLiteral {
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
        self.init(value: value.unsafe_loadAsInt, type: .double)
    }
    
    convenience required init(integerLiteral value: Int) {
        self.init(value: value)
    }
}
