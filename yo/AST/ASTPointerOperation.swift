//
//  ASTPointerOperation.swift
//  yo
//
//  Created by Lukas Kollmer on 2019-01-25.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTPointerOperation: ASTExpression {
    enum Operation {
        case ref, ref_absolute
        case deref
    }
    
    let operation: Operation
    let target: ASTExpression
    
    init(operation: Operation, target: ASTExpression) {
        self.operation = operation
        self.target = target
    }
}
