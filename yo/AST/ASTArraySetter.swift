//
//  ASTArraySetter.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTArraySetter: ASTStatement {
    let target: ASTExpression
    let offset: ASTExpression
    let value: ASTExpression
    let typeOfWrittenValue: ASTType?
    
    init(target: ASTExpression, offset: ASTExpression, value: ASTExpression, typeOfWrittenValue: ASTType? = nil) {
        self.target = target
        self.offset = offset
        self.value  = value
        self.typeOfWrittenValue = typeOfWrittenValue
    }
}
