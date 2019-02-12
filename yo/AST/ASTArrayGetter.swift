//
//  ASTArrayGetter.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTArrayGetter: ASTExpression {
    let target: ASTExpression
    let offset: ASTExpression
    let typeOfAccessedField: ASTType? // if nil, typeof(target) will be used instead
    
    init(target: ASTExpression, offset: ASTExpression, typeOfAccessedField: ASTType? = nil) {
        self.target = target
        self.offset = offset
        self.typeOfAccessedField = typeOfAccessedField
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return [target, offset].accessedIdentifiers
    }
}
