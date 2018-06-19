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
    
    init(target: ASTExpression, offset: ASTExpression) {
        self.target = target
        self.offset = offset
    }
}
