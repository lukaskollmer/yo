//
//  ASTInlineBooleanExpression.swift
//  yo
//
//  Created by Lukas Kollmer on 06.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTInlineBooleanExpression: ASTExpression {
    let condition: ASTCondition
    
    init(condition: ASTCondition) {
        self.condition = condition
    }
}
