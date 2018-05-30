//
//  ASTArrayLiteral.swift
//  yo
//
//  Created by Lukas Kollmer on 29.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTArrayLiteral: ASTExpression {
    let elements: [ASTExpression]
}
