//
//  ASTArrayGetter.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTArrayGetter: ASTExpression {
    let target: ASTExpression
    let offset: ASTExpression
}
