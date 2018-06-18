//
//  ASTTypecast.swift
//  yo
//
//  Created by Lukas Kollmer on 18.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTTypecast: ASTExpression {
    let expression: ASTExpression
    let type: ASTType
}
