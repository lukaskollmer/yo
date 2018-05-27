//
//  ASTTypeMemberFunctionCall.swift
//  yo
//
//  Created by Lukas Kollmer on 27.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTTypeMemberFunctionCall: ASTExpression {
    let target: ASTIdentifier
    let functionName: ASTIdentifier
    let arguments: [ASTExpression]
}
