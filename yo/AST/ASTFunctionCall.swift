//
//  ASTFunctionCall.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// TODO is adding conformance to both `ASTExpression` and `ASTStatement` really the best idea?
class ASTFunctionCall: ASTExpression & ASTStatement {
    let functionName: String    // TODO make ASTIdentifier? (not actually necessary but would save us some manual identifier inits all over the place)
    let arguments: [ASTExpression]
    let unusedReturnValue: Bool
    
    init(functionName: String, arguments: [ASTExpression], unusedReturnValue: Bool) {
        self.functionName = functionName
        self.arguments = arguments
        self.unusedReturnValue = unusedReturnValue
    }
}
