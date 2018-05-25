//
//  ASTFunctionCall.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct ASTFunctionCall: ASTExpression {
    let functionName: String
    let arguments: [ASTExpression]
    //let unusedReturnValue: Bool // TODO implement
}
