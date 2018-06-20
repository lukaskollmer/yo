//
//  ASTLambda.swift
//  yo
//
//  Created by Lukas Kollmer on 18.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// An anonymous function
// Doesn't (yet) support captured variables // TODO
class ASTLambda: ASTExpression {
    var signature: ASTType // is always .function
    let parameterNames: [ASTIdentifier]
    let body: ASTComposite
    
    init(signature: ASTType, parameterNames: [ASTIdentifier], body: ASTComposite) {
        
        // TODO
        //guard case .function(_) = signature else {
        //    fatalError() // TODO throw instead
        //}
        self.signature = signature
        self.parameterNames = parameterNames
        self.body = body
    }
}
