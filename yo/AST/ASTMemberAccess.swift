//
//  ASTMemberAccess.swift
//  yo
//
//  Created by Lukas Kollmer on 15.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTMemberAccess: ASTExpression, ASTStatement {
    enum Kind {
        case attribute(name: ASTIdentifier)
        case functionCall(name: ASTIdentifier, arguments: [ASTExpression], unusedReturnValue: Bool)
    }
    
    let members: [Kind]
    
    init(members: [Kind]) {
        self.members = members
    }
}
