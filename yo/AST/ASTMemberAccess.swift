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
        case initial_identifier(ASTIdentifier)
        case initial_functionCall(ASTFunctionCall)
        
        case attribute(name: ASTIdentifier)
        case functionCall(name: ASTIdentifier, arguments: [ASTExpression], unusedReturnValue: Bool)
    }
    
    let members: [Kind]
    
    init(members: [Kind]) {
        self.members = members
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return members.lk_flatMap {
            switch $0 {
            case .initial_identifier(let identifier):
                return [identifier]
            case .initial_functionCall(let functionCall):
                return functionCall.accessedIdentifiers
            case .attribute(name: _):
                return []
            case .functionCall(name: _, let arguments, unusedReturnValue: _):
                return arguments.accessedIdentifiers
            }
        }
    }
}
