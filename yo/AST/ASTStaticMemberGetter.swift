//
//  ASTStaticMemberGetter.swift
//  yo
//
//  Created by Lukas Kollmer on 05.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTStaticMemberGetter: ASTExpression {
    let typename: ASTIdentifier
    let memberName: ASTIdentifier
    
    init(typename: ASTIdentifier, memberName: ASTIdentifier) {
        self.typename = typename
        self.memberName = memberName
    }
    
    var accessedIdentifiers: [ASTIdentifier] {
        return []
    }
}
