//
//  ASTIdentifier.swift
//  yo
//
//  Created by Lukas Kollmer on 25.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTIdentifier: ASTExpression, Equatable {
    let name: String
    
    init(name: String) {
        self.name = name
    }
    
    
    static func == (lhs: ASTIdentifier, rhs: ASTIdentifier) -> Bool {
        return lhs.name == rhs.name
    }
}

