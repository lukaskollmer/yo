//
//  BuiltinIdentifiers.swift
//  yo
//
//  Created by Lukas Kollmer on 02.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


extension ASTIdentifier {
    enum Builtin : String {
        case function
    }
    
    var builtin_identifier: ASTIdentifier.Builtin {
        return ASTIdentifier.Builtin(rawValue: value)!
    }
    
    var builtin_type: ASTType {
        guard self.isBuiltin else {
            fatalError("this only works for builtin identifiers")
        }
        
        switch builtin_identifier {
        case .function:
            return .String
        }
    }
}
