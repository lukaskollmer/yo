//
//  ASTAnnotation.swift
//  yo
//
//  Created by Lukas Kollmer on 23.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTAnnotation {
    struct Element: Equatable {
        enum Value: Equatable {
            case bool(Bool)
            case string(String)
            case number(Int)
        }
        
        let key: String
        let value: Value
    }
    
    let elements: [Element]
    
    init(elements: [Element]) {
        self.elements = elements
    }
}


extension ASTAnnotation.Element: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        self.init(key: value, value: .bool(true))
    }
    
    
}
