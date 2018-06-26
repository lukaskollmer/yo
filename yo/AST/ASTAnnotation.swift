//
//  ASTAnnotation.swift
//  yo
//
//  Created by Lukas Kollmer on 23.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTAnnotation {
    enum ElementValue {
        case bool(Bool)
        case string(String)
        case number(Int)
    }
    
    typealias Element = (String, ElementValue)
    
    let elements: [Element]
    
    init(elements: [Element]) {
        self.elements = elements
    }
}
