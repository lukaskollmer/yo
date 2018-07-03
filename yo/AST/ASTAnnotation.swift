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
        
        static let disable_arc = Element(key: "disable_arc", value: .bool(true))
        static let static_initializer = Element(key: "static_initializer", value: .bool(true))
        static let static_cleanup = Element(key: "static_cleanup", value: .bool(true))
        
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


// TODO come up w/ a better name
protocol ASTTypeThatCanBeAnnotated {
    var annotations: [ASTAnnotation.Element] { get set }
}

extension ASTTypeThatCanBeAnnotated {
    func hasAnnotation(_ annotation: ASTAnnotation.Element) -> Bool {
        return annotations.contains(annotation)
    }
}

