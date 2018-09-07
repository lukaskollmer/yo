//
//  ASTAnnotation.swift
//  yo
//
//  Created by Lukas Kollmer on 23.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

// TODO The whole annotation thing needs a rewrite
// the differentiation between ASTAnnotation and ASTAnnotation.Element is a bit reductive
// since we work w/ Array<ASTAnnotation.Element> most of the time and the ASTAnnotation objects are basically only present during parsing

class ASTAnnotation {
    struct Element: Equatable {
        enum Value: Equatable {
            case bool(Bool)
            case string(String)
            case number(Int)
        }
        
        static let disable_arc: Element = "disable_arc"
        static let static_initializer: Element = "static_initializer"
        static let static_cleanup: Element = "static_cleanup"
        static let base_protocol: Element = "base_protocol"
        static let attribute_accessors: Element = "attribute_accessors"
        static let variadic: Element = "variadic"
        static let pass_unchecked: Element = "pass_unchecked"
        static let disable_header_field: Element = "disable_header_field"
        
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
    var annotations: [ASTAnnotation.Element] { get }
}

extension ASTTypeThatCanBeAnnotated {
    func hasAnnotation(_ annotation: ASTAnnotation.Element) -> Bool {
        return annotations.contains(annotation)
    }
}

extension Array where Element == ASTAnnotation {
    var allElements: [ASTAnnotation.Element] {
        return self.reduce(into: []) { $0.append(contentsOf: $1.elements) }
    }
}

