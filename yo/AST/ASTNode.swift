//
//  ASTNode.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct SourceCodeLocation {
    let path: String
    let range: Range<Int>
    
    var filename: String {
        return path.lastPathComponent
    }
}

// TODO use this instead of `AST` throughout the entire codebase!
typealias AST = [ASTNode]


protocol ASTNode: class, CustomStringConvertible {
    /// position in the original source code
    //var sourceCodeLocation: SourceCodeLocation { get } // TODO
    // Get all identifiers accessed by this node or its child nodes
    var accessedIdentifiers: [ASTIdentifier] { get }
}

protocol ASTStatement: ASTNode {}

protocol ASTExpression: ASTNode {}



extension Array where Element == ASTStatement {
    var accessedIdentifiers: [ASTIdentifier] {
        return self.lk_flatMap { $0.accessedIdentifiers }
    }
}

extension Array where Element == ASTExpression {
    var accessedIdentifiers: [ASTIdentifier] {
        return self.lk_flatMap { $0.accessedIdentifiers }
    }
}


// MARK: Node+Debug

extension ASTNode {
    var description: String {
        var desc = ""
        
        let mirror = Mirror(reflecting: self)
        desc += "\(type(of: self)) ["
        
        let children = mirror.children
        
        if children.isEmpty {
            return desc + "]"
        }
        desc += "\n"
        
        for (offset, child) in children.enumerated() {
            guard case let (label?, value) = child else { continue }
            
            desc.append(withIndentation: 2, "\(label): \(String(describing: value))")
            if AnyIndex(offset + 1) != children.endIndex {
                desc += ","
            }
            desc += "\n"
        }
        desc += "]"
        
        return desc
    }
}


extension Array where Element == ASTNode {
    var ast_description: String {
        return self.enumerated().reduce(into: "Array<Node> [\n", { (desc, arg1) in
            desc.append(withIndentation: 2, arg1.element.description)
            if arg1.offset < endIndex.advanced(by: -1) {
                desc += ","
            }
            desc += "\n"
        }) + "]"
    }
}
