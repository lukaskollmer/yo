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


protocol ASTNode: _AssociatedObjectImp {
    /// position in the original source code
    //var sourceCodeLocation: SourceCodeLocation { get } // TODO
    func print(to printer: ASTPrinter)
}

protocol ASTStatement: ASTNode {}
protocol ASTExpression: ASTNode {}



extension Array where Element == ASTNode {
    var functions: [ASTFunctionDeclaration] {
        return self.lk_flatMap {
            if let fn = $0 as? ASTFunctionDeclaration {
                return [fn]
            } else if let impl = $0 as? ASTTypeImplementation {
                return impl.functions
            }
            return []
        }
    }
}
