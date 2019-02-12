//
//  ASTArbitraryNodes.swift
//  yo
//
//  Created by Lukas Kollmer on 2019-01-22.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTArbitraryNodes: ASTStatement & ASTExpression {
    enum TypeInferenceHelper {
        case type(ASTType)
        case expression(ASTExpression)
    }
    
    let nodes: [ASTNode]
    let typeInferenceHelper: TypeInferenceHelper
    
    init(nodes_inferringTypeFromFirst nodes: [ASTNode]) {
        self.nodes = nodes
        self.typeInferenceHelper = .expression(nodes[0] as! ASTExpression)
    }
    
    init(nodes: [ASTNode], typeInferenceHelper: TypeInferenceHelper) {
        self.nodes = nodes
        self.typeInferenceHelper = typeInferenceHelper
    }
    
    
    var accessedIdentifiers: [ASTIdentifier] {
        return nodes.lk_flatMap { $0.accessedIdentifiers }
    }
}
