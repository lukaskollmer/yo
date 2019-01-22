//
//  ASTRawOperation.swift
//  yo
//
//  Created by Lukas Kollmer on 29.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// A "raw" instruction (instruction + immediate)
class ASTRawUnresolvedInstruction: ASTStatement & ASTExpression {
    let instruction: UnresolvedInstruction
    
    init(instruction: UnresolvedInstruction) {
        self.instruction = instruction
    }
    
    init(operation: Operation, immediate: Int) {
        self.instruction = .operation(operation, immediate)
    }
}
