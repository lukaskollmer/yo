//
//  ASTRawOperation.swift
//  yo
//
//  Created by Lukas Kollmer on 29.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


// A "raw" instruction (instruction + immediate)
class ASTRawWIPInstruction: ASTStatement & ASTExpression {
    let instruction: WIPInstruction
    
    init(instruction: WIPInstruction) {
        self.instruction = instruction
    }
    
    init(operation: Operation, immediate: Int) {
        self.instruction = .operation(operation, immediate)
    }
}
