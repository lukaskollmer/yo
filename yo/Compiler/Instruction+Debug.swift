//
//  Instruction+Debug.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation



// String + padding
extension String {
    
    enum PadDirection {
        case left, right
    }
    
    func padding<T: StringProtocol>(_ padDirection: PadDirection, toLength length: Int, withPad pad: T) -> String {
        switch padDirection {
        case .right:
            return self.padding(toLength: length, withPad: pad, startingAt: 0)
        case .left:
            return String.init(repeating: pad as! String, count: length - self.count) + self
        }
    }
}


// TODO is this actually used anywhere?
extension Array where Element == Instruction {
    var fancyDescription: String {
        var desc = [String]()
        
        for (idx, element) in self.enumerated() {
            let instruction = InstructionDescriptor(instruction: element)
            
            let lineNumber = String(describing: idx).padding(.left, toLength: 5, withPad: "0")
            let operation = instruction.operation.mnemonic.padding(.right, toLength: 8, withPad: " ")
            desc.append("  [\(lineNumber)] \(operation) \(instruction.immediate)")
        }
        
        return desc.joined(separator: "\n")
    }
}
