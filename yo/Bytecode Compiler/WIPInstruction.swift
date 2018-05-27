//
//  WIPInstruction.swift
//  yo
//
//  Created by Lukas Kollmer on 27.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


enum WIPInstruction {
    case label(String)                  // A label
    case operation(Operation, Int)      // A "finalized" instruction
    case unresolved(Operation, String)  // An instruction that takes an address as parameter, which will be resolved later (after all codegen finished)
}


extension Array where Element == WIPInstruction {
    
    func finalized() -> [Instruction] {
        return self.enumerated().map { index, instruction in
            switch instruction {
            case .operation(let operation, let immediate):
                return operation.encode(withImmediate: immediate)
            case .unresolved(let operation, let label):
                return operation.encode(withImmediate: getAddress(ofLabel: label))
            case .label(_):
                return 0
            }
        }
    }
    
    func getAddress(ofLabel label: String) -> Int {
        return self.index { instruction in
            if case .label(let name) = instruction {
                return name == label
            }
            return false
            }!
    }
    
    
    var fancyDescription: String {
        var desc = [String]()
        
        let line: (Int, String, String) -> () = { idx, operation, immediate in
            
            let lineNumber = String(describing: idx).padding(.left, toLength: 3, withPad: "0")
            let op = immediate.isEmpty ? operation : operation.padding(.right, toLength: 8, withPad: " ")
            desc.append("  [\(lineNumber)] \(op) \(immediate)")
        }
        
        for (idx, element) in self.enumerated() {
            
            switch element {
            case .label(let label):
                line(idx, label + ":", "")
            case .operation(let operation, let immediate):
                line(idx, String(describing: operation), "\(immediate)")
            case .unresolved(let operation, let unresolvedLabel):
                line(idx, String(describing: operation), unresolvedLabel)
            }
        }
        
        return desc.joined(separator: "\n")
    }
}
