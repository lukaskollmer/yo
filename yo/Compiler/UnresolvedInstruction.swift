//
//  UnresolvedInstruction.swift
//  yo
//
//  Created by Lukas Kollmer on 27.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


enum UnresolvedInstruction {
    case label(String)                  // A label
    case operation(Operation, Int)      // A "finalized" instruction
    case unresolved(Operation, String)  // An instruction that takes an address as parameter, which will be resolved later (after all codegen finished)
    case arrayLiteral(String, Int, [Int])   // label : element size : elements
    case comment(String)
    case raw(Int)                       // Just a 64 bit wide integer
    
    var isLabel: Bool {
        return labelValue != nil
    }
    
    var isArrayLiteral: Bool {
        if case .arrayLiteral(_) = self { return true }
        return false
    }
    
    var labelValue: String? {
        if case .label(let label) = self { return label }
        return nil
    }
}



struct InstructionFinalizationResult {
    let instructions: [Instruction]
    let procedureEntryAddresses: [Int: String]
}



extension Array where Element == UnresolvedInstruction {
    func withArrayLiteralsResolved(insertionPoint: Int) -> [UnresolvedInstruction] {
        var _self = self
        
        // Insert all array literals after the bootstrapping code
        let arrayliterals = _self.remove { $0.isArrayLiteral }
        _self.insert(contentsOf: arrayliterals, at: insertionPoint)
        
        return _self.lk_flatMap { instruction in
            if case .arrayLiteral(let label, let elementSize, let elements) = instruction {
                return [
                    .label(label),
                    .operation(.noop, elementSize),
                    .operation(.noop, elements.count)
                ] + elements.map { .operation(.noop, $0) }
            }
            return [instruction]
        }
    }
    
    // make sure all labels have odd addresses
    // more info in the documentation // TODO
    // TODO only apply this to function entry points?
    func withLabelsPadded() -> [UnresolvedInstruction] {
        var retval = [UnresolvedInstruction]()
        
        for instruction in self {
            guard instruction.isLabel else {
                retval.append(instruction)
                continue
            }
            
            if retval.count.isEven {
                retval.append(.operation(.noop, 0))
            }
            retval.append(instruction)
        }
        return retval
    }
    
    
    func finalized() -> InstructionFinalizationResult {
        // TODO move withArrayLiteralsResolved back into finalized?
        var procedureEntryAddresses = [Int: String]()
        let instructions: [Instruction] = self.enumerated().map { index, instruction in
            switch instruction {
            case .operation(let operation, let immediate):
                return operation.encode(withImmediate: immediate)
            case .unresolved(let operation, let label):
                return operation.encode(withImmediate: getAddress(ofLabel: label))
            case .label(let label):
                procedureEntryAddresses[index] = label
                return 0
            case .comment(_):
                return 0
            case .raw(let value):
                return value
            case .arrayLiteral(_):
                fatalError() // should never reach here
            }
        }
        
        return InstructionFinalizationResult(instructions: instructions, procedureEntryAddresses: procedureEntryAddresses)
    }
    
    func getAddress(ofLabel label: String) -> Index {
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
            
            let lineNumber = String(describing: idx).padding(.left, toLength: 5, withPad: "0")
            let op = immediate.isEmpty ? operation : operation.padding(.right, toLength: 12, withPad: " ")
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
            case .arrayLiteral(_):
                line(idx, "ARRAY LITERAL", "TODO")
            case .comment(let comment):
                line(idx, ";" + comment, "")
            case .raw(let value):
                line(idx, "(raw)", String(value))
            }
        }
        
        return desc.joined(separator: "\n")
    }
}
