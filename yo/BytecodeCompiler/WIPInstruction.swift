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
    case arrayLiteral(String, [Int])    // TODO write desc
    
    
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


extension Array where Element == WIPInstruction {
    
    
    func withArrayLiteralsResolved() -> [WIPInstruction] {
        
        var _self = self
        let arrayliterals = _self.remove { $0.isArrayLiteral }
        _self.insert(contentsOf: arrayliterals, at: 4)
        
        return _self.lk_flatMap { instruction in
            if case WIPInstruction.arrayLiteral(let label, let array) = instruction {
                return [
                    WIPInstruction.label(label),
                    WIPInstruction.operation(.noop, array.count)
                ] + array.map { WIPInstruction.operation(.noop, $0) }
            }
            return [instruction]
        }
    }
    
    // make sure all labels have odd addresses
    // more info in the documentation // TODO
    // TODO only apply this to function entry points?
    func withLabelsPadded() -> [WIPInstruction] {
        var retval = [WIPInstruction]()
        
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
    
    func finalized() -> [Instruction] {
        // TODO move withArrayLiteralsResolved back into finalized?
        return self.map { instruction in
            switch instruction {
            case .operation(let operation, let immediate):
                return operation.encode(withImmediate: immediate)
            case .unresolved(let operation, let label):
                return operation.encode(withImmediate: getAddress(ofLabel: label))
            case .label(_):
                return 0
            case .arrayLiteral(_):
                fatalError() // should never reach here
            }
        }
    }
    
    func getAddress(ofLabel label: String) -> Index {
        return self.index { instruction in
            if case .label(let name) = instruction {
                return name == label
            }
            return false
        }!
    }
    
    
    
    func firstIndex(after index: Index, where block: (Element) -> Bool) -> Index {
        return self.enumerated().first { $0.offset > index && block($0.1) }!.offset
    }
    
    
    var fancyDescription: String {
        var desc = [String]()
        
        let line: (Int, String, String) -> () = { idx, operation, immediate in
            
            let lineNumber = String(describing: idx).padding(.left, toLength: 4, withPad: "0")
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
            case .arrayLiteral(let label, let array):
                line(idx, "ARRAY LITERAL", "TODO")
            }
        }
        
        return desc.joined(separator: "\n")
    }
}
