//
//  Optimizer.swift
//  yo
//
//  Created by Lukas Kollmer on 26.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct Optimizations: OptionSet {
    let rawValue: Int
    
    static let unusedSymbols = Optimizations(rawValue: 1 << 0)
    
    static let all: Optimizations = [.unusedSymbols]
}

class Optimizer {
    private var instructions: [WIPInstruction]
    let ast: AST
    private let stats: BytecodeCompiler.CompilationStats
    
    init(instructions: [WIPInstruction], ast: AST, stats: BytecodeCompiler.CompilationStats) {
        self.instructions = instructions
        self.ast = ast
        self.stats = stats
    }
    
    func optimize(_ optimizations: Optimizations) -> [WIPInstruction] {
        if optimizations.contains(.unusedSymbols) {
            _unusedSymbols()
        }
        
        return instructions
    }
    
    
    private func _unusedSymbols() {
        let unusedFunctions = ast.functions
            .filter { !$0.annotations.contains("unused") && !stats.calledFunctions.contains($0.mangledName) }
            .map { $0.mangledName }
        
        for fn in unusedFunctions {
            let entryPoint = instructions.getAddress(ofLabel: fn)
            let nextFunctionEntryPoint = instructions.firstIndex(after: entryPoint) { $0.isLabel && !$0.labelValue!.hasPrefix(".") }
            
            instructions.removeSubrange(entryPoint..<nextFunctionEntryPoint)
        }
    }
}
