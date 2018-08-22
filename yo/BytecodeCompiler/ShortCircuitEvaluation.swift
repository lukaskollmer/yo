//
//  ShortCircuitEvaluation.swift
//  yo
//
//  Created by Lukas Kollmer on 21.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class CodeGenOptimizer {
    
}


extension Array {
    // TODO rewrite if possible?
    func contains(ofType _type: Any.Type) -> Bool {
        return self.any { Mirror(reflecting: $0).subjectType == _type }
    }
}

extension CodeGenOptimizer {
    
    static func canOptimizeToUseShortCircuitEvaluation(_ condition: ASTCondition) -> Bool {
        guard let binaryCondition = condition as? ASTBinaryCondition else { return false }
        
        return [binaryCondition.lhs, binaryCondition.rhs].contains(ofType: ASTBinaryCondition.self)
    }
    
    static func transformToUseShortCircuitEvaluation(_ conditionalStatement: ASTConditionalStatement) -> ASTConditionalStatement {
        guard let binaryCondition = conditionalStatement.condition as? ASTBinaryCondition else {
            fatalError("unable to apply short circuit evaluation")
        }
        
        // TODO this is a lot more difficult than it might seem at first
        
        let (a, b, c, d, e, f) = (true, true, true, true, true, true)
        
        
        // input
        
        if a && b && c && d {
            // foo
        } else {
            // bar
        }
        
        // output
        // problem: we have to make sure bar is evaluated only once!
        
        if a {
            if b {
                if c {
                    if d {
                        // goto foo
                    } else {
                        // goto bar
                        // goto .end
                    }
                } else {
                    // goto bar
                    // goto .end
                }
            } else {
                // goto bar
                // goto .end
            }
        } else {
            // goto bar
            // goto .end
        }
        //.foo:
        //.bar:
        //.end:
        
        
        /*let _main: () -> Void = { print("MAIN") }
        let _else: () -> Void = { print("MAIN") }
        
        
        if id(true) && id(true) || id(false) {
            _main()
        } else {
            _else()
        }
        
        
        if id(true) {
            if id(true) {
                _main()
            } else {
                _else()
            }
        } else if id(false) {
            _main()
        
        } else {
            _else()
        }*/
        
        test()
        fatalError("TODO")
    }
}



func makeCond(_ name: String, _ arg0: @autoclosure @escaping () -> Bool) -> () -> Bool {
    return {
        print("[makeCond_eval]", name)
        return arg0()
    }
}


func test() {
    let cond1 = makeCond("true", true)
    let cond2 = makeCond("false", false)
    
    let _main = { print("MAIN") }
    let _else = { print("ELSE") }
    
    
    if true || false {
        print("TRUE")
    } else {
        print("FALSE")
    }
    
    // if cond1 && cond2
    if cond1() { 
        if cond2() {
            _main()
        } else {
            _else()
        }
    } else {
        _else()
    }
    
    
    
}







