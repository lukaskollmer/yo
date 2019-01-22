//
//  CodeGen+Globals.swift
//  yo
//
//  Created by Lukas Kollmer on 03.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

// How do global variables work?
// At startup time (before calling main), we call all functions w/ the `static_initializer` attribute
// The synthesized `__initialize_globals` function is always called first
// `__initialize_globals` allocates an array at the beginning of the heap (address: 2)
// This array is used as the "registry" of all global variables and stores the pointers to each global variable
// (or, if they are primitives, their values)
// We then (still in __initialize_globals) fill that array with the initial values provided for all global variables.
// If a variable does not have an initial value, we simply leave that slot in the register empty (aka 0)

// After filling all initial values, we call all other functions with the `static_initializer` attribute
// At shutdown (ie after `main` returns), we call all functions w/ the `static_cleanup` attribute
// We first call `__release_globals`, which frees/releases all globals that have an initial value
// After that, we call all other functions w/ the `static_cleanup` attribute
// At the end, we call `__free_global_variable_registry`, which frees the array allocated at the beginning of `__initialize_globals`

extension AutoSynthesizedCodeGen {
    
    
    func handleGlobals() -> AST {
        let globals = compiler.globals
        var retval = AST()
        
        if globals.isEmpty {
            // No globals
            // However, we still create empty stubs for `__INVOKING_ALL_STATIC_INITIALIZERS__` and `__INVOKING_ALL_STATIC_CLEANUP_FUNCTIONS__`
            // Why? these two functions are called before we call main (and therefore before any instructions is generated)
            // Therefore, we can't yet check whether these functions even exist
            // See the boostrapping code in the compiler thing
            
            
            for functionName in ["__INVOKING_ALL_STATIC_INITIALIZERS__", "__INVOKING_ALL_STATIC_CLEANUP_FUNCTIONS__"] {
                let fn = ASTFunctionDeclaration(
                    signature: ASTFunctionSignature(name: ASTIdentifier(functionName), kind: .global, parameters: [], returnType: .void),
                    body: []
                )
                compiler.functions.insert(functionDeclaration: fn)
                retval.append(fn)
            }
            
            return retval
        }
        
        
        
        
        
        // All static initializers, in the following order:
        // 1. metatype generation
        // 2. `__initialize_globals`
        // 3. all other custom ones
        // This allows us to access global variables w/ an initial value in custom static initializers
        
        let allStaticInitializers = compiler.functions.filter({ $0.value.annotations.contains(.static_initializer) }).keys
        let isMetatypeInitializer: (String) -> Bool = { $0.hasPrefix("__") && $0.hasSuffix("_metatype_init") }
        
        let invokeStaticInitializers = ASTFunctionDeclaration(
            signature: ASTFunctionSignature(
                name: ASTIdentifier("__INVOKING_ALL_STATIC_INITIALIZERS__"),
                kind: .global,
                parameters: [],
                returnType: .void
            ),
            body: [
                // Allocate space for the global variables
                // Since this is the very first alloc call, we know for a fact that the address of the allocates heap space is 16
                // We allocate twice the space, so that all globals have even addresses, which is important for the runtime to treat them as objects
                ASTFunctionCall(functionName: SymbolMangling.alloc, arguments: [ASTNumberLiteral(value: globals.count * TypeCache.sizeof([.i64]))], unusedReturnValue: true),
                
                // Call all metatype initializers
                ASTComposite(statements:
                    allStaticInitializers.filter({ isMetatypeInitializer($0) }).map { ASTFunctionCall(functionName: $0, arguments: [], unusedReturnValue: true) }
                ),
                
                // Set all supplied initial values
                ASTComposite(statements:
                    globals
                        .filter { $0.initialValue != nil }
                        .map { global in
                            
                        let value: ASTExpression = {
                            let initialValue = global.initialValue!
                            return global.type.supportsReferenceCounting
                                ? ASTFunctionCall(functionName: SymbolMangling.retain, arguments: [initialValue], unusedReturnValue: false)
                                : initialValue
                        }()
                        
                        return ASTArraySetter(
                            target: ASTNumberLiteral(value: 0).as(.i8), // beginning of heap
                            offset: ASTNumberLiteral(value: compiler._actualAddressOfGlobal(withIdentifier: global.identifier)!),
                            value: value
                        )
                    }
                ),

                // Call all custom static initializers
                ASTComposite(statements:
                    allStaticInitializers.filter({ !isMetatypeInitializer($0) }).map { ASTFunctionCall(functionName: $0, arguments: [], unusedReturnValue: true) }
                )
            ]
        )
        compiler.functions.insert(functionDeclaration: invokeStaticInitializers)
        retval.append(invokeStaticInitializers)
        
        
        
        // All static cleanup functions, in the following order:
        // 1. all custom ones
        // 2. `__release_globals`
        // 3. the metatype__free ones
        
        
        let allStaticCleanupFunctions = compiler.functions
            .filter({ $0.value.annotations.contains(.static_cleanup) })
            .keys
            .filter { $0 != SymbolMangling.mangleMetatypeCleanupFunction(forType: "String")}
        
        let isMetatypeCleanup: (String) -> Bool = { $0.hasPrefix("__") && $0.hasSuffix("_metatype_free") }
        
        
        let invokeStaticCleanupFunctions = ASTFunctionDeclaration(
            signature: ASTFunctionSignature(
                name: ASTIdentifier("__INVOKING_ALL_STATIC_CLEANUP_FUNCTIONS__"),
                kind: .global,
                parameters: [],
                returnType: .void
            ),
            body: [
                // call all custom cleanup functions
                ASTComposite(statements:
                    allStaticCleanupFunctions.filter({ !isMetatypeCleanup($0) }).map { ASTFunctionCall(functionName: $0, arguments: [], unusedReturnValue: true) }
                ),
                
                // release all complex globals w/ an initial value
                ASTComposite(statements:
                    globals
                        .filter { $0.type.supportsReferenceCounting }
                        .map { ASTFunctionCall(functionName: SymbolMangling.release, arguments: [$0.identifier], unusedReturnValue: true) }
                ),
                
                // free all metatypes (except String)
                ASTComposite(statements:
                    allStaticCleanupFunctions.filter({ isMetatypeCleanup($0) }).map { ASTFunctionCall(functionName: $0, arguments: [], unusedReturnValue: true) }
                ),
                
                // free String's metatype
                // since all other metatypes contain a string (typename) we have to make sure that the string metatype sticks around until they've all been freed
                ASTFunctionCall(
                    functionName: SymbolMangling.mangleMetatypeCleanupFunction(forType: "String"),
                    arguments: [],
                    unusedReturnValue: true
                ),
                
                // free the space allocated for globals
                ASTFunctionCall(
                    functionName: SymbolMangling.free,
                    //arguments: [ASTNumberLiteral(value: )],
                    arguments: [
                        ASTNumberLiteral(value: 16)
                    ],
                    unusedReturnValue: true
                )
            ]
        )
        compiler.functions.insert(functionDeclaration: invokeStaticCleanupFunctions)
        retval.append(invokeStaticCleanupFunctions)
        
        return retval
    }
}
