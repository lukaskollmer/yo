//
//  AutoSynthesizedCodeGen.swift
//  yo
//
//  Created by Lukas Kollmer on 30.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

private let _self = ASTIdentifier(value: "self")

/// Generates some functions for types:
/// - Initializers
/// - Attribute Getters and Setters
/// - Dealloc function
class AutoSynthesizedCodeGen {
    static func synthesize(for ast: [ASTNode], globalFunctions: inout GlobalFunctions, typeCache: TypeCache) -> [ASTNode] {
        var retval = [ASTNode]()
        
        for typeDeclaration in ast.compactMap({ $0 as? ASTTypeDeclaration }) {
            retval.append(generateInitializer(forType: typeDeclaration, globalFunctions: &globalFunctions, typeCache: typeCache))
            
            if !typeDeclaration.isStruct {
                retval.append(generateDeallocFunction(forType: typeDeclaration, globalFunctions: &globalFunctions))
                retval.append(contentsOf: generateGettersAndSetters(forType: typeDeclaration, globalFunctions: &globalFunctions, typeCache: typeCache))
            }
        }
        
        return retval
    }
    
    
    
    private static func generateInitializer(forType typeDeclaration: ASTTypeDeclaration, globalFunctions: inout GlobalFunctions, typeCache: TypeCache) -> ASTFunctionDeclaration {
        let typename = typeDeclaration.name.value
        let _selfType = ASTType.complex(name: typename)
        
        let deallocFunction = SymbolMangling.mangleInstanceMember(ofType: typename, memberName: "__dealloc")
        
        let initializer = ASTFunctionDeclaration(
            name: ASTIdentifier(value: "init"),
            parameters: typeDeclaration.attributes,
            returnType: _selfType,
            kind: .staticImpl(typename),
            body: [
                // declare self
                ASTVariableDeclaration(identifier: _self, type: .any), // TODO use the current type?
                
                // allocate space on the heap
                ASTAssignment(
                    target: _self,
                    value: ASTFunctionCall(
                        functionName: SymbolMangling.mangleStaticMember(ofType: "runtime", memberName: "alloc"),
                        arguments: [ASTNumberLiteral(value: typeDeclaration.attributes.count + (typeDeclaration.isStruct ? 0 : 1))],
                        unusedReturnValue: false)
                ),
                
                typeDeclaration.isStruct
                    ? ASTNoop()
                    : ASTComposite(statements: [
                        // fill the metadata field
                        // set the address of the type's dealloc function
                        // since we have to resolve this at compile time, we push the address of the function onto the stack, then use a noop as the value expression
                        
                        // push the type's dealloc address onto the stack and shift it 40 to the left
                        ASTRawWIPInstruction(instruction: .operation(.push, 40)),
                        ASTRawWIPInstruction(instruction: .unresolved(.push, deallocFunction)),
                        ASTRawWIPInstruction(instruction: .operation(.shl, 0)),
                        
                        // push the type's id onto the stack
                        ASTRawWIPInstruction(instruction: .operation(.push, typeCache.index(ofType: typename))),
                        
                        // combine dealloc address and type id
                        ASTRawWIPInstruction(instruction: .operation(.or, 0)),
                        
                        // set the type's dealloc address and type id (from the steps above) in its first field
                        ASTArraySetter(target: _self, offset: ASTNumberLiteral(value: 0), value: ASTNoop()),
                    ]
                ),
                
                // go through the parameters and fill the attributes
                ASTComposite(
                    statements: typeDeclaration.attributes.enumerated().map { arg0 -> ASTStatement in
                        let (offset, attribute) = arg0
                        return ASTArraySetter(
                            target: _self,
                            offset: ASTNumberLiteral(value: offset + (typeDeclaration.isStruct ? 0 : 1)),
                            value: typeDeclaration.isStruct || !attribute.type.isComplex
                                ? attribute.identifier
                                : ASTFunctionCall(
                                    functionName: SymbolMangling.retain,
                                    arguments: [attribute.identifier],
                                    unusedReturnValue: false
                            )
                        )
                    }
                ),
                
                // return the newly created object
                ASTReturnStatement(expression: _self)
            ]
        )
        
        
        globalFunctions[SymbolMangling.mangleInitializer(forType: typename)] = (
            argc: initializer.parameters.count,
            parameterTypes: initializer.parameters.map { $0.type },
            returnType: initializer.returnType,
            annotations: initializer.annotations
        )
        
        
        return initializer
    }
    
    
    
    private static func generateDeallocFunction(forType typeDeclaration: ASTTypeDeclaration, globalFunctions: inout GlobalFunctions) -> ASTFunctionDeclaration {
        
        let typename = typeDeclaration.name.value
        let _selfType = ASTType.complex(name: typename)
        
        // Function names
        let customDeallocFunctionName = SymbolMangling.mangleInstanceMember(ofType: typename, memberName: "dealloc")
        
        let hasCustomDeallocFunction = globalFunctions.keys.contains(customDeallocFunctionName)
        
        let deallocFunction = ASTFunctionDeclaration(
            name: "__dealloc",
            parameters: [ASTVariableDeclaration(identifier: _self, type: _selfType)],
            returnType: .void,
            kind: .impl(typename),
            annotations: [.disable_arc],
            body: [
                !hasCustomDeallocFunction
                    ? ASTNoop()
                    : ASTFunctionCall(functionName: customDeallocFunctionName, arguments: [_self], unusedReturnValue: true),
                ASTComposite(statements: typeDeclaration.attributes.enumerated().filter({ $0.element.type.isComplex }).map { arg0 in
                    return ASTFunctionCall(
                        functionName: SymbolMangling.release,
                        arguments: [ASTArrayGetter(target: _self, offset: ASTNumberLiteral(value: arg0.offset + 1))],
                        unusedReturnValue: true
                    )
                })
            ]
        )
        
        globalFunctions[deallocFunction.mangledName] = (
            argc: 1,
            parameterTypes: [_selfType],
            returnType: .void,
            annotations: [.disable_arc]
        )
        
        return deallocFunction
    }
    
    
    private static func generateGettersAndSetters(forType typeDeclaration: ASTTypeDeclaration, globalFunctions: inout GlobalFunctions, typeCache: TypeCache) -> [ASTFunctionDeclaration] {
        guard !typeDeclaration.hasAnnotation("disable_getters_setters") else {
            return []
        }
        
        var retval = [ASTFunctionDeclaration]()
        let typename = typeDeclaration.name.value
        let _selfType = ASTType.complex(name: typename)
        
        for attribute in typeDeclaration.attributes {
            let attributeName = attribute.identifier.value
            let offset = ASTNumberLiteral(value: typeCache.offset(ofMember: attributeName, inType: typename))
            
            // Generate a getter
            let getter = ASTFunctionDeclaration(
                name: ASTIdentifier(value: attributeName),
                parameters: [ASTVariableDeclaration(identifier: _self, type: _selfType)],
                returnType: attribute.type,
                kind: .impl(typename),
                annotations: [.disable_arc],
                body: [
                    ASTReturnStatement(expression: ASTArrayGetter(target: _self, offset: offset))
                ]
            )
            
            
            // Generate a setter
            let setterName: String
            if !attributeName.hasPrefix("_") {
                setterName = "set" + attributeName.capitalized
            } else {
                setterName = "_set" + attributeName.ns.substring(from: 1).capitalized // TODO surely we can implement this w/out falling back to NSString?
            }
            
            let setter = ASTFunctionDeclaration(
                name: ASTIdentifier(value: setterName),
                parameters: [.init(identifier: _self, type: _selfType), .init(identifier: "newValue", type: attribute.type)],
                returnType: .void,
                kind: .impl(typename),
                annotations: [.disable_arc],
                body: [
                    // TODO this code is pretty bad, but it was the best i could come up with that keeps the retain/release dance within the body literal
                    
                    // release the old value
                    !attribute.type.isComplex ? ASTNoop() :
                        ASTFunctionCall(
                            functionName: SymbolMangling.release,
                            arguments: [ASTArrayGetter(target: _self, offset: offset)],
                            unusedReturnValue: true
                    ),
                    
                    // store the new value
                    ASTArraySetter(target: _self, offset: offset, value: ASTIdentifier(value: "newValue")),
                    
                    // retain the new value
                    !attribute.type.isComplex ? ASTNoop() :
                        ASTFunctionCall(
                            functionName: SymbolMangling.retain,
                            arguments: [ASTIdentifier(value: "newValue")],
                            unusedReturnValue: true
                    ),
                ]
            )
            
            
            for fn in [getter, setter] {
                if !globalFunctions.keys.contains(fn.mangledName) {
                    retval.append(fn)
                    globalFunctions[fn.mangledName] = (
                        argc: fn.parameters.count,
                        parameterTypes: fn.parameters.map { $0.type },
                        returnType: fn.returnType,
                        annotations: fn.annotations
                    )
                }
            }
        }
        
        return retval
    }
    
    
}











