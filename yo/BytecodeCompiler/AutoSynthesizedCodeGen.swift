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
    struct CodegenOptions: OptionSet {
        let rawValue: Int
        
        static let initializer          = CodegenOptions(rawValue: 1 << 0)
        static let dealloc              = CodegenOptions(rawValue: 1 << 1)
        static let attributeAccessors   = CodegenOptions(rawValue: 1 << 2)
        // TODO implement proper protocol support?!
        static let baseProtocolConformances = CodegenOptions(rawValue: 1 << 3)
        
        static let all: CodegenOptions = [.initializer, .dealloc, .attributeAccessors, .baseProtocolConformances]
    }
    
    let compiler: BytecodeCompiler
    private var _allProtocols: [ASTProtocolDeclaration]!
    
    
    init(compiler: BytecodeCompiler) {
        self.compiler = compiler
    }
    
    func fetchProtocols(fromAST ast: AST) {
        if _allProtocols == nil {
            _allProtocols = (compiler.baseProtocols + ast.compactMap { $0 as? ASTProtocolDeclaration }).withDuplicatesRemoved()
        }
    }
    
    func synthesize(intoAST ast: inout AST, options: CodegenOptions = .all) {
        var retval = AST()
        ast
            .compactMap { $0 as? ASTTypeDeclaration }
            .filter { !$0.isStruct }
            .forEach { typeDecl in
                // Q: Why the `as AST` casts?
                // A: https://forums.swift.org/t/weird-protocol-behaviour/14963
                
                if options.contains(.initializer) {
                    retval += generateMetatypeInitializerAndCleanupFunctions(forType: typeDecl) as AST
                    retval += generateInitializer(forType: typeDecl) as AST
                }
                
                if options.contains(.dealloc) {
                    retval += [generateDeallocFunction(forType: typeDecl)]
                }
                
                if options.contains(.attributeAccessors) {
                    retval += generateAttributeAccessors(forType: typeDecl) as AST
                }
                
                if options.contains(.baseProtocolConformances) {
                    retval += generateProtocolConformance(forType: typeDecl)
                }
            }
        
        ast.insert(contentsOf: retval, at: ast.count > 2 ? ast.count - 2 : 0) // TODO find insertion position in a better way
    }
    
    
    private func generateMetatypeInitializerAndCleanupFunctions(forType type: ASTTypeDeclaration) -> [ASTFunctionDeclaration] {
        var retval = [ASTFunctionDeclaration]()
        
        let typename = type.name.value
        let g_metatype = ASTIdentifier(value: SymbolMangling.mangleMetatypeTableName(forType: typename))
        compiler.globals.append(ASTStaticVariableDeclaration(identifier: g_metatype, type: .int, initialValue: nil))
        
        let vtable = Constants.vtable.map { SymbolMangling.mangleInstanceMember(ofType: typename, memberName: $0) }
        
        let l_typename: ASTIdentifier = "l_typename"
        let l_metatype: ASTIdentifier = "l_metatype"
        let l_vtable: ASTIdentifier   = "l_vtable"
        
        let metatype_init = ASTFunctionDeclaration(
            name: ASTIdentifier(value: SymbolMangling.mangleMetatypeSetupFunction(forType: typename)),
            parameters: [],
            returnType: .void,
            kind: .global,
            annotations: [.static_initializer, .disable_arc],
            body: [
                ASTVariableDeclaration(identifier: l_typename, type: .any),
                ASTVariableDeclaration(identifier: l_metatype, type: .int),
                ASTVariableDeclaration(identifier: l_vtable,   type: .int),
                
                ASTAssignment(
                    target: l_typename,
                    value: ASTStringLiteral(value: typename)
                ),
                
                // Not sure what's going on here, but extracting the retain call into a separate statement works fine,
                // while `l_typename = retain(String::init(typename))` doesn't increment the retain count
                ASTFunctionCall(
                    functionName: SymbolMangling.retain,
                    arguments: [
                        l_typename
                    ],
                    unusedReturnValue: true
                ),
                
                ASTAssignment(
                    target: l_metatype,
                    value: ASTFunctionCall(
                        functionName: SymbolMangling.alloc,
                        arguments: [
                            ASTNumberLiteral(value: 3)
                        ],
                        unusedReturnValue: false
                    )
                ),
                
                ASTArraySetter(
                    target: l_metatype,
                    offset: 0 as ASTNumberLiteral,
                    value: ASTNumberLiteral(value: compiler.typeCache.index(ofType: typename))
                ),
                
                ASTArraySetter(
                    target: l_metatype,
                    offset: 1 as ASTNumberLiteral,
                    value: l_typename
                ),
                
                ASTAssignment(
                    target: l_vtable,
                    value: ASTFunctionCall(functionName: SymbolMangling.alloc, arguments: [ASTNumberLiteral(value: vtable.count)], unusedReturnValue: false)
                ),
                
                // fill the vtable
                ASTComposite(statements:
                    vtable.enumerated().map { offset, element -> ASTStatement in
                        return [
                            ASTArraySetter(
                                target: l_vtable,
                                offset: ASTNumberLiteral(value: offset),
                                value: ASTRawWIPInstruction(instruction: .unresolved(.push, element))
                            )
                        ] as ASTComposite
                    }
                ),
                
                ASTArraySetter(
                    target: l_metatype,
                    offset: 2 as ASTNumberLiteral,
                    value: l_vtable
                ),
                
                // assign the local metatyoe to the global variable
                ASTAssignment(target: g_metatype, value: l_metatype)
            ]
        )
        compiler.functions.insert(functionDeclaration: metatype_init)
        retval.append(metatype_init)
        
        
        // TODO what about a single function that frees all metatypes?
        let metatype_free = ASTFunctionDeclaration(
            name: ASTIdentifier(value: SymbolMangling.mangleMetatypeCleanupFunction(forType: typename)),
            parameters: [],
            returnType: .void,
            kind: .global,
            annotations: [.static_cleanup],
            body: [
                // release the typename
                ASTFunctionCall(
                    functionName: SymbolMangling.release,
                    arguments: [
                        ASTArrayGetter(target: g_metatype, offset: 1 as ASTNumberLiteral)
                    ],
                    unusedReturnValue: true
                ),
                
                
                // free the vtable
                ASTFunctionCall(
                    functionName: SymbolMangling.free,
                    arguments: [
                        ASTArrayGetter(target: g_metatype, offset: 2 as ASTNumberLiteral)
                    ],
                    unusedReturnValue: true
                ),
                
                
                // free the metatype
                ASTFunctionCall(
                    functionName: SymbolMangling.free,
                    arguments: [
                        g_metatype
                    ],
                    unusedReturnValue: true
                )
            ]
        )
        compiler.functions.insert(functionDeclaration: metatype_free)
        retval.append(metatype_free)
        
        return retval
    }
    
    
    private func generateInitializer(forType typeDeclaration: ASTTypeDeclaration) -> [ASTFunctionDeclaration] {
        var retval = [ASTFunctionDeclaration]()
        
        let typename = typeDeclaration.name.value
        let _selfType = ASTType.complex(name: typename)
        
        //
        // initializer
        //
        
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
                    :
                    // store a pointer to the metatype in the upper 32 bits
                    // the lower 32 bits contain the retain count
                    ASTArraySetter(
                        target: _self,
                        offset: 0 as ASTNumberLiteral,
                        value: {
                            // We have to push the address of the global, instead of loading the value at the address the global is pointing to
                            // This workaround is necessary because the type's metatype might still be uninitialized when an instance of the type
                            // is created (ie we create Strings which we put in metatypes, but there's no guarantee that the string metatype has already been created)
                            let metatype_name = SymbolMangling.mangleMetatypeTableName(forType: typename)
                            let metatype_address = compiler._actualAddressOfGlobal(withIdentifier: ASTIdentifier(value: metatype_name))!
                            return ASTRawWIPInstruction(
                                instruction: .operation(.push, metatype_address << 32)
                            )
                        }()
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
        
        compiler.functions.insert(functionDeclaration: initializer)
        retval.append(initializer)
        
        return retval
    }
    
    
    
    private func generateDeallocFunction(forType typeDeclaration: ASTTypeDeclaration) -> ASTFunctionDeclaration {
        
        let typename = typeDeclaration.name.value
        let _selfType = ASTType.complex(name: typename)
        
        // Function names
        let customDeallocFunctionName = SymbolMangling.mangleInstanceMember(ofType: typename, memberName: "dealloc")
        
        let hasCustomDeallocFunction = compiler.functions.keys.contains(customDeallocFunctionName)
        
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
                ASTComposite(statements: typeDeclaration.attributes.enumerated().filter({ $0.element.type.supportsReferenceCounting }).map { arg0 in
                    return ASTFunctionCall(
                        functionName: SymbolMangling.release,
                        arguments: [ASTArrayGetter(target: _self, offset: ASTNumberLiteral(value: arg0.offset + 1))],
                        unusedReturnValue: true
                    )
                })
            ]
        )
        
        compiler.functions.insert(functionDeclaration: deallocFunction)
        
        return deallocFunction
    }
    
    
    private func generateAttributeAccessors(forType typeDeclaration: ASTTypeDeclaration) -> [ASTFunctionDeclaration] {
        guard !typeDeclaration.hasAnnotation(.disable_attribute_accessors) else {
            return []
        }
        
        var retval = [ASTFunctionDeclaration]()
        let typename = typeDeclaration.name.value
        let _selfType = ASTType.complex(name: typename)
        
        for attribute in typeDeclaration.attributes {
            let attributeName = attribute.identifier.value
            let offset = ASTNumberLiteral(value: compiler.typeCache.offset(ofMember: attributeName, inType: typename))
            
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
                if !compiler.functions.keys.contains(fn.mangledName) {
                    retval.append(fn)
                    compiler.functions.insert(functionDeclaration: fn)
                }
            }
        }
        
        return retval
    }
    
    
    
    // MARK: Protocol Conformance
    
    private func generateProtocolConformance(forType typeDecl: ASTTypeDeclaration) -> AST {
        var retval = AST()
        
        let getProtocolWithName: (ASTIdentifier) -> ASTProtocolDeclaration? = { identifier in
            self._allProtocols
                .first { $0.name == identifier }
        }
        
        // every type implements all protocols declared w/ the "base_protocol" annotation
        for baseProtocol in compiler.baseProtocols where !typeDecl.protocols.contains(baseProtocol.name) {
            typeDecl.protocols.append(baseProtocol.name)
        }
        
        let typename = typeDecl.name
        
        for protocolName in typeDecl.protocols {
            guard let _protocol = getProtocolWithName(protocolName) else {
                fatalError("Unable to get protocol named '\(protocolName)'")
            }
            
            let protocolImplementation = ASTTypeImplementation(typename: typename, functions: [])
            
            for fn in _protocol.functions {
                guard !compiler.functions.keys.contains(SymbolMangling.mangleInstanceMember(ofType: typename.value, memberName: fn.name.value)) else {
                    continue
                }
                
                let implementation = ASTFunctionDeclaration(
                    name: fn.name,
                    parameters: fn.parameters.map { parameter in
                        return ASTVariableDeclaration(identifier: parameter.identifier, type: parameter.type == .Self ? .complex(name: typename.value) : parameter.type)
                    },
                    returnType: fn.returnType,
                    kind: fn.kind.withTypename(typename.value),
                    annotations: fn.annotations,
                    body: fn.body)
                
                
                protocolImplementation.functions.append(implementation)
                compiler.functions.insert(functionDeclaration: implementation)
            }
            retval.append(protocolImplementation)
        }
        
        return retval
    }
}
