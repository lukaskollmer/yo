//
//  TypeCache.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

enum LKError: Error {
    case other(String)
}

class TypeCache {
    private(set) var types = [ASTStructDeclaration]()
    private(set) var enums = [ASTEnumDeclaration]()
    
    private func nameIsRegistered(_ name: String) -> Bool {
        return typeExists(withName: name) || enumExists(withName: name)
    }
    
    private func guardNameStillAvailable(_ name: String) throws {
        guard !nameIsRegistered(name) else {
            throw LKError.other("typename '\(name)' already registered")
        }
    }
    
    func register(struct _struct: ASTStructDeclaration) {
        try! guardNameStillAvailable(_struct.identifier.value)
        types.append(_struct)
    }
    
    func register(enum enumDecl: ASTEnumDeclaration) {
        try! guardNameStillAvailable(enumDecl.name.value)
        enums.append(enumDecl)
    }
    
    func enumExists(withName name: String) -> Bool {
        return enumDecl(withName: name) != nil
    }
    
    func enum_hasCase(_ enumname: String, _ casename: String) -> Bool {
        return index(ofCase: casename, inEnum: enumname) != nil
    }
    
    func index(ofCase casename: String, inEnum typename: String) -> Int? {
        return enumDecl(withName: typename)?.cases.index { $0.value == casename }
    }
    
    
    func typeExists(withName name: String) -> Bool {
        return type(withName: name) != nil
    }
    
    func type(_ typename: String, hasMember member: String) -> Bool {
        return type(withName: typename)!.attributes.contains { $0.identifier.value == member } // TODO don't force unwrap
    }
    
    func index(ofType typename: String) -> Int {
        return types.index { $0.identifier.value == typename }! // TODO don't force-unwrap
    }
    
    func offset(ofMember member: String, inType typename: String) -> Int {
        guard let type = self.type(withName: typename) else {
            fatalError("type \(typename) doesn't exist")
        }
        
        let indexInAttributes = type.attributes.index { $0.identifier.value == member }!
        var fields = type.attributes[0...indexInAttributes].map { $0.type }
        
        if type.hasArcEnabled {
            fields.insert(.i64, at: 0)
        }
        
        return TypeCache.sizeof(fields) - fields.last!.size // i guess this is faster than creating a new array from a slice excluding the last element?
    }
    
    func sizeof(type typeName: String) -> Int {
        guard let type = self.type(withName: typeName) else {
            fatalError()
        }
        
        let allFields = type.attributes.map { $0.type } + (type.hasArcEnabled ? [ASTType.i64] : []) // TODO (at the end: use intptr instead)
        return TypeCache.sizeof(allFields)
    }
    
    func hasArcEnabled(_ typename: String) -> Bool {
        // returning true by default seems wrong, but otherwise there's a bunch of errors
        return self.type(withName: typename)?.hasArcEnabled ?? true
    }
    
    
    func type(ofMember member: String, ofType typename: String) -> ASTType? {
        return type(withName: typename)?.attributes.first { $0.identifier.value == member }?.type
    }
    
    
    func type(withName typename: String) -> ASTStructDeclaration? {
        return types.first { $0.identifier.value == typename }
    }
    
    private func enumDecl(withName typename: String) -> ASTEnumDeclaration? {
        return enums.first { $0.name.value == typename }
    }
    
    
    // Returns the input if we can't determine whether it's complex or an enum
    func resolveAsComplexOrEnum(_ type: ASTType) -> ASTType {
        guard case .complex(let typeName) = type, ![ASTType.any, .id].contains(type) else {
            return type
        }
        
        if !typeExists(withName: typeName) && enumExists(withName: typeName) {
            return ._enum(typeName)
        }
        return type
    }
    
    
    static func sizeof(_ fields: [ASTType]) -> Int {
        return fields.reduce(into: 0) { $0 += $1.size }
    }
    
}
