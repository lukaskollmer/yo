//
//  TypeCache.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class TypeCache {
    private(set) var types = [ASTTypeDeclaration]()
    
    func register(type: ASTTypeDeclaration) {
        types.append(type)
    }
    
    
    func typeExists(withName name: String) -> Bool {
        return type(withName: name) != nil
    }
    
    func type(_ typename: String, hasMember member: String) -> Bool {
        return type(withName: typename)!.attributes.contains { $0.identifier.value == member } // TODO don't force unwrap
    }
    
    func index(ofType typename: String) -> Int {
        return types.index { $0.name.value == typename }! // TODO don't force-unwrap
    }
    
    func offset(ofMember member: String, inType typename: String) -> Int {
        guard let type = self.type(withName: typename) else {
            fatalError("type \(typename) doesn't exist")
        }
        
        return type
            .attributes
            .index { $0.identifier.value == member }! + (type.isStruct ? 0 : 1)
    }
    
    func isStruct(_ typename: String) -> Bool {
        return self.type(withName: typename)?.isStruct ?? false
    }
    
    
    func type(ofMember member: String, ofType typename: String) -> ASTType? {
        return type(withName: typename)?.attributes.first { $0.identifier.value == member }?.type
    }
    
    
    private func type(withName typename: String) -> ASTTypeDeclaration? {
        return types.first { $0.name.value == typename }
    }
}
