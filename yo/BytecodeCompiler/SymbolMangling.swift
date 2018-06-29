//
//  SymbolMangling.swift
//  yo
//
//  Created by Lukas Kollmer on 26.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


enum SymbolMangling {
    
    static func mangleStaticMember(ofType typename: String, memberName: String) -> String {
        return typename + "_S" + memberName
    }
    
    static func mangleInstanceMember(ofType typename: String, memberName: String) -> String {
        return typename + "_I" + memberName
    }
    
    static func mangleInitializer(forType typename: String) -> String {
        return SymbolMangling.mangleStaticMember(ofType: typename, memberName: "init")
    }
    
    static func mangleGlobalFunction(name: String) -> String {
        return name
    }
    
    static func mangleGetter(forType typename: String, attributeName: String) -> String {
        return mangleInstanceMember(ofType: typename, memberName: attributeName)
    }
    
    static func mangleSetter(forType typename: String, attributeName: String) -> String {
        return mangleInstanceMember(ofType: typename, memberName: "set" + attributeName.capitalized)
    }
    
}
