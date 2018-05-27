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
        return typename + "_" + memberName
    }
    
    static func mangleInstanceMember(ofType typename: String, memberName: String) -> String {
        return typename + "__" + memberName
    }
    
}
