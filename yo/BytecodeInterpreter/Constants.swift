//
//  Constants.swift
//  yo
//
//  Created by Lukas Kollmer on 02.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


enum Constants {
    
    enum NumberTypeMapping {
        static let integer = 0
        static let boolean = 1
        static let double  = 2
    }
    
    
    enum BooleanValues {
        static let `false` = 0
        static let `true`  = 1
    }
    
    // NOTE keep this in sync w/ the attributes of the `vtable` struct in the standard library
    static let vtable = [
        "__dealloc",
        "description",
        "isEqualTo",
        "hashValue"
    ]
    
}
