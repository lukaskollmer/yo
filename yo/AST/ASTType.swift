//
//  ASTType.swift
//  yo
//
//  Created by Lukas Kollmer on 08.06.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


/// An `ASTType` describes the type of an identifier: whether it is primitive, complex or a function pointer
indirect enum ASTType: Equatable {
    case primitive(name: String)    // int/void
    case complex(name: String)      // something declared w/ the `type` keyword
    case function(returnType: ASTType, parameterTypes: [ASTType])   // a function pointer
}
