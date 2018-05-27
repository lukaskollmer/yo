//
//  ImportPathResolver.swift
//  yo
//
//  Created by Lukas Kollmer on 27.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


private let internalModules = ["std"]


enum ImportPathResolver {
    
    static func resolve(moduleName: String) -> String {
        let split = moduleName.split(separator: "/").map(String.init)
        
        if internalModules.contains(split[0]) {
            return yo.workingDirectory.appending(pathComponent: "stdlib/\(moduleName).yo")
        }
        
        fatalError("external modules not yet implemented")
    }
}
