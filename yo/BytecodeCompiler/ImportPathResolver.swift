//
//  ImportPathResolver.swift
//  yo
//
//  Created by Lukas Kollmer on 27.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

enum ImportPathResolver {
    
    static let stdlibPath = CLI.value(ofFlag: "-stdlib-path", type: String.self) ?? "/Users/lukas/Developer/yo/stdlib"
    
    static func _resolve(moduleName: String, baseDirectory: String) -> String? {
        // Module resolving:
        // 1. is there a file w/ that name in the cwd?
        //    -> import that
        // 2. is there a directory w/ that name in the cwd?
        //    -> import that directory/main.yo
        // TODO update above description!
        
        let path = baseDirectory.appending(pathComponent: moduleName)
        
        let fm = FileManager.default
        
        if fm.fileExists(atPath: path + ".yo") {
            return path + ".yo"
            
        } else if fm.directoryExists(atPath: path) {
            let moduleMainPath = path.appending(pathComponent: "main.yo")
            if fm.fileExists(atPath: moduleMainPath) {
                return moduleMainPath
            }
        }
        
        
        print(moduleName, path, baseDirectory)
        return nil
    }
    
    static func resolve(moduleName: String, currentWorkingDirectory: String) -> String {
        // TODO take `CLI.value(ofFlag: "-stdlib-path", type: String.self)` into account!!!
        
        if let path = _resolve(moduleName: moduleName, baseDirectory: currentWorkingDirectory)
                ?? _resolve(moduleName: moduleName, baseDirectory: stdlibPath) {
            return path
        }
        
        fatalError("unable to resolve module '\(moduleName)'")
    }
}
