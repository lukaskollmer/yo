//
//  extensions.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

// mostly filepath related stuff
extension String {
    var ns: NSString {
        return NSString(string: self)
    }
    
    func appending(pathComponent: String) -> String {
        // we forward this to -[NSString appendingPathComponent] bc that already takes care of things like making sure we don't have too many `/`, etc
        return ns.appendingPathComponent(pathComponent)
    }
    
    var lastPathComponent: String {
        return ns.lastPathComponent
    }
}

// CharacterSet
extension String {
    func allScalarsInCharacterSet(_ set: CharacterSet) -> Bool {
        return self.unicodeScalars.first(where: { !set.contains($0) }) == nil
    }
}


extension Array where Element: Equatable {
    func intersection(with otherArray: [Element]) -> [Element] {
        return self.filter(otherArray.contains)
    }
}
