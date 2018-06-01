//
//  extensions.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

func cast<T, R>(_ arg0: inout T) -> R {
    return withUnsafeBytes(of: &arg0) { $0.load(as: R.self) }
}



extension Sequence {
    func all(_ block: (Element) -> Bool) -> Bool {
        for element in self {
            if !block(element) {
                return false
            }
        }
        return true
    }
    
    
    func any(_ block:(Element) -> Bool) -> Bool {
        return self.first(where: block) != nil
    }
}


extension Array {
    
    // TODO move this to Sequence?
    func lk_flatMap<T>(_ block: (Element) throws -> [T]) rethrows -> [T] {
        var retval = [T]()
        try self.forEach { retval.append(contentsOf: try block($0)) }
        return retval
    }
    
    //func lk_flatMap<T>(_ block: (Element) throws -> [T]) rethrows ->
    
    // remove all elements matching a predicate and return the removed elements
    mutating func remove(where block: (Element) -> Bool) -> [Element] {
        let initialSize = self.count
        var retval = [Element]()
        
        for (idx, elememt) in self.reversed().enumerated() {
            if block(elememt) {
                retval.append(self.remove(at: initialSize - idx - 1))
            }
        }
        return retval
    }
}




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
        return self.unicodeScalars.all { set.contains($0) }
        //return self.unicodeScalars.first(where: { !set.contains($0) }) == nil
    }
}


extension Array where Element: Equatable {
    func intersection(with otherArray: [Element]) -> [Element] {
        return self.filter(otherArray.contains)
    }
}


extension Dictionary {
    mutating func insert(contentsOf other: Dictionary<Key, Value>) {
        for (key, value) in other {
            self[key] = value
        }
    }
}
