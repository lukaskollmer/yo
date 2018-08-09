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

extension Int {
    var unsafe_loadAsDouble: Double {
        var value = self
        return cast(&value)
    }
}

extension Double {
    var unsafe_loadAsInt: Int {
        var value = self
        return cast(&value)
    }
}



extension Int {
    var isEven: Bool {
        return self % 2 == 0
    }
    
    var isOdd: Bool {
        return !isEven
    }
}


extension Sequence {
    func all(_ block: (Element) throws -> Bool) rethrows -> Bool {
        for element in self {
            if try !block(element) {
                return false
            }
        }
        return true
    }
    
    
    func any(_ block:(Element) throws -> Bool) rethrows -> Bool {
        return try self.first(where: block) != nil
    }
    
    func firstIndex(where block: (Element) -> Bool) -> Int? {
        return self.lazy.enumerated().first(where: { block($0.element) })?.offset
    }
    
    func lastIndex(where block: (Element) -> Bool) -> Int? {
        return self.lazy.enumerated().reversed().first(where: { block($0.element) })?.offset
    }
    
    func firstIndex(after index: Int, where block: (Element) -> Bool) -> Int? {
        return self.lazy.enumerated().first(where: { $0.offset > index && block($0.1) })?.offset
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
    @discardableResult mutating func remove(where block: (Element) -> Bool) -> [Element] {
        let initialSize = self.count
        var retval = [Element]()
        
        for (idx, elememt) in self.reversed().enumerated() {
            if block(elememt) {
                retval.append(self.remove(at: initialSize - idx - 1))
            }
        }
        return retval
    }
    
    func isValidIndex(_ index: Index) -> Bool {
        return index >= self.startIndex && index < self.endIndex
    }
    
    subscript(safe index: Index) -> Element? {
        get {
            var index = index
            if index < 0 {
                index = self.count + index
            }
            guard self.isValidIndex(index) else {
                return nil
            }
            return self[index]
        }
    }
    
    var excludingFirstAndLast: [Element] {
        return Array(self.dropFirst().dropLast())
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
    
    var directory: String {
        return self.ns.deletingLastPathComponent
    }
}

// CharacterSet
extension String {
    func allScalarsInCharacterSet(_ set: CharacterSet) -> Bool {
        return self.unicodeScalars.all { set.contains($0) }
    }
    
    func anyScalarInCharacterSet(_ set: CharacterSet) -> Bool {
        return self.unicodeScalars.any { set.contains($0) }
    }
}


extension Array where Element: Equatable {
    func intersection(with otherArray: [Element]) -> [Element] {
        return self.filter(otherArray.contains)
    }
}

extension Array where Element: Hashable {
    func withDuplicatesRemoved() -> [Element] {
        var retval = [Element]()
        for element in self where !retval.contains(element) {
            retval.append(element)
        }
        
        return retval
    }
    
    mutating func removeDuplicates() {
        self = self.withDuplicatesRemoved()
    }
}


extension Dictionary {
    mutating func insert(contentsOf other: Dictionary<Key, Value>) {
        for (key, value) in other {
            self[key] = value
        }
    }
    
    
    func inserting(contentsOf other: Dictionary<Key, Value>) -> Dictionary<Key, Value> {
        var retval = self
        retval.insert(contentsOf: other)
        
        return retval
    }
}


extension FileManager {
    func directoryExists(atPath path: String) -> Bool {
        var isDirectory: ObjCBool = false
        return self.fileExists(atPath: path, isDirectory: &isDirectory) && isDirectory.boolValue
    }
}
