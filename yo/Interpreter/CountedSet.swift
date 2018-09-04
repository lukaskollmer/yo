//
//  CountedSet.swift
//  yo
//
//  Created by Lukas Kollmer on 25.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct CountedSet<T: Hashable> {
    private(set) var backing = [T: Int]()
    
    mutating func insert(_ element: T) {
        let count = self.count(of: element)
        
        if count == 0 {
            backing.updateValue(1, forKey: element)
        } else {
            backing.updateValue(count + 1, forKey: element)
        }
    }
    
    
    mutating func remove(_ element: T) {
        let count = self.count(of: element)
        
        if count == 1 {
            backing.removeValue(forKey: element)
        } else {
            backing.updateValue(count - 1, forKey: element)
        }
    }
    
    
    func contains(_ element: T) -> Bool {
        return backing.keys.contains(element)
    }
    
    
    func count(of element: T) -> Int {
        return backing[element] ?? 0
    }
}
