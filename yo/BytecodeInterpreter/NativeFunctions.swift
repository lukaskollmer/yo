//
//  NativeFunctions.swift
//  yo
//
//  Created by Lukas Kollmer on 05.07.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


protocol NativeFunctions {
    static func register(_ runtime: Runtime)
}


extension NativeFunctions {
    static func getString(atAddress _address: Int, heap: Heap) -> String {
        let address = heap[_address + 1]
        let size = heap[address]
        
        let start = address + 1
        let end = start + size
        
        let characters: [Character] = heap[start..<end]
            .compactMap(UnicodeScalar.init)
            .map(Character.init)
        
        return String(characters)
    }

}
