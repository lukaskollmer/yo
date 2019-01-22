//
//  Enum+NameInitializable.swift
//  yo
//
//  Created by Lukas Kollmer on 2019-01-22.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//


protocol NameInitializable: CaseIterable {
    init?(name: String)
    var name: String { get }
}

extension NameInitializable {
    init?(name: String) {
        guard let value = Self.allCases.first(where: { $0.name == name}) else {
            return nil
        }
        self = value
    }
    
    var name: String {
        return String(describing: self)
    }
}
