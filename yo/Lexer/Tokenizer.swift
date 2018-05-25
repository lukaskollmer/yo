//
//  Tokenizer.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


private let ignoredChars: [Unicode.Scalar] = [" ", "\n"]
private let set = CharacterSet(charactersIn: " .+-*/;(){}\n") // TODO give this a more descriptive name

class Tokenizer {
    typealias Token = String
    
    static func tokenize(source: String) -> [Token] {
        var tokens = [Token]()
        var currentToken = ""
        
        
        let scalars = source.unicodeScalars.map { $0 }
        
        for (index, char) in scalars.enumerated() { // tbh i have no idea what i'm doing here
            if ignoredChars.contains(char) { continue }
            currentToken.unicodeScalars.append(char)
            
            let isLast = index == scalars.count - 1
            
            if !isLast {
                if set.contains(scalars[index + 1]) {
                    tokens.append(currentToken)
                    currentToken = ""
                }
            } else {
                tokens.append(currentToken)
            }
        }
        
        return tokens
    }
}
