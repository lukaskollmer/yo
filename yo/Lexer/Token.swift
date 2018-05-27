//
//  Token.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct Token {
    let type: TokenType
    let range: Range<Int>
}



enum TokenType: Equatable, Hashable {
    
    // literals
    case stringLiteral(String)
    case numberLiteral(Int) // TODO make all numbers doubles. Maybe use a public typealias?
    
    case plus           // +
    case minus          // -
    case asterik        // *
    case forwardSlash   // /
    case percentageSign // %
    
    case equalsSign     // =
    case equals         // ==
    case notEqual       // !=
    case less           // <
    case greater        // >
    case lessEqual      // <=
    case greaterEqual   // >=
    
    case doubleAmpersand
    case doublePipe
    
    // other
    case period
    case comma
    case colon
    case semicolon
    case openingParentheses
    case closingParentheses
    case openingCurlyBrackets
    case closingCurlyBrackets
    case openingSquareBrackets
    case closingSquareBrackets
    
    case identifier(String)
    
    
    // keywords
    case `import`
    case type
    case fn
    case ret
    case val
    case `if`
    case `else`
    case `while`
    
    case EOF
}

