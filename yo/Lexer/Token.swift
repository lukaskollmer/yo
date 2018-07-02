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
    case integerLiteral(Int)
    case doubleLiteral(Double)
    
    case plus           // +
    case minus          // -
    case asterik        // *
    case forwardSlash   // /
    case percentageSign // %
    
    case exclamationMark// !
    case equalsSign     // =
    case less           // <
    case greater        // >
    
    case ampersand      // &
    case pipe           // |
    case circumflex     // ^
    case tilde          // ~
    
    case atSign         // @
    case hashtag
    
    // other
    case period
    case comma
    case colon
    case semicolon
    case questionmark
    case openingParentheses
    case closingParentheses
    case openingCurlyBrackets
    case closingCurlyBrackets
    case openingSquareBrackets
    case closingSquareBrackets
    
    case identifier(String)
    
    
    // keywords
    case use
    case type
    case `struct`
    case impl
    case `static`
    case fn
    case ret
    case val
    case `if`
    case `else`
    case `while`
    case `for`
    case `in`
    case `break`
    case `continue`
    case __asm
    case `protocol`
    case null
    case `as`
    case `true`
    case `false`
    case `nil`
    case EOF
}

