//
//  ASTUnsafeBlock.swift
//  yo
//
//  Created by Lukas Kollmer on 23.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class ASTUnsafeBlock: ASTStatement {
    let body: ASTComposite
    
    init(body: ASTComposite) {
        self.body = body
    }
}
