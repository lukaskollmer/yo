//
//  ASTTypeMemberSetter.swift
//  yo
//
//  Created by Lukas Kollmer on 27.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct _ASTTypeMemberSetter: ASTStatement {
    let target: ASTIdentifier
    let memberName: ASTIdentifier
    let newValue: ASTExpression
}
