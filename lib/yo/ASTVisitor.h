//
//  ASTVisitor.h
//  yo
//
//  Created by Lukas Kollmer on 2020-03-05.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once


#include "AST.h"




namespace yo {
namespace ast {


// TODO implement this properly

// returns true if sig depends on decl
bool check_sig_ty_dep(FunctionSignature &, std::shared_ptr<StructDecl>);



} // namespace ast
} // namespace yo
