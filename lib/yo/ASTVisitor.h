//
//  ASTVisitor.h
//  yo
//
//  Created by Lukas Kollmer on 2020-03-05.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once


#include "AST.h"

#include <string>


namespace yo {
namespace ast {


// TODO implement the visitor properly!!


// TODO these don't belong in the ast NS

// returns true if sig depends on decl
bool check_sig_ty_dep(FunctionSignature &, std::shared_ptr<StructDecl>);
bool check_sig_tyname_dep(FunctionSignature &, const std::string&);


void print_ast(const ast::AST&);



} // namespace ast
} // namespace yo
