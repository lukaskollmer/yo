//
//  SemA.h
//  yo
//
//  Created by Lukas Kollmer on 2020-03-06.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once


#include "AST.h"
#include "util.h"

NS_START(yo::sema)

/// Restructures the AST to resolve top-level dependencies between nodes
///
/// For example, a function F has a dependency on a struct S if F's signature contains a type desc which contains F.
/// In this case, we will sort the AST in a manner that S comes before F
///
/// Returns: true on success, false on failure
bool resolveTopLevelDependencies(ast::AST&);




void run(const ast::AST&);

NS_END

