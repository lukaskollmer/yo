//
//  Mangling.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-01.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <string>
#include <memory>
#include "AST.h"
#include "Type.h"

namespace yo::mangling {
    std::string mangleFullyResolved(std::shared_ptr<ast::FunctionDecl>);
    std::string mangleCanonicalName(std::shared_ptr<ast::FunctionDecl>);
    std::string mangleCanonicalName(std::string_view type, std::string_view method, ast::FunctionKind kind);
    
    // TODO add a yo::irgen::Type equivalent
    //std::string mangleTemplatedComplexType(TypeInfo *TI);
    
    bool isCanonicalInstanceMethodName(std::string_view ident);
    
    // Operators
    std::string encodeOperator(ast::Operator);
    std::string mangleCanonicalName(ast::Operator);
    ast::Operator demangleCanonicalOperatorEncoding(std::string_view);
    
    
    std::string demangle(std::string_view);
}
