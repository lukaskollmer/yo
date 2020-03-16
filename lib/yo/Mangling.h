//
//  Mangling.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-01.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "parse/AST.h"
#include "parse/TypeDesc.h"
#include "Type.h"

#include <string>
#include <memory>

namespace yo::mangling {
    std::string mangleCanonicalName(std::shared_ptr<ast::FunctionDecl>);
    std::string mangleCanonicalName(std::string_view type, std::string_view method, ast::FunctionKind kind);
    
    std::string mangleFullyResolved(std::shared_ptr<ast::FunctionDecl>);
    std::string mangleFullyResolved(std::shared_ptr<ast::StructDecl>);
    
    std::string mangleAsStruct(std::string_view);
    std::string mangleFullyResolved(const irgen::Type *);
    
    bool isCanonicalInstanceMethodName(std::string_view ident);
    
    // Operators
    std::string encodeOperator(ast::Operator);
    std::string mangleCanonicalName(ast::Operator);
    ast::Operator demangleCanonicalOperatorEncoding(std::string_view);
    
    
    std::string demangle(std::string_view);
    std::string demangleCanonicalName(std::string_view);
}
