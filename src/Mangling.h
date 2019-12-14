//
//  Mangling.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-01.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "AST.h"
#include "TypeDesc.h"
#include "Type.h"

#include <string>
#include <memory>
#include <map>

namespace yo::mangling {
    std::string mangleFullyResolved(std::shared_ptr<ast::FunctionDecl>);
    std::string mangleCanonicalName(std::shared_ptr<ast::FunctionDecl>);
    std::string mangleCanonicalName(std::string_view type, std::string_view method, ast::FunctionKind kind);
    
    // TODO add a yo::irgen::Type equivalent
    std::string mangleTemplateSpecialization(std::shared_ptr<ast::StructDecl>, std::map<std::string, irgen::Type *> templateParameterMapping);
    std::string mangleTemplateInstantiation(std::shared_ptr<ast::TypeDesc>);
    
    std::string mangleFullyResolved(std::shared_ptr<ast::StructDecl>);
    
    bool isCanonicalInstanceMethodName(std::string_view ident);
    
    // Operators
    std::string encodeOperator(ast::Operator);
    std::string mangleCanonicalName(ast::Operator);
    ast::Operator demangleCanonicalOperatorEncoding(std::string_view);
    
    
    std::string demangle(std::string_view);
}
