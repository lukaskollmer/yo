//
//  Mangling.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-01.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include <string>
#include <memory>
#include "AST.h"

namespace mangling {
    std::string MangleFullyResolvedNameForSignature(std::shared_ptr<ast::FunctionSignature>);
    std::string MangleCanonicalNameForSignature(std::shared_ptr<ast::FunctionSignature>);
    std::string MangleCanonicalName(std::string_view Type, std::string_view Method, ast::FunctionSignature::FunctionKind Kind);
}
