//
//  Diagnostics.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-09-30.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Diagnostics.h"

using namespace yo;


void diagnostics::failWithError(std::string_view msg) {
    std::cout << msg << std::endl;
    util::exitOrAbort();
}

void diagnostics::failWithError(const parser::TokenSourceLocation &loc, std::string_view msg) {
    if (loc.empty()) {
        std::cout << "Error: " << msg << std::endl;
    } else {
        std::cout << loc.filepath << ":" << loc.line << ":" << loc.column << ": " << msg << std::endl;
        std::cout << util::fs::read_specific_line(loc.filepath, loc.line - 1) << std::endl;
        
        for (auto i = 0; i < loc.column - 1; i++) {
            std::cout << ' ';
        }
        std::cout << "^" << std::endl;
    }
    
    util::exitOrAbort();
}
