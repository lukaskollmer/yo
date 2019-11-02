//
//  Diagnostics.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-09-30.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Diagnostics.h"

using namespace yo;


void diagnostics::emitNote(std::string_view msg) {
    std::cout << msg << std::endl;
}

void diagnostics::emitNote(const parser::TokenSourceLocation &loc, std::string_view msg) {
    if (loc.empty()) return emitError(msg);
    
    util::fmt::print("{}:{}:{}: {}", loc.filepath, loc.line, loc.column, msg);
    std::cout << util::fs::read_specific_line(loc.filepath, loc.line - 1) << std::endl;
    
    for (uint64_t i = 0; i < loc.column - 1; i++) {
        std::cout << ' ';
    }
    std::cout << "^" << std::endl;
}


void diagnostics::emitError(std::string_view msg) {
    emitNote(msg);
    util::exitOrAbort();
}

void diagnostics::emitError(const parser::TokenSourceLocation &loc, std::string_view msg) {
    emitNote(loc, msg);
    util::exitOrAbort();
}
