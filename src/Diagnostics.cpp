//
//  Diagnostics.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-09-30.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Diagnostics.h"

#include <cstdarg>

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


// TODO replace the va_lists with variadic templates!

void diagnostics::failWithError(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("Error: %s\n", util::fmt_cstr(fmt, args));
    va_end(args);
    
    util::exitOrAbort();
}


void diagnostics::failWithError(const parser::TokenSourceLocation &loc, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    auto msg = util::fmt_cstr(fmt, args);
    va_end(args);
    
    failWithError(loc, std::string_view(msg));
}
