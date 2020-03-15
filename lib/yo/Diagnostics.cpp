//
//  Diagnostics.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-09-30.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Diagnostics.h"

using namespace yo;


void diagnostics::emit(std::string_view category, std::string_view msg) {
    std::cout << category << ": " << msg << std::endl;
}

void diagnostics::emit(std::string_view category, const lex::SourceLocation &loc, std::string_view msg) {
    if (loc.empty()) return emit(category, msg);
    
    util::fmt::print("{}:{}:{}: {}: {}", loc.filepath, loc.line, loc.column, category, msg);
    
    if (util::fs::file_exists(loc.filepath)) {
        std::cout << util::fs::read_specific_line(loc.filepath, loc.line - 1) << std::endl;
        for (uint64_t i = 0; i < loc.column - 1; i++) {
            std::cout << ' ';
        }
        std::cout << "^" << std::endl;
    }
}


void diagnostics::emitNote(std::string_view msg) {
    emit("note", msg);
}

void diagnostics::emitNote(const lex::SourceLocation &loc, std::string_view msg) {
    emit("note", loc, msg);
}


void diagnostics::emitError(std::string_view msg) {
    emit("error", msg);
    util::exitOrAbort();
}

void diagnostics::emitError(const lex::SourceLocation &loc, std::string_view msg) {
    emit("error", loc, msg);
    util::exitOrAbort();
}
