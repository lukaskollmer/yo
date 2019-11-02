//
//  Diagnostics.h
//  yo
//
//  Created by Lukas Kollmer on 2019-09-30.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"
#include "Token.h"

#include <string>

NS_START(yo::diagnostics)

void emitNote(std::string_view);
void emitNote(const parser::TokenSourceLocation&, std::string_view);

[[noreturn]]
void emitError(std::string_view);

[[noreturn]]
void emitError(const parser::TokenSourceLocation&, std::string_view);

NS_END
