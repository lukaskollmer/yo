//
//  Diagnostics.h
//  yo
//
//  Created by Lukas Kollmer on 2019-09-30.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

// TODO can you call it diagnostics if it fails after the first error and (almost) never suggests any fixits?


#include "util.h"
#include "Token.h"

#include <string>

NS_START(yo::diagnostics)

[[noreturn]]
void failWithError(std::string_view);

[[noreturn]]
void failWithError(const parser::TokenSourceLocation&, std::string_view);


[[noreturn]] __attribute__((format(printf, 1, 2)))
void failWithError(const char *fmt, ...);

[[noreturn]] __attribute__((format(printf, 2, 3)))
void failWithError(const parser::TokenSourceLocation&, const char *fmt, ...);

NS_END
