//
//  StdlibResolution.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-22.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "StdlibResolution.h"

#include <map>
#include <iostream>
#include "stdlib_sources.h"

#include "util.h"

#define MODULE(_0, _1) { _0, std::string_view(reinterpret_cast<const char *>(stdlib_##_1##_yo), stdlib_##_1##_yo_len) }

static std::map<std::string, std::string_view> stdlibModules = {
    MODULE(":runtime/core", runtime_core),
    MODULE(":runtime/intrinsics", runtime_intrinsics),
    MODULE(":runtime/casts", runtime_casts),
    MODULE(":runtime/memory", runtime_memory),
    MODULE(":runtime/refcounting", runtime_refcounting),
    MODULE(":std/math", std_math),
};
#undef MODULE


std::string_view yo::stdlib_resolution::getContentsOfModuleWithName(const std::string &name) {
    if (auto module = util::map::get_opt(stdlibModules, name)) {
        return *module;
    }
    LKFatalError("Unable to resolve import of stdlib module '%s'", name.c_str());
}
