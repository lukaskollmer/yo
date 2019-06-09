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

static std::map<std::string, std::string_view> StdlibModules = {
    MODULE(":std/array", std_array),
    MODULE(":std/string", std_string),
    MODULE(":runtime/casts", runtime_casts),
    MODULE(":runtime/memory", runtime_memory),
    MODULE(":runtime/refcounting", runtime_refcounting),
};
#undef MODULE


std::string_view yo::stdlib_resolution::GetContentsOfModuleWithName(const std::string &Name) {
    if (auto Module = util::map::get_opt(StdlibModules, Name)) {
        return *Module;
    }
    LKFatalError("Unable to resolve import of stdlib module '%s'", Name.c_str());
}
