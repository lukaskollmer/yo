//
//  StdlibResolution.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-22.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "StdlibResolution.h"

#include <map>
#include "stdlib_sources.h"

#include "util.h"

#define MODULE(_0, _1) { _0, std::string_view(reinterpret_cast<const char *>(stdlib_##_1##_yo), stdlib_##_1##_yo_len) }

static const std::map<std::string_view, std::string_view> stdlibModules = {
    MODULE(":runtime/core", runtime_core),
    MODULE(":runtime/operators", runtime_operators),
    MODULE(":runtime/intrinsics", runtime_intrinsics),
    MODULE(":runtime/memory", runtime_memory),
    MODULE(":runtime/refcounting", runtime_refcounting),
    MODULE(":std/math", std_math),
};
#undef MODULE


std::optional<std::string_view> yo::stdlib_resolution::getContentsOfModuleWithName(std::string_view name) {
    return util::map::get_opt(stdlibModules, name);
}
