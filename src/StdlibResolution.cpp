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


#define MODULE(name) std::string_view(reinterpret_cast<const char *>(stdlib_##name##_yo), stdlib_##name##_yo_len)

static std::map<std::string, std::string_view> StdlibModules = {
    { ":std/array",     MODULE(std_array)     },
    { ":std/string",    MODULE(std_string)    },
    { ":runtime/casts", MODULE(runtime_casts) }
};

#undef MODULE


std::string_view stdlib_resolution::GetContentsOfModuleWithName(const std::string &Name) {
    try {
        return StdlibModules.at(Name);
    } catch(...) {
        std::cout << "Error: Unable to find stdlib module '" << Name << "'\n";
        throw;
    }
}
