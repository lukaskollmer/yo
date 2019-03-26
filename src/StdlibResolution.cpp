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

static std::map<std::string, std::string_view> StdlibModules = {
    { ":std/string", std::string_view(reinterpret_cast<const char *>(stdlib_std_string_yo), stdlib_std_string_yo_len) }
};


std::string_view stdlib_resolution::GetContentsOfModuleWithName(const std::string &Name) {
    try {
        return StdlibModules.at(Name);
    } catch(...) {
        std::cout << "Error: Unable to find stdlib module '" << Name << "'\n";
        throw;
    }
}
