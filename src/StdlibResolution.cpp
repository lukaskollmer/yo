//
//  StdlibResolution.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-22.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "StdlibResolution.h"

#include "util.h"
#include "stdlib_sources.h"

#include <map>
#include <dlfcn.h>


// It works, but I'm not sure if the dlopen/dlsym approach is a good idea

static std::map<std::string, std::string_view> stdlibModules;

std::optional<std::string_view> yo::stdlib_resolution::getContentsOfModuleWithName(const std::string &name) {
    if (auto module = util::map::get_opt(stdlibModules, name)) {
        return *module;
    }
    
    std::string symbolName = "stdlib_";
    symbolName.append(name.substr(1));
    symbolName = util::string::replace_all(symbolName, "/", "_");
    symbolName.append("_yo");
    
    static void *handle = dlopen(nullptr, RTLD_LAZY); // TODO is it safe to just keep this open?
    auto stringPtr = reinterpret_cast<unsigned char *>(dlsym(handle, symbolName.c_str()));
    symbolName.append("_len");
    auto stringLenPtr = reinterpret_cast<unsigned int *>(dlsym(handle, symbolName.c_str()));
    
    if (!stringPtr || !stringLenPtr) {
        return std::nullopt;
    }
    
    auto SV = std::string_view(reinterpret_cast<const char *>(stringPtr), *stringLenPtr);
    return stdlibModules[name] = SV;
}
