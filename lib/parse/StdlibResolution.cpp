//
//  StdlibResolution.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-22.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "StdlibResolution.h"

#include "util/MapUtils.h"
#include "stdlib_sources.cpp"


std::optional<std::string_view> yo::stdlib_resolution::getContentsOfModuleWithName(std::string_view name) {
    return util::map::get_opt(stdlibModules, name);
}
