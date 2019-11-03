//
//  StdlibResolution.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-22.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <string>
#include <optional>

namespace yo::stdlib_resolution {
    std::optional<std::string_view> getContentsOfModuleWithName(const std::string&);
}
