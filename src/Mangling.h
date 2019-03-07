//
//  Mangling.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-01.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include <string>

namespace mangling {
    std::string MangleFunction(std::string Name);
    
    enum class MethodKind { Static, Instance };
    std::string MangleMethod(std::string Typename, std::string MethodName, MethodKind Kind);
}
