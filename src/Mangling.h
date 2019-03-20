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
    
    
    // "temporary" mangling
    // used to encode static calls in the ast, without having to introduce additional node types
    std::string MangleStaticMethodCallNameForAST(const std::string Typename, const std::string MethodName);
    void DemangleStaticMethodCallNameForAST(const std::string Mangled, std::string *Typename, std::string *MethodName);
}
