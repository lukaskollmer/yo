//
//  Mangling.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-01.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Mangling.h"
#include "util.h"


std::string mangling::MangleFunction(std::string Name) {
    std::string Mangled;
    Mangled.append("_F");
    Mangled.append(std::to_string(Name.length()));
    Mangled.append(Name);
    return Mangled;
}


std::string mangling::MangleMethod(std::string Typename, std::string MethodName, MethodKind Kind) {
    std::string Mangled;
    Mangled += "_";
    Mangled += Kind == MethodKind::Static ? "S" : "I";
    Mangled += std::to_string(Typename.length());
    Mangled += Typename;
    Mangled += std::to_string(MethodName.length());
    Mangled += MethodName;
    return Mangled;
}



std::string mangling::MangleStaticMethodCallNameForAST(const std::string Typename, const std::string MethodName) {
    return std::string(Typename).append("~").append(MethodName);
}

std::pair<std::string, std::string> mangling::DemangleStaticMethodCallNameForAST(const std::string Mangled) {
    auto Pos = Mangled.find('~');
    precondition(Pos != Mangled.npos);
    return {
        Mangled.substr(0, Pos),
        Mangled.substr(Pos + 1)
    };
}

