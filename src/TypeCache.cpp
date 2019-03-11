//
//  TypeCache.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-10.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "TypeCache.h"

using namespace irgen;

void TypeCache::Insert(std::string Name, std::shared_ptr<ast::StructDecl> Struct) {
    Types.insert({Name, Struct});
}

bool TypeCache::Contains(std::string Name) {
    return Get(Name) != nullptr;
}

ast::StructDecl* TypeCache::Get(std::string Name) {
    try {
        return Types.at(Name).get();
    } catch (...) {
        return nullptr;
    }
}
