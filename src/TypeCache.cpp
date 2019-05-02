//
//  TypeCache.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-10.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "TypeCache.h"

using namespace yo;
using namespace irgen;


void TypeCache::Insert(std::string Name, TypeInfo *Type) {
    Types[Name] = Type;
}

void TypeCache::RegisterStruct(std::string Name, std::shared_ptr<ast::StructDecl> Struct) {
    Structs.insert({Name, Struct});
}

bool TypeCache::Contains(const std::string &Name) {
    return util::map::contains_key(Types, Name);
}

std::shared_ptr<ast::StructDecl> TypeCache::GetStruct(std::string Name) {
    try {
        return Structs.at(Name);
    } catch (...) {
        return nullptr;
    }
}
