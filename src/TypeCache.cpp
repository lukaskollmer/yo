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

std::shared_ptr<ast::StructDecl> TypeCache::GetStruct(const std::string &name) {
    try {
        return Structs.at(name);
    } catch (...) {
        return nullptr;
    }
}


bool TypeCache::StructHasMember(const std::string &structName, const std::string &memberName) {
    if (auto structDecl = GetStruct(structName)) {
        return util::vector::contains_where(structDecl->Members, [&memberName] (auto member) {
            return member->Name->Value == memberName;
        });
    }
    return false;
}


std::pair<int64_t, TypeInfo *> TypeCache::GetMember(const std::string &structName, const std::string &memberName) {
    if (auto structDecl = GetStruct(structName)) {
        int64_t memberIndex = 0;
        for (auto &member : structDecl->Members) {
            if (member->Name->Value == memberName) {
                return {memberIndex, member->Type};
            }
            memberIndex += 1;
        }
    }
    
    return {-1, TypeInfo::Unresolved};
    
}
