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


void TypeCache::insert(std::string name, TypeInfo *type) {
    types[name] = type;
}

void TypeCache::registerStruct(std::string name, std::shared_ptr<ast::StructDecl> structDecl) {
    structs.insert({name, structDecl});
}

bool TypeCache::contains(const std::string &name) {
    return util::map::contains_key(types, name);
}

std::shared_ptr<ast::StructDecl> TypeCache::getStruct(const std::string &name) {
    try {
        return structs.at(name);
    } catch (...) {
        return nullptr;
    }
}


bool TypeCache::structHasMember(const std::string &structName, const std::string &memberName) {
    if (auto structDecl = getStruct(structName)) {
        return util::vector::contains_where(structDecl->members, [&memberName] (auto member) {
            return member->name->value == memberName;
        });
    }
    return false;
}


std::pair<int64_t, TypeInfo *> TypeCache::getMember(const std::string &structName, const std::string &memberName) {
    if (auto structDecl = getStruct(structName)) {
        int64_t memberIndex = 0;
        for (auto &member : structDecl->members) {
            if (member->name->value == memberName) {
                return {memberIndex, member->type};
            }
            memberIndex += 1;
        }
    }
    
    return {-1, TypeInfo::Unresolved};
    
}
