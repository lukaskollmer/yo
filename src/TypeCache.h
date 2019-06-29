//
//  TypeCache.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-10.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <string>
#include <map>
#include <memory>

#include "AST.h"
#include "util.h"


NS_START(yo::irgen)

class TypeCache {
    // TODO what about enums?
    std::map<std::string, std::shared_ptr<ast::StructDecl>> structs;
    std::map<std::string, TypeInfo *> types;
    
public:
    TypeCache() {}
    
    void insert(std::string name, TypeInfo *type);
    
    void registerStruct(std::string name, std::shared_ptr<ast::StructDecl> structDecl);
    
    bool contains(const std::string &name);
    TypeInfo *get(const std::string &name) { return types.at(name); }
    std::shared_ptr<ast::StructDecl> getStruct(const std::string &name);
    
    bool structHasMember(const std::string &structName, const std::string &memberName);
    
    // Returns (memberIndex, memberType)
    std::pair<int64_t, TypeInfo *> getMember(const std::string &structName, const std::string &memberName);
    
};

NS_END
