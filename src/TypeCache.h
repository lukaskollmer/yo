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
    std::map<std::string, std::shared_ptr<ast::StructDecl>> Structs;
    std::map<std::string, TypeInfo *> Types;
    
public:
    TypeCache() {}
    
    void Insert(std::string Name, TypeInfo *Type);
    
    void RegisterStruct(std::string Name, std::shared_ptr<ast::StructDecl> Struct);
    
    bool Contains(const std::string &Name);
    TypeInfo *Get(const std::string &Name) { return Types.at(Name); }
    std::shared_ptr<ast::StructDecl> GetStruct(std::string Name);
    
};

NS_END
