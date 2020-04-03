//
//  NameLookup.h
//  yo
//
//  Created by Lukas Kollmer on 2020-03-26.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "parse/AST.h"

#include <vector>
#include <memory>
#include <ostream>
#include <variant>
#include <map>
#include <tuple>
#include <utility>

namespace yo {
namespace irgen {

class Type;
class IRGenerator;


/// Info about a (named) value
// TOOD rewrite, this is atrocious
struct ValueInfo {
    using TypeTmplInfo = std::shared_ptr<ast::TopLevelStmt>;
//    using PropertyInfo = std::tuple<Type *, Type *, std::string>;
    using FunctionInfo = std::pair<Type *, std::shared_ptr<ast::FunctionDecl>>;
    struct PropertyInfo {
        Type *type;       // type of the property
        Type *parentType; // the type this property is a member of // TODO come up w/ a better name
        std::string name;
    };
    
    enum class Kind {
        TypeRef,
        TypeRefTmpl,
        LocalVar,
        Property,
        Function,
    };
    
    Kind kind;
    std::variant<Type *, TypeTmplInfo, PropertyInfo, FunctionInfo> data;
    bool isStaticMember;

    template <typename T>
    ValueInfo(Kind K, const T &D, bool ISM) : kind(K), data(D), isStaticMember(ISM) {}
    
    template <typename T>
    const T& getData() const {
        return std::get<T>(data);
    }
    
    Type* getTypeRef() const {
        return std::get<Type *>(data);
    }
    
    const TypeTmplInfo& getTypeRefTmplDecl() const {
        return std::get<TypeTmplInfo>(data);
    }
    
    const PropertyInfo& getPropertyInfo() const {
        return std::get<PropertyInfo>(data);
    }
    
    const FunctionInfo& getFunctionInfo() const {
        return std::get<FunctionInfo>(data);
    }
    
    
    static ValueInfo typeRef(Type *type) {
        return ValueInfo(Kind::TypeRef, type, false);
    }
    
    static ValueInfo typeRefTmpl(std::shared_ptr<ast::TopLevelStmt> decl) {
        return ValueInfo(Kind::TypeRefTmpl, decl, false);
    }
    
    static ValueInfo localVar(Type *type) {
        return ValueInfo(Kind::LocalVar, type, false);
    }
    
    static ValueInfo property(Type *parentType, Type *type, const std::string &name, bool isStatic = false) {
        return ValueInfo(Kind::Property, PropertyInfo{parentType, type, name}, isStatic);
    }
    
    static ValueInfo function(Type *selfType, std::shared_ptr<ast::FunctionDecl> func, bool isStatic = false) {
        return ValueInfo(Kind::Function, std::make_pair(selfType, func), isStatic);
    }
};

std::ostream& operator<<(std::ostream&, const ValueInfo&);



struct TypeMembersTable {
    Type *type;
    std::map<std::string, std::vector<ValueInfo>> members;
    
    explicit TypeMembersTable(Type *ty) : type(ty) {}
    
    void addProperty(Type *parentTy, const std::string &name, Type *type, bool isStatic = false) {
        members[name].push_back(ValueInfo::property(parentTy, type, name, isStatic));
    }
    
    void addMemberFunction(Type *selfType, const std::shared_ptr<ast::FunctionDecl> &funcDecl, bool isStatic = false) {
        members[funcDecl->getName()].push_back(ValueInfo::function(selfType, funcDecl, isStatic));
    }
    
    bool contains(const std::string &name) const {
        return members.find(name) != members.end();
    }
    
    void dump() const;
};




// main reason this is a class is to simplify IRGenerator befriending the name lookup impl(s)
class NameLookup {
    IRGenerator &irgen;
    
public:
    explicit NameLookup(IRGenerator &irgen) : irgen(irgen) {}
    
    std::vector<ValueInfo> lookup(const std::shared_ptr<ast::Expr>&);
    
private:
    TypeMembersTable computeMemberTableForType(Type *type);
    bool isAcceptableFirstParam(Type *, const std::shared_ptr<ast::FunctionDecl>&);
};


}
}
