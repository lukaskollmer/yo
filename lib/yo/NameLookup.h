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
#include <utility>

namespace yo {
namespace irgen {

class Type;
class IRGenerator;


/// Info about a (named) value
// TOOD this is atrocious
struct ValueInfo {
    using PropertyInfo = std::pair<Type *, Type *>;
    using FunctionInfo = std::pair<Type *, std::shared_ptr<ast::FunctionDecl>>;
    
    enum class Kind {
        TypeRef,
        LocalVar,
        Property,
        Function,
    };
    
    Kind kind;
    std::variant<Type *, PropertyInfo, FunctionInfo> data;
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
    
    const PropertyInfo& getPropertyInfo() const {
        return std::get<PropertyInfo>(data);
    }
    
    const FunctionInfo& getFunctionInfo() const {
        return std::get<FunctionInfo>(data);
    }
    
    
    static ValueInfo typeRef(Type *type) {
        return ValueInfo(Kind::TypeRef, type, false);
    }
    
    static ValueInfo localVar(Type *type) {
        return ValueInfo(Kind::LocalVar, type, false);
    }
    
    static ValueInfo property(Type *parentType, Type *type) {
        return ValueInfo(Kind::Property, std::make_pair(parentType, type), false);
    }
    
    static ValueInfo function(Type *selfType, std::shared_ptr<ast::FunctionDecl> func) {
        return ValueInfo(Kind::Function, std::make_pair(selfType, func), false);
    }
};

std::ostream& operator<<(std::ostream&, const ValueInfo&);




//struct ValueInfo {
//    enum class Kind {
//        Function,
//        Property,
//        LocalVar
//    };
//
//    const Kind kind;
//    explicit ValueInfo(Kind k) : kind(k) {}
//    virtual ~ValueInfo() = default;
//};
//
//
//struct FunctionValueInfo : public ValueInfo {
//    ResolvedCallable RC;
//
//    // if this is a member function (static or instance), the type it is a member of
//    Type *type = nullptr;
//
//    FunctionValueInfo(ResolvedCallable RC, Type *type) : ValueInfo(Kind::Function), RC(RC), type(type) {}
//};
//
//
//struct Property




struct TypeMembersTable {
    Type *type;
    std::map<std::string, std::vector<ValueInfo>> members;
    
    explicit TypeMembersTable(Type *ty) : type(ty) {}
    
    void addProperty(Type *parentTy, const std::string &name, Type *type) {
        members[name].push_back(ValueInfo::property(parentTy, type));
    }
    
    void addMemberFunction(Type *selfType, const std::shared_ptr<ast::FunctionDecl> &funcDecl) {
        members[funcDecl->getName()].push_back(ValueInfo::function(selfType, funcDecl));
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
