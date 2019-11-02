//
//  TypeDesc.h
//  yo
//
//  Created by Lukas Kollmer on 2019-08-14.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"
#include "Type.h"
#include "Token.h"
//#include "AST.h"

#include <string>
#include <variant>
#include <memory>
#include <iostream>



NS_START(yo::ast)

class Expr;
class TypeDesc;


struct FunctionTypeInfo {
    yo::irgen::CallingConvention callingConvention;
    std::shared_ptr<TypeDesc> returnType;
    std::vector<std::shared_ptr<TypeDesc>> parameterTypes;
    
    FunctionTypeInfo(yo::irgen::CallingConvention cc, std::shared_ptr<TypeDesc> returnType, std::vector<std::shared_ptr<TypeDesc>> parameterTypes) : callingConvention(cc), returnType(returnType), parameterTypes(parameterTypes) {}
};



/// Describes a type, as expressed in the AST
class TypeDesc {
public:
    enum class Kind {
        Nominal,
        Pointer,
        Function,
        Reference,
        Decltype,
        
        Resolved // Special case that simply wraps a fully resolved `yo::Type *`
    };
    
private:
    Kind kind;
    std::variant<
        std::string,
        std::shared_ptr<TypeDesc>,
        FunctionTypeInfo,
        std::shared_ptr<Expr>
    > data;
    yo::irgen::Type *resolvedType = nullptr;
    parser::TokenSourceLocation srcLoc;
    
    explicit TypeDesc(Kind kind, parser::TokenSourceLocation loc = parser::TokenSourceLocation())
    : kind(kind), srcLoc(loc) {}
    
    template <typename T>
    TypeDesc(Kind kind, T data, parser::TokenSourceLocation loc = parser::TokenSourceLocation())
    : kind(kind), data(data), srcLoc(loc) {}
    
    
public:
    //TypeDesc(yo::irgen::Type *type) : kind(Kind::Resolved), resolvedType(type) {}
    
    static std::shared_ptr<TypeDesc> makeNominal(std::string name, parser::TokenSourceLocation loc = parser::TokenSourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Nominal, name, loc));
    }
    
    static std::shared_ptr<TypeDesc> makePointer(std::shared_ptr<TypeDesc> pointee, parser::TokenSourceLocation loc = parser::TokenSourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Pointer, pointee, loc));
    }
    
    static std::shared_ptr<TypeDesc> makeReference(std::shared_ptr<TypeDesc> pointee, parser::TokenSourceLocation loc = parser::TokenSourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Reference, pointee, loc));
    }
    
    static std::shared_ptr<TypeDesc> makeFunction(yo::irgen::CallingConvention cc, std::shared_ptr<TypeDesc> returnTy, std::vector<std::shared_ptr<TypeDesc>> parameterTypes, parser::TokenSourceLocation loc = parser::TokenSourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Function, FunctionTypeInfo(cc, returnTy, parameterTypes), loc));
    }
    
    static std::shared_ptr<TypeDesc> makeResolved(yo::irgen::Type *type, parser::TokenSourceLocation loc = parser::TokenSourceLocation()) {
        auto typeDesc = std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Resolved, loc));
        typeDesc->setResolvedType(type);
        return typeDesc;
    }
    
    static std::shared_ptr<TypeDesc> makeDecltype(std::shared_ptr<ast::Expr> expr, parser::TokenSourceLocation loc = parser::TokenSourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Decltype, expr, loc));
    }
    
    std::string str() const;
    
    Kind getKind() const { return kind; }
    
    bool isPointer() const { return kind == Kind::Pointer; }
    bool isResolved() const { return resolvedType != nullptr; }
    bool isReference() const { return kind == Kind::Reference; }
    
    const std::string& getName() const {
        LKAssert(kind == Kind::Nominal);
        return std::get<std::string>(data);
    }
    
    /// Returns the referenced type, if this is a pointer or a reference
    std::shared_ptr<TypeDesc> getPointee() const {
        LKAssert(kind == Kind::Pointer || kind == Kind::Reference);
        return std::get<std::shared_ptr<TypeDesc>>(data);
    }
    
    const FunctionTypeInfo& getFunctionTypeInfo() const {
        return std::get<FunctionTypeInfo>(data);
    }
    
    yo::irgen::Type* getResolvedType() const { return resolvedType; }
    void setResolvedType(yo::irgen::Type *type) { resolvedType = type; }
    
    const parser::TokenSourceLocation& getSourceLocation() const { return srcLoc; }
    
    std::shared_ptr<Expr> getDecltypeExpr() const {
        return std::get<std::shared_ptr<Expr>>(data);
    }
    
};


inline std::ostream& operator<<(std::ostream &OS, const TypeDesc &typeDesc) {
    return OS << typeDesc.str();
}

inline std::ostream& operator<<(std::ostream &OS, const std::shared_ptr<TypeDesc> &typeDesc) {
    return OS << typeDesc->str();
}



NS_END
