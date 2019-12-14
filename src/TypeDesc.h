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
#include <utility>



NS_START(yo::ast)

using parser::TokenSourceLocation;

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
        NominalTemplated,
        Pointer,
        Function,
        Reference,
        Decltype,
        Resolved // Special case that simply wraps a fully resolved `yo::Type *`
    };
    
private:
    using NominalTemplatedDataT = std::pair<std::string, std::vector<std::shared_ptr<TypeDesc>>>;
    
    Kind kind;
    std::variant<
        std::string,                // Kind::Nominal
        NominalTemplatedDataT,      // Kind::NominalTemplated
        std::shared_ptr<TypeDesc>,  // Kind::Pointer | Kind::Reference
        FunctionTypeInfo,           // Kind::Function
        std::shared_ptr<Expr>       // Kind::Decltype
    > data;
    yo::irgen::Type *resolvedType = nullptr;
    TokenSourceLocation srcLoc;
    
    explicit TypeDesc(Kind kind, TokenSourceLocation loc = TokenSourceLocation())
    : kind(kind), srcLoc(loc) {}
    
    template <typename T>
    TypeDesc(Kind kind, T data, TokenSourceLocation loc = TokenSourceLocation())
    : kind(kind), data(data), srcLoc(loc) {}
    
    
public:
    static std::shared_ptr<TypeDesc> makeNominal(std::string name, TokenSourceLocation loc = TokenSourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Nominal, name, loc));
    }
    
    static std::shared_ptr<TypeDesc> makeNominalTemplated(std::string name, std::vector<std::shared_ptr<TypeDesc>> Ts, TokenSourceLocation loc = TokenSourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::NominalTemplated, NominalTemplatedDataT(name, Ts), loc));
    }
    
    static std::shared_ptr<TypeDesc> makePointer(std::shared_ptr<TypeDesc> pointee, TokenSourceLocation loc = TokenSourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Pointer, pointee, loc));
    }
    
    static std::shared_ptr<TypeDesc> makeReference(std::shared_ptr<TypeDesc> pointee, TokenSourceLocation loc = TokenSourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Reference, pointee, loc));
    }
    
    static std::shared_ptr<TypeDesc> makeFunction(yo::irgen::CallingConvention cc, std::shared_ptr<TypeDesc> returnTy, std::vector<std::shared_ptr<TypeDesc>> parameterTypes, TokenSourceLocation loc = TokenSourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Function, FunctionTypeInfo(cc, returnTy, parameterTypes), loc));
    }
    
    static std::shared_ptr<TypeDesc> makeResolved(yo::irgen::Type *type, TokenSourceLocation loc = TokenSourceLocation()) {
        auto typeDesc = std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Resolved, loc));
        typeDesc->setResolvedType(type);
        return typeDesc;
    }
    
    static std::shared_ptr<TypeDesc> makeDecltype(std::shared_ptr<ast::Expr> expr, TokenSourceLocation loc = TokenSourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Decltype, expr, loc));
    }
    
    std::string str() const;
    
    Kind getKind() const { return kind; }
    
    bool isOfKind(Kind K) const { return kind == K; }
    
    bool isNominal() const { return isOfKind(Kind::Nominal) || isOfKind(Kind::NominalTemplated); }
    bool isPointer() const { return isOfKind(Kind::Pointer); }
    bool isResolved() const { return isOfKind(Kind::Resolved); } // TODO should this also return true if resolvedType != nil ?
    bool isReference() const { return isOfKind(Kind::Reference); }
    
    const std::string& getName() const {
        LKAssert(isNominal());
        return isOfKind(Kind::Nominal)
            ? std::get<std::string>(data)
            : std::get<NominalTemplatedDataT>(data).first;
    }
    
    /// Returns the referenced type, if this is a pointer or a reference
    std::shared_ptr<TypeDesc> getPointee() const {
        LKAssert(kind == Kind::Pointer || kind == Kind::Reference);
        return std::get<std::shared_ptr<TypeDesc>>(data);
    }
    
    const FunctionTypeInfo& getFunctionTypeInfo() const {
        return std::get<FunctionTypeInfo>(data);
    }
    
    const NominalTemplatedDataT::second_type& getTemplateArgs() const {
        LKAssert(isOfKind(Kind::NominalTemplated));
        return std::get<NominalTemplatedDataT>(data).second;
    }
    
    yo::irgen::Type* getResolvedType() const { return resolvedType; }
    void setResolvedType(yo::irgen::Type *type) { resolvedType = type; }
    
    const TokenSourceLocation& getSourceLocation() const { return srcLoc; }
    
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
