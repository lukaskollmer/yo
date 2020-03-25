//
//  TypeDesc.h
//  yo
//
//  Created by Lukas Kollmer on 2019-08-14.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "lex/SourceLocation.h"
#include "util/util.h"

#include <string>
#include <variant>
#include <memory>
#include <iostream>
#include <utility>


namespace yo::irgen {
    class Type;
}



NS_START(yo::ast)

using lex::SourceLocation;

class Expr;
class TypeDesc;


// not really used atm
enum class CallingConvention {
    C
};


struct FunctionTypeInfo {
    CallingConvention callingConvention;
    std::shared_ptr<TypeDesc> returnType;
    std::vector<std::shared_ptr<TypeDesc>> parameterTypes;
    
    FunctionTypeInfo(CallingConvention cc, std::shared_ptr<TypeDesc> returnType, std::vector<std::shared_ptr<TypeDesc>> parameterTypes)
    : callingConvention(cc), returnType(returnType), parameterTypes(parameterTypes) {}
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
        Tuple,
        Resolved // Special case that simply wraps a fully resolved `yo::Type *`
    };
    
private:
    using NominalTemplatedDataT = std::pair<std::string, std::vector<std::shared_ptr<TypeDesc>>>;
    
    Kind kind;
    std::variant<
        std::string,                            // Kind::Nominal
        NominalTemplatedDataT,                  // Kind::NominalTemplated
        std::shared_ptr<TypeDesc>,              // Kind::Pointer | Kind::Reference
        FunctionTypeInfo,                       // Kind::Function
        std::shared_ptr<Expr>,                  // Kind::Decltype
        std::vector<std::shared_ptr<TypeDesc>>  // Kind:Tuple
    > data;
    yo::irgen::Type *resolvedType = nullptr;
    SourceLocation srcLoc;
    
    explicit TypeDesc(Kind kind, SourceLocation loc = SourceLocation())
    : kind(kind), srcLoc(loc) {}
    
    template <typename T>
    TypeDesc(Kind kind, T data, SourceLocation loc = SourceLocation())
    : kind(kind), data(data), srcLoc(loc) {}
    
    
public:
    static std::shared_ptr<TypeDesc> makeNominal(std::string name, SourceLocation loc = SourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Nominal, name, loc));
    }
    
    static std::shared_ptr<TypeDesc> makeNominalTemplated(std::string name, std::vector<std::shared_ptr<TypeDesc>> Ts, SourceLocation loc = SourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::NominalTemplated, NominalTemplatedDataT(name, Ts), loc));
    }
    
    static std::shared_ptr<TypeDesc> makePointer(std::shared_ptr<TypeDesc> pointee, SourceLocation loc = SourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Pointer, pointee, loc));
    }
    
    static std::shared_ptr<TypeDesc> makeReference(std::shared_ptr<TypeDesc> pointee, SourceLocation loc = SourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Reference, pointee, loc));
    }
    
    static std::shared_ptr<TypeDesc> makeFunction(CallingConvention cc, std::shared_ptr<TypeDesc> returnTy, std::vector<std::shared_ptr<TypeDesc>> parameterTypes, SourceLocation loc = SourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Function, FunctionTypeInfo(cc, returnTy, parameterTypes), loc));
    }
    
    static std::shared_ptr<TypeDesc> makeTuple(std::vector<std::shared_ptr<TypeDesc>> M, SourceLocation SL = SourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Tuple, M, SL));
    }
    
    static std::shared_ptr<TypeDesc> makeResolved(yo::irgen::Type *type, SourceLocation loc = SourceLocation()) {
        auto typeDesc = std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Resolved, loc));
        typeDesc->setResolvedType(type);
        return typeDesc;
    }
    
    static std::shared_ptr<TypeDesc> makeDecltype(std::shared_ptr<ast::Expr> expr, SourceLocation loc = SourceLocation()) {
        return std::shared_ptr<TypeDesc>(new TypeDesc(Kind::Decltype, expr, loc));
    }
    
    
    std::string str() const;
    
    Kind getKind() const {
        return kind;
    }
    
    bool isOfKind(Kind K) const {
        return kind == K;
    }
    
    bool isNominal() const {
        return isOfKind(Kind::Nominal);
    }
    
    bool isPointer() const {
        return isOfKind(Kind::Pointer);
    }
    
    bool isResolved() const {
        return isOfKind(Kind::Resolved); // TODO should this also return true if resolvedType != nil ?
    }
    
    bool isReference() const {
        return isOfKind(Kind::Reference);
    }
    
    bool isTuple() const {
        return isOfKind(Kind::Tuple);
    }
    
    bool isNominalTemplated() const {
        return isOfKind(Kind::NominalTemplated);
    }
    
    bool isDecltype() const {
        return isOfKind(Kind::Decltype);
    }
    
    
    const std::string& getName() const {
        LKAssert(isNominal() || isNominalTemplated());
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
    
    yo::irgen::Type* getResolvedType() const {
        return resolvedType;
    }
    void setResolvedType(yo::irgen::Type *type) {
        resolvedType = type;
    }
    
    const SourceLocation& getSourceLocation() const {
        return srcLoc;
    }
    void setSourceLocation(SourceLocation SL) {
        srcLoc = SL;
    }
    
    std::shared_ptr<Expr> getDecltypeExpr() const {
        return std::get<std::shared_ptr<Expr>>(data);
    }
    
    const std::vector<std::shared_ptr<TypeDesc>>& getTupleMembers() const {
        return std::get<std::vector<std::shared_ptr<TypeDesc>>>(data);
    }
    
};


inline std::ostream& operator<<(std::ostream &OS, const TypeDesc &typeDesc) {
    return OS << typeDesc.str();
}

inline std::ostream& operator<<(std::ostream &OS, const std::shared_ptr<TypeDesc> &typeDesc) {
    return OS << typeDesc->str();
}



NS_END
