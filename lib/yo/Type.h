//
//  Type.h
//  yo
//
//  Created by Lukas Kollmer on 2019-08-18.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "lex/SourceLocation.h"
#include "parse/TypeDesc.h"
#include "util/util.h"

#include <vector>
#include <utility>
#include <string>


namespace llvm {
    class Type;
    class DIType;
}


NS_START(yo::irgen)


class Type;
class NumericalType;
class StructType;
class PointerType;
class ReferenceType;
class FunctionType;
class StructType;
using ast::CallingConvention;



class Type {
public:
    enum class TypeID {
        Void,
        Numerical,
        Pointer,
        Reference,
        Function,
        Struct,
        Tuple
    };
    
    enum class Flags : uint8_t {
        IsTemporary = 1 << 0
    };
    
private:
    const TypeID typeId;
    llvm::Type *llvmType = nullptr;
    llvm::DIType *llvmDIType = nullptr;
    PointerType *pointerTo = nullptr;
    ReferenceType *referenceTo = nullptr;
    util::OptionSet<Flags> flags;

protected:
    explicit Type(TypeID typeId) : typeId(typeId) {}
    
public:
    virtual ~Type() = default;
    
    void setFlag(Flags F) {
        flags.insert(F);
    }
    
    bool hasFlag(Flags F) const {
        return flags.contains(F);
    }
    
    TypeID getTypeId() const { return typeId; }
    
    llvm::Type* getLLVMType() const { return llvmType; }
    void setLLVMType(llvm::Type *ty) {
        LKAssert(!llvmType && "can only set llvmType once");
        llvmType = ty;
    }
    
    llvm::DIType* getLLVMDIType() const { return llvmDIType; }
    void setLLVMDIType(llvm::DIType *ty) {
        LKAssert(!llvmDIType && "can only set llvmDIType once");
        llvmDIType = ty;
    }
    
    /// The mangled string representation of the type
    virtual std::string str_mangled() const;
    
    /// A string describing this type
    virtual std::string str_desc() const;
    
    
    bool isVoidTy() const { return typeId == TypeID::Void; }
    bool isPointerTy() const { return typeId == TypeID::Pointer; }
    bool isNumericalTy() const { return typeId == TypeID::Numerical; }
    bool isFunctionTy() const { return typeId == TypeID::Function; }
    bool isStructTy() const { return typeId == TypeID::Struct; }
    bool isReferenceTy() const { return typeId == TypeID::Reference; }
    bool isTupleTy() const { return typeId == TypeID::Tuple; }
        
    PointerType* getPointerTo();
    ReferenceType* getReferenceTo();

    
    static void initPrimitives();
    
    static Type* getVoidType();
    static NumericalType* getBoolType();
    static NumericalType* getInt8Type();
    static NumericalType* getInt16Type();
    static NumericalType* getInt32Type();
    static NumericalType* getInt64Type();
    static NumericalType* getUInt8Type();
    static NumericalType* getUInt16Type();
    static NumericalType* getUInt32Type();
    static NumericalType* getUInt64Type();
    static NumericalType* getFloat32Type(); // An IEEE 754 binary64 floating point type
    static NumericalType* getFloat64Type(); // An IEEE 754 binary64 floating point type
    
    static Type* createTemporary(const std::string& name);
    static StructType* createTemporary(const std::string& name, const std::vector<Type *> &templateArgs);

    
    static bool classof(const Type *type) {
        return type->getTypeId() == TypeID::Void; // Void is the only primitive that doesn't live in a subclass
    }
};

inline std::ostream& operator<<(std::ostream &OS, Type *ty) {
    return OS << ty->str_desc();
}


class NumericalType : public Type {
    friend class Type;
public:
    enum class NumericalTypeID {
        Int8, Int16, Int32, Int64,
        UInt8, UInt16, UInt32, UInt64,
        Float32, Float64, Bool
    };
    
private:
    const NumericalTypeID numericalTypeId;
    explicit NumericalType(NumericalTypeID typeId) : Type(Type::TypeID::Numerical), numericalTypeId(typeId) {}
    
public:
    NumericalTypeID getNumericalTypeID() const { return numericalTypeId; }
    std::string str_desc() const override;
    
    bool numericalTypeIdEquals(NumericalTypeID ID) const { return numericalTypeId == ID; }
    
    uint8_t getSize() const;
    uint8_t getPrimitiveSizeInBits() const;
    bool isBoolTy() const;
    bool isIntegerTy() const;
    bool isFloatTy() const;
    bool isSigned() const;
    bool isUnsigned() const {
        return !isSigned();
    }

    static bool classof(const Type *type) {
        return type->getTypeId() == TypeID::Numerical;
    }
    
    static NumericalType* get(NumericalTypeID);
};




class PointerType : public Type {
    friend class Type;
    
    Type *pointee;
    
    // Use `Type::getPointerTo` to create a pointer type
    explicit PointerType(Type *pointee) : Type(Type::TypeID::Pointer), pointee(pointee) {}
    
public:
    Type* getPointee() const { return pointee; }
    
    std::string str_desc() const override;
    
    static bool classof(const Type *type) {
        return type->getTypeId() == TypeID::Pointer;
    }
};




class ReferenceType : public Type {
    friend class Type;
    Type *pointee;
    
    // Use `Type::getReferenceTo` to create a reference type
    explicit ReferenceType(Type *pointee) : Type(Type::TypeID::Reference), pointee(pointee) {}
    
public:
    Type* getReferencedType() const { return pointee; }
    
    std::string str_desc() const override;
    
    static bool classof(const Type *type) {
        return type->getTypeId() == TypeID::Reference;
    }
};




class FunctionType : public Type {
    friend class Type;
    
    Type *returnType;
    std::vector<Type *> parameterTypes;
    CallingConvention callingConvention;
    
    FunctionType(Type *retTy, std::vector<Type *> paramTys, CallingConvention cc)
    : Type(Type::TypeID::Function), returnType(retTy), parameterTypes(paramTys), callingConvention(cc) {}
    
public:
    Type* getReturnType() const { return returnType; }
    uint64_t getNumberOfParameters() const { return parameterTypes.size(); }
    const std::vector<Type *>& getParameterTypes() const { return parameterTypes; }
    CallingConvention getCallingConvention() const { return callingConvention; }
    
    std::string str_desc() const override;
    
    static FunctionType* create(Type *returnType, std::vector<Type *> parameterTypes, CallingConvention cc) {
        return new FunctionType(returnType, parameterTypes, cc);
    }
    
    static bool classof(const Type *type) {
        return type->getTypeId() == TypeID::Function;
    }
};





class StructType : public Type {
    friend class Type;
public:
    using MembersT = std::vector<std::pair<std::string, Type *>>;

private:
    std::string name;
    MembersT members;
    std::vector<Type *> templateArguments;
    lex::SourceLocation sourceLoc;
    
    StructType(std::string name, MembersT members, lex::SourceLocation sourceLoc)
    : Type(Type::TypeID::Struct), name(name), members(members), sourceLoc(sourceLoc) {}
    
    StructType(std::string name, MembersT members, std::vector<Type *> templateArgs, lex::SourceLocation SL)
    : Type(Type::TypeID::Struct), name(name), members(members), templateArguments(templateArgs), sourceLoc(SL) {}
    
public:
    const std::string& getName() const {
        return name;
    }
    std::string str_mangled() const override {
        return name;
    }
    std::string str_desc() const override;
    
    bool hasMember(const std::string &name) const;
    
    uint64_t memberCount() const {
        return members.size();
    }
    
    // Returns a tuple containing the members index and type
    // Returns {0, nullptr} if the struct does not have a member with this name
    std::pair<uint64_t, Type*> getMember(const std::string &name) const;
    
    const MembersT& getMembers() const {
        return members;
    }
    const lex::SourceLocation& getSourceLocation() const {
        return sourceLoc;
    }
    
    const std::vector<Type *>& getTemplateArguments() const {
        return templateArguments;
    }
    
    bool isTemplateInstantiation() const {
        return !templateArguments.empty();
    }
    
    
    static StructType* create(std::string name, MembersT members, lex::SourceLocation sourceLoc) {
        return new StructType(name, members, sourceLoc);
    }
    
    static StructType* create(std::string name, MembersT members, std::vector<Type *> templateArgs, lex::SourceLocation SL) {
        return new StructType(name, members, templateArgs, SL);
    }
    
    static bool classof(const Type *type) {
        return type->getTypeId() == TypeID::Struct;
    }
};




class TupleType : public Type {
    std::vector<Type *> members;
    StructType *underlyingStructType = nullptr;

    TupleType(const std::vector<Type *> &M) : Type(Type::TypeID::Tuple), members(M) {}

public:
    static TupleType* get(const std::vector<Type *>&);
    
    uint64_t memberCount() const {
        return members.size();
    }
    
    const std::vector<Type *>& getMembers() const {
        return members;
    }
    
    // returns nullptr if the index is invalid
    Type* getTypeOfElementAtIndex(size_t idx) const {
        return (idx >= 0 && idx < members.size()) ? members[idx] : nullptr;
    }
    
    std::string str_desc() const override;
    
    StructType* getUnderlyingStructType() const {
        return underlyingStructType;
    }
    
    void setUnderlyingStructType(StructType *ST) {
        LKAssert(!underlyingStructType && "cannot overwrite a tuple's underlying struct type");
        underlyingStructType = ST;
    }

    static bool classof(const Type *type) {
        return type->getTypeId() == TypeID::Tuple;
    }
};

NS_END
