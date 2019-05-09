//
//  TypeInfo.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-27.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <string>
#include <vector>
#include <functional>
#include "util.h"


namespace llvm {
    class Type;
}


NS_START(yo)

class TypeInfo {
public:
    enum class Kind {
        Primitive,  // any of the "builtin" types
        Pointer,    // A pointer to some other type
        Complex,    // A struct
        Function,   // A function pointer
        Typealias,
        Unresolved  // tbd
    };
    
    
    struct FunctionTypeInfo {
        enum class CallingConvention : uint8_t {
            C, Yo
        };
        
        CallingConvention callingConvention;
        std::vector<TypeInfo *> parameterTypes;
        TypeInfo *returnType;
    };
    
private:
    
    Kind kind;
    uint8_t size; // Size in bytes
    
    std::string name;
    TypeInfo *pointee; // The fact that pointee is nonnull doesn't mean that this is in fact a pointer type (also used for typealiases)
    TypeInfo *pointerTo;
    llvm::Type *llvmType;
    
    // only for function types
    std::unique_ptr<FunctionTypeInfo> functionTypeInfo;
    
    TypeInfo() : kind(Kind::Unresolved), size(0), pointee(nullptr), pointerTo(nullptr), llvmType(nullptr) {}
    
public:
    static TypeInfo *GetWithName(const std::string &name, bool *didCreateNewType = nullptr);
    static TypeInfo *MakeComplex(const std::string &name);
    static TypeInfo *MakePointer(TypeInfo *pointee);
    static TypeInfo *MakeTypealias(const std::string &name, TypeInfo *otherType);
    static TypeInfo *MakeFunctionType(FunctionTypeInfo::CallingConvention callingConvention, std::vector<TypeInfo *> parameterTypes, TypeInfo *returnType);
    
    Kind getKind() { return kind; }
    uint8_t getSize() {
        if (isTypealias()) return pointee->getSize();
        else return size;
    }
    const std::string &getName() { return name; }
    TypeInfo *getPointee() { return pointee; }
    TypeInfo *getPointerTo() { return MakePointer(this); }
    
    FunctionTypeInfo& getFunctionTypeInfo() const { return *functionTypeInfo; }
    
    
    llvm::Type *getLLVMType() {
        if (isTypealias()) return pointee->getLLVMType();
        else return llvmType;
    }
    
    void setLLVMType(llvm::Type *llvmType) {
        if (isTypealias()) pointee->setLLVMType(llvmType);
        else this->llvmType = llvmType;
    }
    
    
    std::string str() const;
    bool equals(TypeInfo *other);
    
    unsigned getIndirectionCount();

    bool isSigned();
    bool isPointer() { return kind == Kind::Pointer; }
    bool isComplex() { return kind == Kind::Complex; }
    bool isPrimitive() { return kind == Kind::Primitive; }
    bool isTypealias() { return kind == Kind::Typealias; }
    bool isFunction() const { return kind == Kind::Function; }

    bool isIntegerType();
    bool isVoidType();
    
    
    static TypeInfo *getType_void();
    static TypeInfo *getType_bool();
    static TypeInfo *getType_double();
    
    static TypeInfo *getType_i8();
    static TypeInfo *getType_i16();
    static TypeInfo *getType_i32();
    static TypeInfo *getType_i64();
    
    static TypeInfo *getType_u8();
    static TypeInfo *getType_u16();
    static TypeInfo *getType_u32();
    static TypeInfo *getType_u64();
    
    
    inline static TypeInfo *Unresolved = nullptr;
    
    inline static const auto i8  = TypeInfo::getType_i8();
    inline static const auto i16 = TypeInfo::getType_i16();
    inline static const auto i32 = TypeInfo::getType_i32();
    inline static const auto i64 = TypeInfo::getType_i64();
    
    inline static const auto u8  = TypeInfo::getType_u8();
    inline static const auto u16 = TypeInfo::getType_u16();
    inline static const auto u32 = TypeInfo::getType_u32();
    inline static const auto u64 = TypeInfo::getType_u64();
    
    inline static const auto Void = TypeInfo::getType_void();
    inline static const auto Bool = TypeInfo::getType_bool();
    inline static const auto Double = TypeInfo::getType_double();
    inline static const auto i8_ptr = TypeInfo::MakePointer(TypeInfo::i8);
    


    static inline constexpr uint8_t kSizeof_i8 = 1;
    static inline constexpr uint8_t kSizeof_i16 = 2;
    static inline constexpr uint8_t kSizeof_i32 = 4;
    static inline constexpr uint8_t kSizeof_i64 = 8;

    static inline constexpr uint8_t kSizeof_u8 = 1;
    static inline constexpr uint8_t kSizeof_u16 = 2;
    static inline constexpr uint8_t kSizeof_u32 = 4;
    static inline constexpr uint8_t kSizeof_u64 = 8;
};

inline std::ostream &operator<<(std::ostream &OS, const TypeInfo *TI) {
    return OS << TI->str();
}

NS_END
