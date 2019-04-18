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


class TypeInfo {
public:
    enum class Kind {
        Primitive,  // any of the "builtin" types
        Pointer,    // A pointer to some other type
        Complex,    // A struct
        Function,    // A function pointer
        Unresolved  // tbd
    };
    
private:

    Kind Kind;
    uint8_t Size; // Size in bytes
    std::string Name;
    TypeInfo *Pointee;
    llvm::Type *LLVMType;
    
    TypeInfo() : Kind(Kind::Unresolved), Size(0), Pointee(nullptr), LLVMType(nullptr) {}
    
public:
    static TypeInfo *GetWithName(const std::string &Name);

    static TypeInfo *MakeComplex(std::string Name) {
        auto TI = GetWithName(Name);
        TI->Name = Name;
        TI->Size = 8;
        TI->Kind = Kind::Complex;
        return TI;
    }

    static TypeInfo *MakePointer(TypeInfo *Pointee) {
        auto TI = new TypeInfo();
        TI->Kind = Kind::Pointer;
        TI->Size = 8;
        TI->Pointee = Pointee;
        return TI;
    }

    
    
    enum Kind getKind() { return Kind; }
    uint8_t getSize() { return Size; }
    const std::string& getName() { return Name; }
    TypeInfo *getPointee() { return Kind == Kind::Pointer ? Pointee : nullptr; }
    
    llvm::Type *getLLVMType() { return LLVMType; }
    void setLLVMType(llvm::Type *LLVMType) { this->LLVMType = LLVMType; }
    
    
    std::string Str();
    bool Equals(TypeInfo *Other);

    bool IsSigned() const;
    bool IsPointer() { return Kind == Kind::Pointer; }
    bool IsComplex() { return Kind == Kind::Complex; }
    bool IsPrimitive() { return Kind == Kind::Primitive; }

    bool IsIntegerType() {
        return this == i8 || this == i16 || this == i32 || this == i64 || this == u8 || this == u16 || this == u32 || this == u64;
    }
    
    
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
