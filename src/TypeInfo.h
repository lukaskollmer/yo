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
        Typealias,
        Unresolved  // tbd
    };
    
private:
    Kind Kind_;
    uint8_t Size_; // Size in bytes
    std::string Name_;
    TypeInfo *Pointee_; // The fact that Pointee_ is nonnull doesn't mean that this is in fact a pointer type
    TypeInfo *PointerTo_;
    llvm::Type *LLVMType_;
    
    TypeInfo() : Kind_(Kind::Unresolved), Size_(0), Pointee_(nullptr), PointerTo_(nullptr), LLVMType_(nullptr) {}
    
public:
    static TypeInfo *GetWithName(const std::string &Name, bool *DidCreateNewType = nullptr);
    static TypeInfo *MakeComplex(const std::string &Name);
    static TypeInfo *MakePointer(TypeInfo *Pointee);
    static TypeInfo *MakeTypealias(const std::string &Name, TypeInfo *OtherType);
    
    Kind getKind() { return Kind_; }
    uint8_t getSize() {
        if (isTypealias()) return Pointee_->getSize();
        else return Size_;
    }
    const std::string &getName() { return Name_; }
    TypeInfo *getPointee() { return Pointee_; }
    TypeInfo *getPointerTo() { return MakePointer(this); }
    
    
    
    llvm::Type *getLLVMType() {
        if (isTypealias()) return Pointee_->getLLVMType();
        else return LLVMType_;
    }
    
    void setLLVMType(llvm::Type *LLVMType) {
        if (isTypealias()) Pointee_->setLLVMType(LLVMType);
        else LLVMType_ = LLVMType;
    }
    
    
    std::string str() const;
    bool equals(TypeInfo *Other);
    
    unsigned getIndirectionCount();

    bool isSigned();
    bool isPointer() { return Kind_ == Kind::Pointer; }
    bool isComplex() { return Kind_ == Kind::Complex; }
    bool isPrimitive() { return Kind_ == Kind::Primitive; }
    bool isTypealias() { return Kind_ == Kind::Typealias; }

    bool isIntegerType();
    
    
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
