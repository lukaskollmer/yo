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

class TypeInfo {
public:
    enum class Kind {
        Primitive,  // any of the "builtin" types
        Pointer,    // A pointer to some other type
        Complex,    // A struct
        Function,    // A function pointer
        Unresolved  // tbd
    };

    union TypeData {
        const std::string Name;     // if `Kind` is Primitive or Complex
        const TypeInfo *Pointee;    // if `Kind` is Pointer
        //const ast::FunctionSignature FnSig; // if `Kind` is Function

        explicit TypeData(std::string Name) : Name(Name) {}
        explicit TypeData(TypeInfo *Pointee) : Pointee(Pointee) {}
        ~TypeData() {}

    };

    const Kind Kind;
    const uint8_t Size; // Size in bytes
    TypeData Data;


    // Primitives
    TypeInfo(std::string Name, uint8_t Size, enum Kind Kind) : Kind(Kind), Size(Size), Data(Name) {
        precondition((Kind == Kind::Primitive || Kind == Kind::Complex)  && "This initializer should only be called with primitive types");
    }

    // Pointers
    TypeInfo(enum Kind Kind, TypeInfo *TI) : Kind(Kind), Size(8), Data(TI) {
        precondition(Kind == Kind::Pointer && "this is the pointer initializer, good sir");
    }

    static TypeInfo *MakeComplex(std::string Name) {
        return new TypeInfo(Name, 8, Kind::Complex);
    }

    static TypeInfo *MakePointer(TypeInfo *TI) {
        return new TypeInfo(Kind::Pointer, TI);
    }


    bool IsSigned() const;

    bool IsPointer() { return Kind == Kind::Pointer; }
    bool IsComplex() { return Kind == Kind::Complex; }
    bool IsPrimitive() { return Kind == Kind::Primitive; }

    bool IsIntegerType() {
        return this == i8 || this == i16 || this == i32 || this == i64 || this == u8 || this == u16 || this == u32 || this == u64;
    }

    TypeInfo *Pointee() {
        return Kind == Kind::Pointer ? const_cast<TypeInfo*>(Data.Pointee) : nullptr;
    }


    static TypeInfo *Unresolved;
    static TypeInfo *Void, *Bool, *Double;
    static TypeInfo *i8, *i16, *i32, *i64;
    static TypeInfo *u8, *u16, *u32, *u64;
    static TypeInfo *i8_ptr;

    static std::vector<TypeInfo *> PrimitiveTypes;

    // Checks whether a builtin type w/ that name exists
    // Returns a nullptr if no matching type was found
    static TypeInfo *GetBuiltinWithName(const std::string &Name);



    //static TypeInfo *GetWithName(const std::string &Name);


    bool Equals(TypeInfo *Other);

    std::string Str();


    static inline constexpr uint8_t kSizeof_i8 = 1;
    static inline constexpr uint8_t kSizeof_i16 = 2;
    static inline constexpr uint8_t kSizeof_i32 = 4;
    static inline constexpr uint8_t kSizeof_i64 = 8;

    static inline constexpr uint8_t kSizeof_u8 = 1;
    static inline constexpr uint8_t kSizeof_u16 = 2;
    static inline constexpr uint8_t kSizeof_u32 = 4;
    static inline constexpr uint8_t kSizeof_u64 = 8;
};


//bool operator==(const TypeInfo &Lhs, const TypeInfo &Rhs);




