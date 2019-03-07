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
        Function    // A function pointer
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
        assert((Kind == Kind::Primitive || Kind == Kind::Complex)  && "This initializer should only be called with primitive types");
    }
    
    // Pointers
    TypeInfo(enum Kind Kind, TypeInfo *TI) : Kind(Kind), Size(8), Data(TI) {
        assert(Kind == Kind::Pointer && "this is the pointer initializer, good sir");
    }
    
    static TypeInfo *MakeComplex(std::string Name) {
        return new TypeInfo(Name, 8, Kind::Complex);
    }
    
    static TypeInfo *MakePointer(TypeInfo *TI) {
        return new TypeInfo(Kind::Pointer, TI);
    }
    
    
    bool IsSigned() const;
    
    const TypeInfo *Pointee() const {
        return Kind == Kind::Pointer ? Data.Pointee : nullptr;
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
    
    
    bool Equals(const TypeInfo *Other) const;
    
    std::string Str() const;
};


//bool operator==(const TypeInfo &Lhs, const TypeInfo &Rhs);




