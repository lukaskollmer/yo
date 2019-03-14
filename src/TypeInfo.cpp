//
//  TypeInfo.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-27.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include <iostream>

#include "TypeInfo.h"


TypeInfo *TypeInfo::Unresolved = TypeInfo::Unresolved;
TypeInfo *TypeInfo::Void = new TypeInfo("void", 0, TypeInfo::Kind::Primitive);

TypeInfo *TypeInfo::i8  = new TypeInfo("i8" , 1, TypeInfo::Kind::Primitive);
TypeInfo *TypeInfo::i16 = new TypeInfo("i16", 2, TypeInfo::Kind::Primitive);
TypeInfo *TypeInfo::i32 = new TypeInfo("i32", 4, TypeInfo::Kind::Primitive);
TypeInfo *TypeInfo::i64 = new TypeInfo("i64", 8, TypeInfo::Kind::Primitive);

TypeInfo *TypeInfo::u8  = new TypeInfo("u8" , 1, TypeInfo::Kind::Primitive);
TypeInfo *TypeInfo::u16 = new TypeInfo("u16", 2, TypeInfo::Kind::Primitive);
TypeInfo *TypeInfo::u32 = new TypeInfo("u32", 4, TypeInfo::Kind::Primitive);
TypeInfo *TypeInfo::u64 = new TypeInfo("u64", 8, TypeInfo::Kind::Primitive);

TypeInfo *TypeInfo::Bool   = new TypeInfo("bool",   1, TypeInfo::Kind::Primitive);
TypeInfo *TypeInfo::Double = new TypeInfo("double", 8, TypeInfo::Kind::Primitive);

TypeInfo *TypeInfo::i8_ptr = TypeInfo::MakePointer(TypeInfo::i8);


std::vector<TypeInfo *> TypeInfo::PrimitiveTypes = {
    TypeInfo::i8, TypeInfo::i16, TypeInfo::i32, TypeInfo::i64,
    TypeInfo::u8, TypeInfo::u16, TypeInfo::u32, TypeInfo::u64,
    TypeInfo::Void, TypeInfo::Bool, TypeInfo::Double
};


TypeInfo *TypeInfo::GetBuiltinWithName(const std::string &Name) {
    for (auto T : PrimitiveTypes) {
        if (T->Data.Name == Name) {
            return T;
        }
    }
    
    return nullptr;
}


bool TypeInfo::IsSigned() const {
    return this == i8 || this == i16 || this == i32 || this == i64;
}


bool TypeInfo::Equals(TypeInfo *Other) {
    if (this == Other) return true;
    if (this->Kind != Other->Kind || this->Size != Other->Size) return false;
    if (this->Kind == Kind::Primitive && this->IsSigned() != Other->IsSigned()) return false;
    
    if (auto Pointee = this->Pointee()) {
        return Pointee->Equals(Other->Pointee());
    }
    
    if (this->Kind == Kind::Complex && Other->Kind == Kind::Complex) return this->Data.Name == Other->Data.Name;
    
    throw; // TODO implement the rest
}

std::string TypeInfo::Str() {
    if (this == TypeInfo::Unresolved) {
        // We have to check this one first since `this` is a nullpointer for unresolved // TODO: don't map unresolved to the nullpointer
        return "<unresolved>";
    }
    if (this->Kind == Kind::Primitive || this->Kind == Kind::Complex) {
        return this->Data.Name;
    }
    if (auto Pointee = this->Pointee()) {
        return std::string("*").append(Pointee->Str());
    }
    
    throw;
}
