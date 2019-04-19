//
//  TypeInfo.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-27.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include <iostream>
#include <map>

#include "TypeInfo.h"


static std::map<std::string, TypeInfo *> Types = {
#define builtin(name) { #name, TypeInfo::getType_##name() }
    builtin(i8), builtin(i16), builtin(i32), builtin(i64),
    builtin(u8), builtin(u16), builtin(u32), builtin(u64),
    builtin(void), builtin(bool), builtin(double)
#undef builtin
};


TypeInfo *TypeInfo::GetWithName(const std::string &Name) {
    auto It = Types.find(Name);
    if (It != Types.end()) return It->second;
    
    auto TI = new TypeInfo();
    TI->Name = Name;
    Types[Name] = TI;
    return TI;
}



#define TI_GETTER(name, size)                   \
TypeInfo *TypeInfo::getType_##name() {          \
    static TypeInfo *TI = nullptr;              \
    if (!TI) {                                  \
        TI = new TypeInfo();                    \
        TI->Name = #name;                       \
        TI->Kind = TypeInfo::Kind::Primitive;   \
        TI->Size = size;                        \
    }                                           \
    return TI;                                  \
}


TI_GETTER(i8, 1)
TI_GETTER(i16, 2)
TI_GETTER(i32, 4)
TI_GETTER(i64, 8)

TI_GETTER(u8, 1)
TI_GETTER(u16, 2)
TI_GETTER(u32, 4)
TI_GETTER(u64, 8)

TI_GETTER(void, 0)
TI_GETTER(bool, 1)
TI_GETTER(double, 8)



bool TypeInfo::IsSigned() const {
    return this == i8 || this == i16 || this == i32 || this == i64;
}


bool TypeInfo::Equals(TypeInfo *Other) {
    if (this == Other) return true;
    if (this->Kind != Other->Kind || this->Size != Other->Size) return false;
    if (this->Kind == Kind::Primitive && this->IsSigned() != Other->IsSigned()) return false;
    
    if (auto Pointee = this->Pointee) {
        return Pointee->Equals(Other->Pointee);
    }
    
    if (this->Kind == Kind::Complex && Other->Kind == Kind::Complex) return this->Name == Other->Name;
    
    throw; // TODO implement the rest
}

std::string TypeInfo::Str() {
    if (this == TypeInfo::Unresolved || this->Kind == Kind::Unresolved) {
        // We have to check this one first since `this` is a nullpointer for unresolved // TODO: don't map unresolved to the nullpointer
        // TODO we also end up in this branch in some cases where the type isn't actually unresolved (template arguments, etc!)
        return "<unresolved>";
    }
    if (this->Kind == Kind::Primitive || this->Kind == Kind::Complex) {
        return this->Name;
    }
    if (auto Pointee = this->Pointee) {
        return std::string("*").append(Pointee->Str());
    }
    
    throw;
}
