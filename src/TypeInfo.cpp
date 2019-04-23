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

#include "llvm/IR/Type.h"


static std::map<std::string, TypeInfo *> Types = {
#define builtin(name) { #name, TypeInfo::getType_##name() }
    builtin(i8), builtin(i16), builtin(i32), builtin(i64),
    builtin(u8), builtin(u16), builtin(u32), builtin(u64),
    builtin(void), builtin(bool), builtin(double)
#undef builtin
};


TypeInfo *TypeInfo::GetWithName(const std::string &Name, bool *DidCreateNewType) {
    auto It = Types.find(Name);
    if (It != Types.end()) return It->second;
    
    auto TI = new TypeInfo();
    TI->Name_ = Name;
    Types[Name] = TI;
    if (DidCreateNewType) {
        *DidCreateNewType = true;
    }
    return TI;
}



#define TI_GETTER(name, size)                   \
TypeInfo *TypeInfo::getType_##name() {          \
    static TypeInfo *TI = nullptr;              \
    if (!TI) {                                  \
        TI = new TypeInfo();                    \
        TI->Name_ = #name;                      \
        TI->Kind_ = TypeInfo::Kind::Primitive;  \
        TI->Size_ = size;                       \
    }                                           \
    return TI;                                  \
}


TI_GETTER(i8, kSizeof_i8)
TI_GETTER(i16, kSizeof_i16)
TI_GETTER(i32, kSizeof_i32)
TI_GETTER(i64, kSizeof_i64)

TI_GETTER(u8, kSizeof_u8)
TI_GETTER(u16, kSizeof_u16)
TI_GETTER(u32, kSizeof_u32)
TI_GETTER(u64, kSizeof_u64)

TI_GETTER(void, 0)
TI_GETTER(bool, 1)
TI_GETTER(double, 8)


// Initializers

TypeInfo *TypeInfo::MakeComplex(const std::string &Name) {
    auto TI = GetWithName(Name);
    TI->Name_ = Name;
    TI->Size_ = 8;
    TI->Kind_ = Kind::Complex;
    return TI;
}

TypeInfo *TypeInfo::MakePointer(TypeInfo *Pointee) {
    if (auto Ptr = Pointee->PointerTo_) return Ptr;
    
    auto Ptr = new TypeInfo();
    Ptr->Kind_ = Kind::Pointer;
    Ptr->Size_ = 8;
    Ptr->Pointee_ = Pointee;
    
    if (Pointee->LLVMType_) {
        // The cast is only necessary because TypeInfo.h forward declares llvm::Type
        Ptr->LLVMType_ = reinterpret_cast<llvm::Type*>(Pointee->LLVMType_->getPointerTo());
    }
    Pointee->PointerTo_ = Ptr;
    return Ptr;
}

TypeInfo *TypeInfo::MakeTypealias(const std::string &Name, TypeInfo *OtherType) {
    bool DidCreateNewType = false;
    auto TI = GetWithName(Name, &DidCreateNewType);
    //precondition(DidCreateNewType && "Creating typealias for already existing typename");
    TI->Kind_ = Kind::Typealias;
    TI->Name_ = Name;
    TI->Pointee_ = OtherType;
    return TI;
}



unsigned TypeInfo::getIndirectionCount() {
    if (!IsPointer()) return 0;
    unsigned count = 0;
    
    auto T = this;
    while (T->IsPointer()) {
        count += 1;
        T = T->getPointee();
    }
    return count;
}


bool TypeInfo::IsSigned() {
    return this->Equals(i8) || this->Equals(i16) || this->Equals(i32) || this->Equals(i64);
}

static std::array<TypeInfo *, 8> IntegerTypes = {
    TypeInfo::i8, TypeInfo::i16, TypeInfo::i32, TypeInfo::i64,
    TypeInfo::u8, TypeInfo::u16, TypeInfo::u32, TypeInfo::u64
};

bool TypeInfo::IsIntegerType() {
    return std::find_if(IntegerTypes.begin(), IntegerTypes.end(), [this](TypeInfo *TI) { return this->Equals(TI); }) != IntegerTypes.end();
}


bool TypeInfo::Equals(TypeInfo *Other) {
    if (this == Other) return true;
    if (Other == Unresolved) return false; // we know that this is nonnull bc of the check above
    if (this->Name_ != Other->Name_) return false;
    
    // Typealiases
    if (Kind_ == Kind::Typealias && this->Pointee_->Equals(Other)) return true;
    if (Other->Kind_ == Kind::Typealias && Other->Pointee_->Equals(this)) return true;
    
    //std::cout << this->Str() << ", " << Other->Str() << std::endl;
    if (Kind_ != Other->Kind_ || Size_ != Other->Size_) return false;
    //if (Kind_ == Kind::Primitive && this->IsSigned() != Other->IsSigned()) return false;
    
    
    if ((Kind_ == Kind::Pointer || Kind_ == Kind::Typealias) && Kind_ == Other->Kind_) {
        return Pointee_->Equals(Other->Pointee_);
    }
    
    if (Kind_ == Kind::Complex && Other->Kind_ == Kind::Complex) return this->Name_ == Other->Name_;
    
    throw; // TODO implement the rest
}

std::string TypeInfo::Str() const {
    if (this == TypeInfo::Unresolved) {
        // We have to check this one first since `this` is a nullpointer for unresolved // TODO: don't map unresolved to the nullpointer
        return "<unresolved>";
    }
    
    if (Kind_ == Kind::Unresolved && !Name_.empty()) {
        return Name_;
    }
    
    if (Kind_ == Kind::Primitive || Kind_ == Kind::Complex) {
        return Name_;
    }
    
    if (Kind_ == Kind::Pointer) {
        return std::string("*").append(Pointee_->Str());
    }
    
    if (Kind_ == Kind::Typealias) {
        return std::string(Name_).append(" (alias for ").append(Pointee_->Str()).append(")");
    }
    
    throw;
}
