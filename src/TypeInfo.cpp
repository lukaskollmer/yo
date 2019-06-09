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


using namespace yo;


static std::map<std::string, TypeInfo *> Types = {
#define builtin(name) { #name, TypeInfo::getType_##name() }
    builtin(i8), builtin(i16), builtin(i32), builtin(i64),
    builtin(u8), builtin(u16), builtin(u32), builtin(u64),
    builtin(void), builtin(bool), builtin(double)
#undef builtin
};


TypeInfo *TypeInfo::GetWithName(const std::string &name, bool *didCreateNewType) {
    auto it = Types.find(name);
    if (it != Types.end()) return it->second;
    
    auto TI = new TypeInfo();
    TI->name = name;
    Types[name] = TI;
    if (didCreateNewType) {
        *didCreateNewType = true;
    }
    return TI;
}



#define TI_GETTER(name_, size_)                 \
TypeInfo *TypeInfo::getType_##name_() {         \
    static TypeInfo *TI = nullptr;              \
    if (!TI) {                                  \
        TI = new TypeInfo();                    \
        TI->name = #name_;                      \
        TI->kind = TypeInfo::Kind::Primitive;   \
        TI->size = size_;                       \
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

TypeInfo *TypeInfo::MakeComplex(const std::string &name) {
    auto TI = GetWithName(name);
    TI->name = name;
    TI->size = 8;
    TI->kind = Kind::Complex;
    return TI;
}

TypeInfo *TypeInfo::MakePointer(TypeInfo *pointee) {
    if (auto ptr = pointee->pointerTo) return ptr;
    
    auto ptr = new TypeInfo();
    ptr->kind = Kind::Pointer;
    ptr->size = 8;
    ptr->pointee = pointee;
    
    if (pointee->llvmType) {
        // The cast is only necessary because TypeInfo.h forward declares llvm::Type
        ptr->llvmType = reinterpret_cast<llvm::Type*>(pointee->llvmType->getPointerTo());
    }
    pointee->pointerTo = ptr;
    return ptr;
}

TypeInfo *TypeInfo::MakeTypealias(const std::string &name, TypeInfo *otherType) {
    bool didCreateNewType = false;
    auto TI = GetWithName(name, &didCreateNewType);
    //LKAssert(DidCreateNewType && "Creating typealias for already existing typename");
    TI->kind = Kind::Typealias;
    TI->name = name;
    TI->pointee = otherType;
    return TI;
}



TypeInfo *TypeInfo::MakeFunctionType(FunctionTypeInfo::CallingConvention callingConvention, std::vector<TypeInfo *> parameterTypes, TypeInfo *returnType) {
    auto TI = new TypeInfo();
    TI->kind = Kind::Function;
    TI->size = 8;
    TI->functionTypeInfo = std::make_unique<FunctionTypeInfo>();
    TI->functionTypeInfo->callingConvention = callingConvention;
    TI->functionTypeInfo->parameterTypes = parameterTypes;
    TI->functionTypeInfo->returnType = returnType;
    return TI;
}


unsigned TypeInfo::IndirectionCount() {
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

bool TypeInfo::IsVoidType() {
    return this->Equals(Void);
}


bool TypeInfo::Equals(TypeInfo *other) {
    if (this == other) return true;
    if (other == Unresolved) return false; // we know that this is nonnull bc of the check above
    
    // Typealiases
    if (kind == Kind::Typealias && this->pointee->Equals(other)) return true;
    if (other->kind == Kind::Typealias && other->pointee->Equals(this)) return true; // TODO should this call this->Equals(other->pointee?)
    
    if (this->name != other->name) return false;
    
    if (kind != other->kind || size != other->size) return false;
    
    if ((kind == Kind::Pointer || kind == Kind::Typealias) && kind == other->kind) {
        return pointee->Equals(other->pointee);
    }
    
    if (kind == Kind::Complex && other->kind == Kind::Complex) return this->name == other->name;
    
    throw; // TODO implement the rest
}

std::string TypeInfo::Str() const {
    if (this == TypeInfo::Unresolved) {
        // We have to check this one first since `this` is a nullpointer for unresolved // TODO: don't map unresolved to the nullpointer
        return "<unresolved>";
    }
    
    if (kind == Kind::Unresolved && !name.empty()) {
        return name;
    }
    
    if (kind == Kind::Primitive || kind == Kind::Complex) {
        return name;
    }
    
    if (kind == Kind::Pointer) {
        return std::string("*").append(pointee->Str());
    }
    
    if (kind == Kind::Typealias) {
        return std::string(name).append(" (alias for ").append(pointee->Str()).append(")");
    }
    
    if (kind == Kind::Function) {
        std::string str = "fn#";
        switch (functionTypeInfo->callingConvention) {
            case FunctionTypeInfo::CallingConvention::C:
                str.append("c"); break;
            case FunctionTypeInfo::CallingConvention::Yo:
                str.append("yo"); break;
        }
        str.append("(");
        for (auto it = functionTypeInfo->parameterTypes.begin(); it != functionTypeInfo->parameterTypes.end(); it++) {
            str.append((*it)->Str());
            if (it + 1 != functionTypeInfo->parameterTypes.end()) {
                str.append(", ");
            }
        }
        str.append("): ").append(functionTypeInfo->returnType->Str());
        return str;
    }
    
    throw;
}
