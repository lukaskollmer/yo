//
//  Type.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-08-18.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Type.h"

using namespace yo;
using namespace yo::irgen;


#pragma mark - Type

static Type *_voidTy;
static NumericalType *_boolTy;
static NumericalType *_int8Ty;
static NumericalType *_int16Ty;
static NumericalType *_int32Ty;
static NumericalType *_int64Ty;
static NumericalType *_uint8Ty;
static NumericalType *_uint16Ty;
static NumericalType *_uint32Ty;
static NumericalType *_uint64Ty;
static NumericalType *_float64Ty;


void Type::initPrimitives() {
    _voidTy = new Type(TypeID::Void);
    _boolTy = new NumericalType(NumericalType::NumericalTypeID::Bool);
    _int8Ty = new NumericalType(NumericalType::NumericalTypeID::Int8);
    _int16Ty = new NumericalType(NumericalType::NumericalTypeID::Int16);
    _int32Ty = new NumericalType(NumericalType::NumericalTypeID::Int32);
    _int64Ty = new NumericalType(NumericalType::NumericalTypeID::Int64);
    _uint8Ty = new NumericalType(NumericalType::NumericalTypeID::UInt8);
    _uint16Ty = new NumericalType(NumericalType::NumericalTypeID::UInt16);
    _uint32Ty = new NumericalType(NumericalType::NumericalTypeID::UInt32);
    _uint64Ty = new NumericalType(NumericalType::NumericalTypeID::UInt64);
    _float64Ty = new NumericalType(NumericalType::NumericalTypeID::Float64);
}


Type* Type::getVoidType() { return _voidTy; }
NumericalType* Type::getBoolType() { return _boolTy; }
NumericalType* Type::getInt8Type() { return _int8Ty; }
NumericalType* Type::getInt16Type() { return _int16Ty; }
NumericalType* Type::getInt32Type() { return _int32Ty; }
NumericalType* Type::getInt64Type() { return _int64Ty; }
NumericalType* Type::getUInt8Type() { return _uint8Ty; }
NumericalType* Type::getUInt16Type() { return _uint16Ty; }
NumericalType* Type::getUInt32Type() { return _uint32Ty; }
NumericalType* Type::getUInt64Type() { return _uint64Ty; }
NumericalType* Type::getFloat64Type() { return _float64Ty; }

//
//#define TYPE_GETTER(type, typeId, name)             \
//type* Type::name() {                                \
//    static type* ty = nullptr;                      \
//    if (ty == nullptr) { ty = new type(typeId); }   \
//    return ty;                                      \
//}
//
//
//TYPE_GETTER(Type, Type::TypeID::Void, getVoidType)
//TYPE_GETTER(NumericalType, NumericalType::NumericalTypeID::Bool, getBoolType)
//TYPE_GETTER(NumericalType, NumericalType::NumericalTypeID::Int8, getInt8Type)
//TYPE_GETTER(NumericalType, NumericalType::NumericalTypeID::Int16, getInt16Type)
//TYPE_GETTER(NumericalType, NumericalType::NumericalTypeID::Int32, getInt32Type)
//TYPE_GETTER(NumericalType, NumericalType::NumericalTypeID::Int64, getInt64Type)
//TYPE_GETTER(NumericalType, NumericalType::NumericalTypeID::UInt8, getUInt8Type)
//TYPE_GETTER(NumericalType, NumericalType::NumericalTypeID::UInt16, getUInt16Type)
//TYPE_GETTER(NumericalType, NumericalType::NumericalTypeID::UInt32, getUInt32Type)
//TYPE_GETTER(NumericalType, NumericalType::NumericalTypeID::UInt64, getUInt64Type)
//TYPE_GETTER(NumericalType, NumericalType::NumericalTypeID::Float64, getFloat64Type)
//
//
//#undef TYPE_GETTER




bool Type::isNominalType() const {
    return typeId == TypeID::Void || typeId == TypeID::Numerical;
}



std::string Type::str() const {
    return getName();
}

std::string Type::getName() const {
    if (isVoidTy()) return "void";
    // Void is the only type that doesn't have its own subclass, so all other `getName` calls should get dispatched to the relative subclass
    LKFatalError("should never reach here");
}



PointerType* Type::getPointerTo() {
    if (pointerTo) return pointerTo;
    return pointerTo = new PointerType(this);
}


#pragma mark - NumericalType


bool NumericalType::isIntegerTy() const {
    switch (numericalTypeId) {
        case NumericalTypeID::Int8:
        case NumericalTypeID::Int16:
        case NumericalTypeID::Int32:
        case NumericalTypeID::Int64:
        case NumericalTypeID::UInt8:
        case NumericalTypeID::UInt16:
        case NumericalTypeID::UInt32:
        case NumericalTypeID::UInt64:
            return true;
        case NumericalTypeID::Float64:
        case NumericalTypeID::Bool:
            return false;
    }
}

std::string NumericalType::str() const {
    return getName();
}

std::string NumericalType::getName() const {
    switch (numericalTypeId) {
        case NumericalTypeID::Bool: return "bool";
        case NumericalTypeID::Int8: return "i8";
        case NumericalTypeID::Int16: return "i16";
        case NumericalTypeID::Int32: return "i32";
        case NumericalTypeID::Int64: return "i64";
        case NumericalTypeID::UInt8: return "u8";
        case NumericalTypeID::UInt16: return "u16";
        case NumericalTypeID::UInt32: return "u32";
        case NumericalTypeID::UInt64: return "u64";
        case NumericalTypeID::Float64: return "f64";
    }
}


uint8_t NumericalType::getSize() const {
    if (numericalTypeId == NumericalTypeID::Bool) {
        return 0; // TODO: this seems like a bad idea?
    } else {
        return getPrimitiveSizeInBits() / 8;
    }
}


uint8_t NumericalType::getPrimitiveSizeInBits() const {
    switch (numericalTypeId) {
        case NumericalTypeID::Bool:
            return 1; // TODO 1 or 8?
        case NumericalTypeID::Int8:
        case NumericalTypeID::UInt8:
            return 8;
        case NumericalTypeID::Int16:
        case NumericalTypeID::UInt16:
            return 16;
        case NumericalTypeID::Int32:
        case NumericalTypeID::UInt32:
            return 32;
        case NumericalTypeID::Int64:
        case NumericalTypeID::UInt64:
        case NumericalTypeID::Float64:
            return 64;
    }
}

bool NumericalType::isSigned() const {
    switch (numericalTypeId) {
        case NumericalTypeID::Bool:
        case NumericalTypeID::UInt8:
        case NumericalTypeID::UInt16:
        case NumericalTypeID::UInt32:
        case NumericalTypeID::UInt64:
            return false;
        case NumericalTypeID::Int8:
        case NumericalTypeID::Int16:
        case NumericalTypeID::Int32:
        case NumericalTypeID::Int64:
        case NumericalTypeID::Float64:
            return true;
    }
}


#pragma mark - PointerType


std::string PointerType::str() const {
    return std::string("*").append(pointee->str());
}

std::string PointerType::getName() const {
    return "";
}


#pragma mark - FunctionType

std::string FunctionType::str() const {
    LKFatalError("TODO");
    return "TODO";
}

std::string FunctionType::getName() const {
    return "";
}


#pragma mark - StructType


std::string StructType::str() const {
    return name; // TODO add a bit more info here?
}

std::string StructType::getName() const {
    return name;
}

// TODO add an option to calculate a member's offset
std::pair<uint64_t, Type *> StructType::getMember(const std::string &name) const {
    uint64_t index = 0;
    for (auto& [memName, memType] : members) {
        if (memName == name) return {index, memType};
        index += 1;
    }
    return {0, nullptr};
}


