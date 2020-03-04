//
//  Type.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-08-18.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Type.h"
#include "Mangling.h"

#include <string>
#include <map>
#include <vector>
#include <ostream>

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
static NumericalType *_float32Ty;
static NumericalType *_float64Ty;


void Type::initPrimitives() {
    static bool didInitialize = false;
    if (didInitialize) return;
    
    _voidTy    = new Type(TypeID::Void);
    _boolTy    = new NumericalType(NumericalType::NumericalTypeID::Bool);
    _int8Ty    = new NumericalType(NumericalType::NumericalTypeID::Int8);
    _int16Ty   = new NumericalType(NumericalType::NumericalTypeID::Int16);
    _int32Ty   = new NumericalType(NumericalType::NumericalTypeID::Int32);
    _int64Ty   = new NumericalType(NumericalType::NumericalTypeID::Int64);
    _uint8Ty   = new NumericalType(NumericalType::NumericalTypeID::UInt8);
    _uint16Ty  = new NumericalType(NumericalType::NumericalTypeID::UInt16);
    _uint32Ty  = new NumericalType(NumericalType::NumericalTypeID::UInt32);
    _uint64Ty  = new NumericalType(NumericalType::NumericalTypeID::UInt64);
    _float32Ty = new NumericalType(NumericalType::NumericalTypeID::Float32);
    _float64Ty = new NumericalType(NumericalType::NumericalTypeID::Float64);
    
    didInitialize = true;
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
NumericalType* Type::getFloat32Type() { return _float32Ty; }
NumericalType* Type::getFloat64Type() { return _float64Ty; }



std::string Type::str_mangled() const {
    return mangling::mangleFullyResolved(this);
}

std::string Type::str_desc() const {
    if (isVoidTy()) return "void";
    
    // Void is the only type that doesn't have its own subclass, so all other `str_desc` calls should get dispatched to the respective implementation
    LKFatalError("should never reach here");
}



PointerType* Type::getPointerTo() {
    LKAssert(!isReferenceTy() && "pointer to reference is illegal");
    if (pointerTo) return pointerTo;
    return pointerTo = new PointerType(this);
}

ReferenceType* Type::getReferenceTo() {
    LKAssert(!isReferenceTy() && "reference to reference is illegal");
    if (referenceTo) return referenceTo;
    return referenceTo = new ReferenceType(this);
}



Type* Type::createTemporary(const std::string &name) {
    auto ty = StructType::create(name, {}, {});
    ty->flags.insert(Flags::IsTemporary);
    return ty;
}

static std::map<std::string, StructType *> templatedTemporaryTypes;

StructType* Type::createTemporary(const std::string &name, const std::vector<Type *> &templateArgs) {
    if (auto type = util::map::get_opt(templatedTemporaryTypes, name)) {
        return *type;
    }
    auto type = StructType::create(name, {}, templateArgs, parser::TokenSourceLocation());
    type->setFlag(Flags::IsTemporary);
    templatedTemporaryTypes[name] = type;
    return type;
}


#pragma mark - NumericalType


NumericalType* NumericalType::get(NumericalTypeID id) {
#define CASE(name) case NumericalTypeID::name: return Type::get##name##Type();
    switch (id) {
    CASE(Bool)
    CASE(Int8)
    CASE(Int16)
    CASE(Int32)
    CASE(Int64)
    CASE(UInt8)
    CASE(UInt16)
    CASE(UInt32)
    CASE(UInt64)
    CASE(Float32)
    CASE(Float64)
    }
#undef CASE
    LKFatalError("unknown type id");
}


bool NumericalType::isBoolTy() const {
    return numericalTypeId == NumericalTypeID::Bool;
}

bool NumericalType::isFloatTy() const {
    return numericalTypeId == NumericalTypeID::Float32 || numericalTypeId == NumericalTypeID::Float64;
}

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
        case NumericalTypeID::Float32:
        case NumericalTypeID::Float64:
        case NumericalTypeID::Bool:
            return false;
    }
}



std::string NumericalType::str_desc() const {
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
        case NumericalTypeID::Float32: return "f32";
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
        case NumericalTypeID::Int8:
        case NumericalTypeID::UInt8:
            return 8;
        case NumericalTypeID::Int16:
        case NumericalTypeID::UInt16:
            return 16;
        case NumericalTypeID::Int32:
        case NumericalTypeID::UInt32:
        case NumericalTypeID::Float32:
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
        case NumericalTypeID::Float32:
        case NumericalTypeID::Float64:
            return true;
    }
}


#pragma mark - PointerType

std::string PointerType::str_desc() const {
    return util::fmt::format("*{}", pointee->str_desc());
}



#pragma mark - ReferenceType

std::string ReferenceType::str_desc() const {
    return util::fmt::format("&{}", pointee->str_desc());
}



#pragma mark - FunctionType


std::string FunctionType::str_desc() const {
    std::ostringstream OS;
    
    OS << '(';
    util::vector::iterl(parameterTypes, [&OS](auto param, bool isLast) {
        OS << param->str_desc();
        if (!isLast) OS << ", ";
    });
    OS << ") -> " << returnType->str_desc();
    
    return OS.str();
}


#pragma mark - StructType

std::string StructType::str_desc() const {
    return mangling::demangle(name);
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




#pragma mark - TupleType

static std::map<std::vector<Type *>, TupleType *> registeredTupleTypes;

TupleType* TupleType::get(const std::vector<Type *> &members) {
    if (auto TT = util::map::get_opt(registeredTupleTypes, members)) {
        return *TT;
    }

    auto TT = new TupleType(members);
    registeredTupleTypes[members] = TT;
    return TT;
}

std::string TupleType::str_desc() const {
    std::ostringstream OS;
    
    OS << "(";
    util::vector::iterl(members, [&OS](auto member, bool isLast) {
        OS << member->str_desc();
        if (!isLast) OS << ", ";
    });
    OS << ")";
    
    return OS.str();
}
