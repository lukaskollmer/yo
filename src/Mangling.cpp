//
//  Mangling.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-01.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Mangling.h"
#include "util.h"
#include <map>

using namespace yo;


namespace yo::mangling {
    inline constexpr char kCommonPrefix = '_';
    inline constexpr char kFunctionAttributeGlobalFunction = 'F';
    inline constexpr char kFunctionAttributeInstanceMethod = 'I';
    inline constexpr char kFunctionAttributeStaticMethod   = 'S';
    inline constexpr char kFunctionAttributeOperatorOverload = 'O';
    
    inline constexpr char kTemplatedComplexTypePrefix = 'T';
//    inline constexpr char kComplexTypePrefix = 'C';
    
    
    inline constexpr char kCanonicalPrefixInstanceMethod = '-';
    inline constexpr char kCanonicalPrefixStaticMethod = '+';
    inline constexpr char kCanonicalPrefixOperatorOverload = '~';

}


bool yo::mangling::isCanonicalInstanceMethodName(std::string_view ident) {
    return ident[0] == '-';
}


class ManglingStringBuilder {
    std::string Buffer;
  
public:
    ManglingStringBuilder() {}
    explicit ManglingStringBuilder(char initial) : Buffer(std::string(1, initial)) {}
    explicit ManglingStringBuilder(std::string_view initial) : Buffer(initial) {}
    
    ManglingStringBuilder& appendWithCount(std::string_view str) {
        Buffer.append(std::to_string(str.length()));
        Buffer.append(str);
        return *this;
    }
    
    ManglingStringBuilder& append(std::string_view str) {
        Buffer.append(str);
        return *this;
    }
    
    ManglingStringBuilder& append(char c) {
        Buffer.push_back(c);
        return *this;
    }
    
    ManglingStringBuilder& appendEncodedType(yo::irgen::Type *ty);
    
    std::string& str() { return Buffer; }
};





std::string mangling::mangleCanonicalName(std::string_view type, std::string_view method, ast::FunctionKind kind) {
    using FK = ast::FunctionKind;
    
    ManglingStringBuilder mangler;
    
    switch (kind) {
        case ast::FunctionKind::GlobalFunction:
            return std::string(method);
            
        case FK::OperatorOverload:
            mangler.append(kCanonicalPrefixOperatorOverload);
            mangler.append(method);
            return mangler.str();
        
        case FK::StaticMethod:
            mangler.append(kCanonicalPrefixStaticMethod);
            break;
        
        case FK::InstanceMethod:
            mangler.append(kCanonicalPrefixInstanceMethod);
            break;
    }
    
    return mangler
        .appendWithCount(type)
        .appendWithCount(method)
        .str();
}



std::string mangling::mangleCanonicalName(std::shared_ptr<ast::FunctionDecl> funcDecl) {
    if (!funcDecl->getAttributes().mangledName.empty()) {
        return funcDecl->getAttributes().mangledName;
    }
    auto typeName = funcDecl->getFunctionKind() == ast::FunctionKind::GlobalFunction ? "" : funcDecl->getImplType()->getName();
    return mangleCanonicalName(typeName, funcDecl->getName(), funcDecl->getFunctionKind());
}


/*
 type encodings:
 
 i8     c
 i16    s
 i32    i
 i64    q
 
 u8     C
 u16    S
 u32    I
 u64    Q
 
 */


static std::map<yo::irgen::NumericalType::NumericalTypeID, std::string_view> numericalTypeEncodings = {
    { yo::irgen::NumericalType::NumericalTypeID::Bool,    "b" },
    { yo::irgen::NumericalType::NumericalTypeID::Float64, "f" },
    
    { yo::irgen::NumericalType::NumericalTypeID::Int8,  "c" },
    { yo::irgen::NumericalType::NumericalTypeID::Int16, "s" },
    { yo::irgen::NumericalType::NumericalTypeID::Int32, "i" },
    { yo::irgen::NumericalType::NumericalTypeID::Int64, "q" },
    
    { yo::irgen::NumericalType::NumericalTypeID::UInt8,  "C" },
    { yo::irgen::NumericalType::NumericalTypeID::UInt16, "S" },
    { yo::irgen::NumericalType::NumericalTypeID::UInt32, "I" },
    { yo::irgen::NumericalType::NumericalTypeID::UInt64, "Q" },
};



ManglingStringBuilder& ManglingStringBuilder::appendEncodedType(yo::irgen::Type *ty) {
    using TypeID = yo::irgen::Type::TypeID;
    switch (ty->getTypeId()) {
        case TypeID::Void:
            return append("v");
        
        case TypeID::Numerical: {
            auto *numTy = static_cast<yo::irgen::NumericalType*>(ty);
            return append(numericalTypeEncodings.at(numTy->getNumericalTypeID()));
        }
        
        case TypeID::Pointer: {
            auto *pointerTy = static_cast<yo::irgen::PointerType*>(ty);
            return append("P").appendEncodedType(pointerTy->getPointee());
        }
        
        case TypeID::Function:
            LKFatalError("TODO");
        
        case TypeID::Struct: {
            return appendWithCount(static_cast<irgen::StructType *>(ty)->getName());
        }
            
    }
//    switch (TI->getKind()) {
//        case TypeInfo::Kind::Primitive: {
//#define HANDLE(t, s) if (TI->equals(TypeInfo::t)) { return append(s); }
//            HANDLE(i8,  "c") HANDLE(u8,  "C")
//            HANDLE(i16, "s") HANDLE(u16, "S")
//            HANDLE(i32, "i") HANDLE(u32, "I")
//            HANDLE(i64, "q") HANDLE(u64, "Q")
//            HANDLE(Void, "v")
//            HANDLE(Bool, "b")
//            LKFatalError("unhandled type: %s", TI->str().c_str());
//#undef HANDLE
//        }
//
//        case TypeInfo::Kind::Pointer:
//            return append("P").appendEncodedType(TI->getPointee());
//
//        case TypeInfo::Kind::Complex:
//            return appendWithCount(TI->getName());
//            //return append("{").append(TI->getName()).append("}");
//
//        case TypeInfo::Kind::Function:
//            throw;
//
//        case TypeInfo::Kind::Typealias:
//            return appendEncodedType(TI->getPointee());
//
//        case TypeInfo::Kind::Unresolved:
//            LKFatalError("should never reach here: %s", TI->str().c_str());
//
//        case TypeInfo::Kind::ComplexTemplated: {
//            throw;
//        }
//    }
//
//    LKFatalError("[EncodeType] Unhandled type: %s", TI->str().c_str());
}





// Mangled name includes type encodings for return- & parameter types
std::string mangling::mangleFullyResolved(std::shared_ptr<ast::FunctionDecl> funcDecl) {
    if (!funcDecl->getAttributes().mangledName.empty()) {
        return funcDecl->getAttributes().mangledName;
    }
    
    ManglingStringBuilder mangler(kCommonPrefix);
    
    switch (funcDecl->getFunctionKind()) {
        case ast::FunctionKind::GlobalFunction:
            mangler.append(kFunctionAttributeGlobalFunction);
            break;
        
        case ast::FunctionKind::OperatorOverload:
            mangler.append(kFunctionAttributeOperatorOverload);
            break;
        
        case ast::FunctionKind::InstanceMethod:
            mangler.append(kFunctionAttributeInstanceMethod);
            mangler.appendWithCount(funcDecl->getImplType()->getName());
            break;
        
        case ast::FunctionKind::StaticMethod:
            mangler.append(kFunctionAttributeStaticMethod);
            mangler.appendWithCount(funcDecl->getImplType()->getName());
            break;
    }
    
    mangler.appendWithCount(funcDecl->getName());
    mangler.appendEncodedType(funcDecl->getSignature().returnType->getResolvedType());
    
    for (auto& param : funcDecl->getSignature().parameters) {
        mangler.appendEncodedType(param->type->getResolvedType());
    }
    
    return mangler.str();
}



//std::string mangling::mangleTemplatedComplexType(TypeInfo *TI) {
//    ManglingStringBuilder mangler(kCommonPrefix);
//    mangler.append(kTemplatedComplexTypePrefix);
//    mangler.appendWithCount(TI->getName());
//
//    for (auto Ty : TI->getTemplateParameterTypes()) {
//        mangler.appendEncodedType(Ty);
//    }
//
//    return mangler.str();
//}


