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
#include <charconv>

using namespace yo;


namespace yo::mangling {
    inline constexpr char kCommonPrefix = '_';
    inline constexpr char kPrefixGlobalFunction = 'F';
    inline constexpr char kPrefixInstanceMethod = 'I';
    inline constexpr char kPrefixStaticMethod   = 'S';
    inline constexpr char kPrefixOperatorOverload = 'O';
    
    inline constexpr char kCanonicalPrefixInstanceMethod = '-';
    inline constexpr char kCanonicalPrefixStaticMethod = '+';
    inline constexpr char kCanonicalPrefixOperatorOverload = '~';
    
    inline constexpr char kTemplatedTypePrefix = 'T';
}


bool yo::mangling::isCanonicalInstanceMethodName(std::string_view ident) {
    return ident[0] == '-';
}


class ManglingStringBuilder {
    std::ostringstream OS;
  
public:
    ManglingStringBuilder() {}
    
    explicit ManglingStringBuilder(char initial) { OS << initial; }
    explicit ManglingStringBuilder(std::string_view initial) { OS << initial; }
    
    ManglingStringBuilder& appendWithCount(std::string_view str) {
        OS << str.length() << str;
        return *this;
    }
    
    ManglingStringBuilder& append(std::string_view str) {
        OS << str;
        return *this;
    }
    
    ManglingStringBuilder& append(char c) {
        OS << c;
        return *this;
    }
    
    ManglingStringBuilder& appendEncodedType(yo::irgen::Type *ty);
    
    std::string str() const { return OS.str(); }
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
    std::string typeName;
    if (funcDecl->isOfFunctionKind(ast::FunctionKind::StaticMethod) || funcDecl->isOfFunctionKind(ast::FunctionKind::InstanceMethod)) {
        typeName = funcDecl->getImplType()->getName();
    }
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


static const std::map<yo::irgen::NumericalType::NumericalTypeID, std::string_view> numericalTypeEncodings = {
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
            auto *numTy = static_cast<yo::irgen::NumericalType *>(ty);
            return append(numericalTypeEncodings.at(numTy->getNumericalTypeID()));
        }
        
        case TypeID::Pointer: {
            auto *pointerTy = static_cast<yo::irgen::PointerType *>(ty);
            return append("P").appendEncodedType(pointerTy->getPointee());
        }
        
        case TypeID::Reference: {
            auto refTy = static_cast<yo::irgen::ReferenceType *>(ty);
            return append("R").appendEncodedType(refTy->getReferencedType());
        }
        
        case TypeID::Function:
            LKFatalError("TODO");
        
        case TypeID::Struct: {
            return appendWithCount(static_cast<irgen::StructType *>(ty)->getName());
        }
    }
}





// Mangled name includes type encodings for return- & parameter types
std::string mangling::mangleFullyResolved(std::shared_ptr<ast::FunctionDecl> funcDecl) {
    if (!funcDecl->getAttributes().mangledName.empty()) {
        return funcDecl->getAttributes().mangledName;
    }
    
    ManglingStringBuilder mangler(kCommonPrefix);
    
    switch (funcDecl->getFunctionKind()) {
        case ast::FunctionKind::GlobalFunction:
            mangler.append(kPrefixGlobalFunction);
            break;
        
        case ast::FunctionKind::OperatorOverload:
            mangler.append(kPrefixOperatorOverload);
            break;
        
        case ast::FunctionKind::InstanceMethod:
            mangler.append(kPrefixInstanceMethod);
            mangler.appendWithCount(funcDecl->getImplType()->getName());
            break;
        
        case ast::FunctionKind::StaticMethod:
            mangler.append(kPrefixStaticMethod);
            mangler.appendWithCount(funcDecl->getImplType()->getName());
            break;
    }
    
    mangler.appendWithCount(funcDecl->getName());
    mangler.appendEncodedType(funcDecl->getSignature().returnType->getResolvedType());
    
    for (auto &paramType : funcDecl->getSignature().paramTypes) {
        mangler.appendEncodedType(paramType->getResolvedType());
    }
    
    if (!funcDecl->getResolvedTemplateArgTypes().empty()) {
        mangler.append(kTemplatedTypePrefix);
        for (auto &ty : funcDecl->getResolvedTemplateArgTypes()) {
            mangler.appendEncodedType(ty);
        }
    }
    
    return mangler.str();
}


std::string mangling::mangleFullyResolved(std::shared_ptr<ast::StructDecl> SD) {
    if (SD->resolvedTemplateArgTypes.empty()) {
        return SD->name;
    }
    
    ManglingStringBuilder mangler(kCommonPrefix);
    mangler.append(kTemplatedTypePrefix);
    mangler.appendWithCount(SD->name);
    mangler.append(kTemplatedTypePrefix);
    for (auto &ty : SD->resolvedTemplateArgTypes) {
        mangler.appendEncodedType(ty);
    }
    return mangler.str();
}




std::string mangling::encodeOperator(ast::Operator op) {
    return std::to_string(static_cast<uint8_t>(op));
}


std::string mangling::mangleCanonicalName(ast::Operator op) {
    std::string str;
    str.push_back(kCanonicalPrefixOperatorOverload);
    str.append(encodeOperator(op));
    return str;
}


ast::Operator mangling::demangleCanonicalOperatorEncoding(std::string_view sv) {
    uint8_t value;
    std::from_chars(sv.data() + 1, sv.data() + sv.size(), value);
    return static_cast<ast::Operator>(value);
}





#pragma mark - Demangling


std::string demangleGlobalFunction(std::string_view name) {
    LKFatalError("TODO");
}


std::string mangling::demangle(std::string_view name) {
    // TODO if this is false, attempt to demangle as canonical symbol?
    LKAssert(name[0] == kCommonPrefix);
    
    switch (name[1]) {
        case kPrefixGlobalFunction:
            return demangleGlobalFunction(name.substr(2));
        default:
            goto fail;
    }
    
fail:
    std::cout << "Unable to demangle symbol '" << name << "'" << std::endl;
    LKFatalError("");
}


