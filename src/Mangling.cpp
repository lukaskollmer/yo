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
    
    inline constexpr char kTemplatedComplexTypePrefix = 'T';
//    inline constexpr char kComplexTypePrefix = 'C';

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
    
    ManglingStringBuilder& appendEncodedType(TypeInfo *TI);
    
    std::string& str() { return Buffer; }
};





std::string mangling::mangleCanonicalName(std::string_view type, std::string_view method, ast::FunctionSignature::FunctionKind kind) {
    ManglingStringBuilder mangler;
    
    switch (kind) {
        case ast::FunctionSignature::FunctionKind::GlobalFunction:
            return std::string(method);
        case ast::FunctionSignature::FunctionKind::StaticMethod:
            mangler.append("+"); break;
        case ast::FunctionSignature::FunctionKind::InstanceMethod:
            mangler.append("-"); break;
    }
    
    return mangler
        .appendWithCount(type)
        .appendWithCount(method)
        .str();
}



std::string mangling::mangleCanonicalNameForSignature(std::shared_ptr<ast::FunctionSignature> signature) {
    if (!signature->attributes->mangledName.empty()) {
        return signature->attributes->mangledName;
    }
    auto typeName = signature->kind == ast::FunctionSignature::FunctionKind::GlobalFunction ? "" : signature->implType->name->value;
    return mangleCanonicalName(typeName, signature->name, signature->kind);
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

ManglingStringBuilder& ManglingStringBuilder::appendEncodedType(TypeInfo *TI) {
    switch (TI->getKind()) {
        case TypeInfo::Kind::Primitive: {
#define HANDLE(t, s) if (TI->equals(TypeInfo::t)) { return append(s); }
            HANDLE(i8,  "c") HANDLE(u8,  "C")
            HANDLE(i16, "s") HANDLE(u16, "S")
            HANDLE(i32, "i") HANDLE(u32, "I")
            HANDLE(i64, "q") HANDLE(u64, "Q")
            HANDLE(Void, "v")
            HANDLE(Bool, "b")
            LKFatalError("unhandled type: %s", TI->str().c_str());
#undef HANDLE
        }
        
        case TypeInfo::Kind::Pointer:
            return append("P").appendEncodedType(TI->getPointee());
        
        case TypeInfo::Kind::Complex:
            return appendWithCount(TI->getName());
            //return append("{").append(TI->getName()).append("}");
        
        case TypeInfo::Kind::Function:
            throw;

        case TypeInfo::Kind::Typealias:
            return appendEncodedType(TI->getPointee());
        
        case TypeInfo::Kind::Unresolved:
            LKFatalError("should never reach here: %s", TI->str().c_str());
        
        case TypeInfo::Kind::ComplexTemplated: {
            throw;
        }
    }
    
    LKFatalError("[EncodeType] Unhandled type: %s", TI->str().c_str());
}





// Mangled name includes type encodings for return- & parameter types
std::string mangling::mangleFullyResolvedNameForSignature(std::shared_ptr<ast::FunctionSignature> signature) {
    if (!signature->attributes->mangledName.empty()) {
        return signature->attributes->mangledName;
    }
    
    using FK = ast::FunctionSignature::FunctionKind;
    ManglingStringBuilder mangler(kCommonPrefix);
    
    switch (signature->kind) {
        case FK::GlobalFunction:
            mangler.append(mangling::kFunctionAttributeGlobalFunction);
            break;
        case FK::InstanceMethod:
            mangler.append(mangling::kFunctionAttributeInstanceMethod);
            mangler.appendWithCount(signature->implType->name->value);
            break;
        case FK::StaticMethod:
            mangler.append(mangling::kFunctionAttributeStaticMethod);
            mangler.appendWithCount(signature->implType->name->value);
            break;
    }
    
    mangler.appendWithCount(signature->name);
    mangler.appendEncodedType(signature->returnType);
    
    for (auto &param : signature->parameters) {
        mangler.appendEncodedType(param->type);
    }
    
    return mangler.str();
}



std::string mangling::mangleTemplatedComplexType(TypeInfo *TI) {
    ManglingStringBuilder mangler(kCommonPrefix);
    mangler.append(kTemplatedComplexTypePrefix);
    mangler.appendWithCount(TI->getName());
    
    for (auto Ty : TI->getTemplateParameterTypes()) {
        mangler.appendEncodedType(Ty);
    }
    
    return mangler.str();
}


