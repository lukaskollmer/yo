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


bool yo::mangling::IsCanonicalInstanceMethodName(std::string_view Ident) {
    return Ident[0] == '-';
}


class ManglingStringBuilder {
    std::string Buffer;
  
public:
    ManglingStringBuilder() {}
    explicit ManglingStringBuilder(char Initial) : Buffer(std::string(1, Initial)) {}
    explicit ManglingStringBuilder(std::string_view Initial) : Buffer(Initial) {}
    
    ManglingStringBuilder& appendWithCount(std::string_view Str) {
        Buffer.append(std::to_string(Str.length()));
        Buffer.append(Str);
        return *this;
    }
    
    ManglingStringBuilder& append(std::string_view Str) {
        Buffer.append(Str);
        return *this;
    }
    
    ManglingStringBuilder& append(char Char) {
        Buffer.push_back(Char);
        return *this;
    }
    
    ManglingStringBuilder& appendEncodedType(TypeInfo *TI);
    
    std::string& str() { return Buffer; }
};





std::string mangling::MangleCanonicalName(std::string_view Type, std::string_view Method, ast::FunctionSignature::FunctionKind Kind) {
    ManglingStringBuilder Mangler;
    
    switch (Kind) {
        case ast::FunctionSignature::FunctionKind::GlobalFunction:
            return std::string(Method);
        case ast::FunctionSignature::FunctionKind::StaticMethod:
            Mangler.append("+"); break;
        case ast::FunctionSignature::FunctionKind::InstanceMethod:
            Mangler.append("-"); break;
    }
    
    return Mangler
        .appendWithCount(Type)
        .appendWithCount(Method)
        .str();
}



std::string mangling::MangleCanonicalNameForSignature(std::shared_ptr<ast::FunctionSignature> signature) {
    if (!signature->attributes->mangledName.empty()) {
        return signature->attributes->mangledName;
    }
    auto typeName = signature->Kind == ast::FunctionSignature::FunctionKind::GlobalFunction ? "" : signature->ImplType->Name->Value;
    return MangleCanonicalName(typeName, signature->Name, signature->Kind);
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
#define HANDLE(t, s) if (TI->Equals(TypeInfo::t)) { return append(s); }
            HANDLE(i8,  "c") HANDLE(u8,  "C")
            HANDLE(i16, "s") HANDLE(u16, "S")
            HANDLE(i32, "i") HANDLE(u32, "I")
            HANDLE(i64, "q") HANDLE(u64, "Q")
            HANDLE(Void, "v")
            HANDLE(Bool, "b")
            LKFatalError("unhandled type: %s", TI->Str().c_str());
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
            LKFatalError("should never reach here: %s", TI->Str().c_str());
        
        case TypeInfo::Kind::ComplexTemplated: {
            throw;
        }
    }
    
    LKFatalError("[EncodeType] Unhandled type: %s", TI->Str().c_str());
}





// Mangled name includes type encodings for return- & parameter types
std::string mangling::MangleFullyResolvedNameForSignature(std::shared_ptr<ast::FunctionSignature> signature) {
    if (!signature->attributes->mangledName.empty()) {
        return signature->attributes->mangledName;
    }
    
    using FK = ast::FunctionSignature::FunctionKind;
    ManglingStringBuilder mangler(kCommonPrefix);
    
    switch (signature->Kind) {
        case FK::GlobalFunction:
            mangler.append(mangling::kFunctionAttributeGlobalFunction);
            break;
        case FK::InstanceMethod:
            mangler.append(mangling::kFunctionAttributeInstanceMethod);
            mangler.appendWithCount(signature->ImplType->Name->Value);
            break;
        case FK::StaticMethod:
            mangler.append(mangling::kFunctionAttributeStaticMethod);
            mangler.appendWithCount(signature->ImplType->Name->Value);
            break;
    }
    
    mangler.appendWithCount(signature->Name);
    mangler.appendEncodedType(signature->ReturnType);
    
    for (auto &param : signature->Parameters) {
        mangler.appendEncodedType(param->Type);
    }
    
    return mangler.str();
}



std::string mangling::MangleTemplatedComplexType(TypeInfo *TI) {
    ManglingStringBuilder Mangler(kCommonPrefix);
    Mangler.append(kTemplatedComplexTypePrefix);
    Mangler.appendWithCount(TI->getName());
    
    for (auto Ty : TI->getTemplateParameterTypes()) {
        Mangler.appendEncodedType(Ty);
    }
    
    return Mangler.str();
}


