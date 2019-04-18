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



class ManglingStringBuilder {
    std::string Buffer;
  
public:
    ManglingStringBuilder() {}
    
    ManglingStringBuilder& appendWithCount(std::string_view Str) {
        Buffer.append(std::to_string(Str.length()));
        Buffer.append(Str);
        return *this;
    }
    
    ManglingStringBuilder& append(std::string_view Str) {
        Buffer.append(Str);
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



std::string mangling::MangleCanonicalNameForSignature(std::shared_ptr<ast::FunctionSignature> Signature) {
    auto Typename = Signature->Kind == ast::FunctionSignature::FunctionKind::GlobalFunction ? "" : Signature->ImplType->Name->Value;
    return MangleCanonicalName(Typename, Signature->Name, Signature->Kind);
}




ManglingStringBuilder& ManglingStringBuilder::appendEncodedType(TypeInfo *TI) {
    switch (TI->getKind()) {
        case TypeInfo::Kind::Primitive: {
            if (TI->Equals(TypeInfo::i8)) {
                return append("i");
            } else if (TI->Equals(TypeInfo::i16)) {
                return append("s");
            } else if (TI->Equals(TypeInfo::i32)) {
                return append("l");
            } else if (TI->Equals(TypeInfo::i64)) {
                return append("I");
            }
            throw;
        }
        
        case TypeInfo::Kind::Pointer:
            return append("^").appendEncodedType(TI->getPointee());
        
        case TypeInfo::Kind::Complex:
            return append("{").append(TI->getName()).append("}");
        
        case TypeInfo::Kind::Function:
            throw;
        
        case TypeInfo::Kind::Unresolved:
            LKFatalError("should never reach here: %s", TI->Str().c_str());
    }
    
    LKFatalError("[EncodeType] Unhandled type: %s", TI->Str().c_str());
}





// Mangled name includes type encodings for return- & parameter types
std::string mangling::MangleFullyResolvedNameForSignature(std::shared_ptr<ast::FunctionSignature> Signature) {
    using FK = ast::FunctionSignature::FunctionKind;
    ManglingStringBuilder Mangler;
    
    switch (Signature->Kind) {
        case FK::GlobalFunction:
            Mangler.append("~"); break;
        case FK::InstanceMethod:
            Mangler.append("-");
            Mangler.appendWithCount(Signature->ImplType->Name->Value);
            break;
        case FK::StaticMethod:
            Mangler.append("+");
            Mangler.appendWithCount(Signature->ImplType->Name->Value);
            break;
    }
    
    Mangler.appendWithCount(Signature->Name);
    Mangler.appendEncodedType(Signature->ReturnType);
    
    for (auto &Param : Signature->Parameters) {
        Mangler.appendEncodedType(Param->Type);
    }
    
    return Mangler.str();
}
