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
#include <optional>
#include "Type.h"

#include "llvm/Support/Casting.h"

using namespace yo;

NS_START(yo::mangling)


inline constexpr char kMangledFunctionCommonPrefix = '_';
inline constexpr char kPrefixGlobalFunction = 'F';
inline constexpr char kPrefixInstanceMethod = 'I';
inline constexpr char kPrefixStaticMethod   = 'S';
inline constexpr char kPrefixOperatorOverload = 'O';

inline constexpr char kCanonicalPrefixInstanceMethod = '-';
inline constexpr char kCanonicalPrefixStaticMethod = '+';
inline constexpr char kCanonicalPrefixOperatorOverload = '~';

inline constexpr char kMangledTemplateArgsListPrefix = 'T';
inline constexpr char kMangledTypeStructPrefix = 'A';
inline constexpr char kMangledTypeTuplePrefix = 'B';

inline constexpr char kMangledTypePointerPrefix = 'P';
inline constexpr char kMangledTypeReferencePrefix = 'R';
inline constexpr char kMangledTypeVoidEncoding = 'v';


bool isCanonicalInstanceMethodName(std::string_view ident) {
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
    
    ManglingStringBuilder& appendEncodedType(irgen::Type const*);
    
    std::string str() const { return OS.str(); }
};





std::string mangleCanonicalName(std::string_view type, std::string_view method, ast::FunctionKind kind) {
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



std::string mangleCanonicalName(std::shared_ptr<ast::FunctionDecl> funcDecl) {
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


static const std::map<irgen::NumericalType::NumericalTypeID, std::string_view> numericalTypeEncodings = {
    { irgen::NumericalType::NumericalTypeID::Bool,    "b" },
    { irgen::NumericalType::NumericalTypeID::Float64, "f" },
    
    { irgen::NumericalType::NumericalTypeID::Int8,  "c" },
    { irgen::NumericalType::NumericalTypeID::Int16, "s" },
    { irgen::NumericalType::NumericalTypeID::Int32, "i" },
    { irgen::NumericalType::NumericalTypeID::Int64, "q" },
    
    { irgen::NumericalType::NumericalTypeID::UInt8,  "C" },
    { irgen::NumericalType::NumericalTypeID::UInt16, "S" },
    { irgen::NumericalType::NumericalTypeID::UInt32, "I" },
    { irgen::NumericalType::NumericalTypeID::UInt64, "Q" },
};



ManglingStringBuilder& ManglingStringBuilder::appendEncodedType(irgen::Type const *ty) {
    using TypeID = irgen::Type::TypeID;
    switch (ty->getTypeId()) {
        case TypeID::Void:
            return append(kMangledTypeVoidEncoding);
        
        case TypeID::Numerical: {
            auto *numTy = llvm::dyn_cast<irgen::NumericalType>(ty);
            return append(numericalTypeEncodings.at(numTy->getNumericalTypeID()));
        }
        
        case TypeID::Pointer: {
            auto *pointerTy = llvm::dyn_cast<irgen::PointerType>(ty);
            return append(kMangledTypePointerPrefix).appendEncodedType(pointerTy->getPointee());
        }
        
        case TypeID::Reference: {
            auto refTy = llvm::dyn_cast<irgen::ReferenceType>(ty);
            return append(kMangledTypeReferencePrefix).appendEncodedType(refTy->getReferencedType());
        }
        
        case TypeID::Function:
            LKFatalError("TODO");
        
        case TypeID::Tuple: {
            auto tupleTy = llvm::dyn_cast<irgen::TupleType>(ty);
            
            ManglingStringBuilder mangler(kMangledTypeTuplePrefix);
            for (auto elemTy : tupleTy->getMembers()) {
                mangler.appendEncodedType(elemTy);
            }
            return appendWithCount(mangler.str());
        }
        
        case TypeID::Struct: {
            return appendWithCount(llvm::dyn_cast<irgen::StructType>(ty)->getName());
        }
    }
}


std::string mangleFullyResolved(irgen::Type const *type) {
    return ManglingStringBuilder().appendEncodedType(type).str();
}


// Mangled name includes type encodings for return- & parameter types
std::string mangleFullyResolved(std::shared_ptr<ast::FunctionDecl> funcDecl) {
    if (!funcDecl->getAttributes().mangledName.empty()) {
        return funcDecl->getAttributes().mangledName;
    }
    
    ManglingStringBuilder mangler(kMangledFunctionCommonPrefix);
    
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
    
    
    if (funcDecl->isOperatorOverload() && !funcDecl->isOfFunctionKind(ast::FunctionKind::OperatorOverload)) {
        // instance/static method which is an operator overload
        mangler.append(kPrefixOperatorOverload);
    }
    
    mangler.appendWithCount(funcDecl->getName());
    mangler.appendEncodedType(funcDecl->getSignature().returnType->getResolvedType());
    
    for (auto &paramType : funcDecl->getSignature().paramTypes) {
        mangler.appendEncodedType(paramType->getResolvedType());
    }
    
    if (!funcDecl->getResolvedTemplateArgTypes().empty()) {
        mangler.append(kMangledTemplateArgsListPrefix);
        for (auto &ty : funcDecl->getResolvedTemplateArgTypes()) {
            mangler.appendEncodedType(ty);
        }
    }
    
    return mangler.str();
}


// TODO rewrite to use an irgen::StructType* object instead!!!!
std::string mangleFullyResolved(std::shared_ptr<ast::StructDecl> SD) {
    if (SD->resolvedTemplateArgTypes.empty()) {
        return SD->name;
    }
    
    ManglingStringBuilder mangler(kMangledTypeStructPrefix);
    mangler.appendWithCount(SD->name);
    mangler.append(kMangledTemplateArgsListPrefix);
    for (auto &ty : SD->resolvedTemplateArgTypes) {
        mangler.appendEncodedType(ty);
    }
    return mangler.str();
}




std::string encodeOperator(ast::Operator op) {
    return std::to_string(static_cast<uint8_t>(op));
}


std::string mangleCanonicalName(ast::Operator op) {
    std::string str;
    str.push_back(kCanonicalPrefixOperatorOverload);
    str.append(encodeOperator(op));
    return str;
}


ast::Operator demangleCanonicalOperatorEncoding(std::string_view sv) {
    uint8_t value;
    std::from_chars(sv.data() + 1, sv.data() + sv.size(), value);
    return static_cast<ast::Operator>(value);
}





#pragma mark - Demangling


std::string_view demangleOperatorIntoSymbol(ast::Operator op) {
#define CASE(c, n) case ast::Operator::c: return n;
    switch (op) {
        CASE(Add, "+")
        CASE(Sub, "-")
        CASE(Mul, "*")
        CASE(Div, "/")
        CASE(Mod, "%")
        CASE(And, "&")
        CASE(Or,  "|")
        CASE(Xor, "^")
        CASE(Shl, "<<")
        CASE(Shr, ">>")
        CASE(Neg, "-")
        CASE(BNot, "~")
        CASE(BNeg, "!")
        CASE(LAnd, "&&")
        CASE(LOr, "||")
        CASE(EQ, "==")
        CASE(NE, "!=")
        CASE(LT, "<")
        CASE(LE, "<=")
        CASE(GT, ">")
        CASE(GE, ">=")
        CASE(FnPipe, "|>")
        CASE(FnCall, "()")
        CASE(Subscript, "[]")
        CASE(Assign, "=")
    }
#undef CASE
}




class Demangler {
    std::string_view input;
    
public:
    explicit Demangler(std::string_view sv) : input(sv) {}
    
    std::string demangle();
    
private:
    std::string demangleFunction();
    std::string demangleType();
    std::string demangleTuple();
    
    std::string demangleTypeList();
    
    /// Attempts to read an integer at the beginning of the input string
    /// Returns: tuple of (value, #digits)
    /// Returns {0,0} if no integer was found
    std::pair<uint64_t, uint64_t> readInt() {
        uint64_t value = 0;
        size_t count = 0;
        while (util::isDigit(input[count])) {
            count += 1;
        }
        if (count == 0) return {0, 0};
        
        for (size_t idx = 0; idx < count; idx++) {
            int charval = input[idx] - '0';
            value += pow<int>(10, count - idx - 1) * charval;
        }
        return {value, count};
    }
    
    uint64_t extractInt() {
        auto [length, count] = readInt();
        input.remove_prefix(count);
        return length;
    }
    
    std::string extractString() {
        auto [length, count] = readInt();
        auto string = std::string(input.substr(count, length));
        input.remove_prefix(count + length);
        return string;
    }
};


std::string Demangler::demangle() {
    if (input.length() >= 2 && input[0] == '_' && input[1] == '_') {
        return std::string(input);
    }
    
    auto [val, count] = this->readInt();
    if (count != 0 && input.size() == val + count) {
        input.remove_prefix(count);
    }
    
    switch (input[0]) {
        case kMangledFunctionCommonPrefix:
            return demangleFunction();
        
        case kMangledTypeStructPrefix:
            return demangleType();
        
        case kMangledTypeTuplePrefix:
            return demangleTuple();
    }
    
    return std::string(input);
}


std::string Demangler::demangleFunction() {
    LKAssert(input[0] == kMangledFunctionCommonPrefix);
    
    std::ostringstream OS;
    
    auto functionKindChar = input[1];
    input.remove_prefix(2);
    
    switch (functionKindChar) {
        case kPrefixGlobalFunction:
            OS << extractString();
            break;
        
        case kPrefixStaticMethod:
        case kPrefixInstanceMethod:
            OS << Demangler(extractString()).demangle();
            OS << (functionKindChar == kPrefixStaticMethod ? "::" : ".");
            if (input[0] == kPrefixOperatorOverload) {
                input.remove_prefix(2); // 'O' and length of operator encoding (we know that the length is only 1 digit, since there are only a handful of operators (ie, never enough operators that the number of operators would be a number exceeding 9 digits)
                auto op = static_cast<ast::Operator>(extractInt());
                OS << "operator" << demangleOperatorIntoSymbol(op);
            } else {
                OS << extractString();
            }
            break;
        
        case kPrefixOperatorOverload: {
            input.remove_prefix(1); // remove length of operator encoding
            //auto opCanon = util::fmt::format("~{}", extractInt());
            //auto op = demangleCanonicalOperatorEncoding(opCanon);
            auto op = static_cast<ast::Operator>(extractInt());
            OS << "operator" << demangleOperatorIntoSymbol(op);
            break;
        }
        
        default:
            LKFatalError("");
    }
    
    auto returnType = demangleType();
    auto paramTys = demangleTypeList();
    std::string templateArgs;
    
    if (input[0] == kMangledTemplateArgsListPrefix) {
        input.remove_prefix(1);
        templateArgs = demangleTypeList();
    }
    
    LKAssert(input.empty());
    
    if (!templateArgs.empty()) {
        OS << '<' << templateArgs << '>';
    }
    
    OS << '(' << paramTys << ") -> " << returnType;
    return OS.str();
}



std::string Demangler::demangleType() {
    switch (input[0]) {
        case kMangledTypeVoidEncoding:
            input.remove_prefix(1);
            return "void";
        
        case kMangledTypePointerPrefix:
            input.remove_prefix(1);
            return std::string("*").append(demangleType());
        
        case kMangledTypeReferencePrefix:
            input.remove_prefix(1);
            return std::string("&").append(demangleType());
        
        case kMangledTypeStructPrefix: {
            input.remove_prefix(1);
            std::ostringstream OS;
            OS << extractString();
            LKAssert(input[0] == 'T');
            input.remove_prefix(1);
            OS << '<';
            OS << demangleTypeList();
            OS << '>';
            return OS.str();
        }
        
        default:
            break;
    }
    
    if (util::isDigit(input[0])) {
        auto str = extractString();
        return Demangler(str).demangle();
    } else {
        if (auto numTyId = util::map::reverse_lookup(numericalTypeEncodings, input.substr(0, 1))) {
            input.remove_prefix(1);
            irgen::Type::initPrimitives();
            return irgen::NumericalType::get(*numTyId)->str_desc();
        }
    }
    
    LKFatalError("TODO");
}


std::string Demangler::demangleTuple() {
    LKAssert(input[0] == kMangledTypeTuplePrefix);
    input.remove_prefix(1);
    
    std::ostringstream OS;
    OS << '(';
    while (true) {
        OS << demangleType();
        if (input.empty()) {
            break;
        } else {
            OS << ", ";
        }
    }
    OS << ')';
    return OS.str();
}


std::string Demangler::demangleTypeList() {
    if (input.empty()) return "";
    
    std::ostringstream OS;
    while (true) {
        OS << demangleType();
        if (input.empty() || input[0] == kMangledTemplateArgsListPrefix) {
            // second check since this might be a function template, in which case the template args are demangled elsewhere
            break;
        } else {
            OS << ", ";
        }
    }
    return OS.str();
}




std::string demangle(std::string_view name) {
    return Demangler(name).demangle();
}

NS_END
