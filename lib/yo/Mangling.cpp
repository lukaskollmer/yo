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


/*
 // TODO move this to the documentation and make it part of the website?
 
 # Mangling reference:
 
 ## Functions
 
 ### Global (free) functions
 - Syntax: _F{name}{ret_ty}{param_tys}{tmpl_info}
 - {name}: the name of the function, prefixed w/ the count. eg: foo -> 3foo
 - {ret_ty}: mangled return type of the function
 - {param_tys}: mangled parameter types of the function
   Note: the length of this portion is not explicitly stated, since it is either the last part of the mangled identifier,
         or followed by the template info (which would be prefixed w/ `T`)
 - {tmpl_info}: if the function is a template instantiation: `T{tmpl_tys}`, with:
    - {tmpl_tys}: a list of the mangled template types used to instantiate the function, prefixed w/ the count (number of tmpl args)
 
 ### Global operator overload:
 - same as a global function, except:
    - `_F` is replaced w/ `_O`
    - {name} is replaced w/ the operator's encoding (a number), prefixed by the length of the encoding string
 
 ### Instance Method
 - Syntax: _I{self_ty}{name}{ret_ty}{param_tys}{tmpl_info}
 - {self_ty}: mangled name of the method's receiver type
 - {name}: method name, prefixed w/ count
 - {ret_ty}: mangled name of the return type
 - {param_tys}: mangled parameter types (does not contain the implicit self parameter)
 - {tmpl_infp}: template instantiation info. see global function.
    Note: this only lists template arguments used for instantiating this specific call, not the type on which it was instantiated.
    ie, this field is only present if the function itself is a template instantiation, regardless of whether its impl-type is templated.
 
 ### Instance Method that is an Operator Overload
 - Same as a non-operator-overload instance method, except:
    - {name} is replaced w/ `O{enc}`, where {enc} is the encoded operator being overloaded, prefixed w/ the length of the overload
 
 
 ### Static Method
 - same as an instance method, except:
    - `_I` is replaced w/ `_S`
 
 
 ## Pointer Type
 - `P{pointee}`
 
 ## Reference Type
 - `R{pointee}`
 
 ## Primitive built-in type
 - see table:
 |  type  | encoding |
 | :----: | :------: |
 | `void` |   `v`    |
 | `bool` |   `b`    |
 | `i8`   |   `c`    |
 | `i16`  |   `s`    |
 | `i32`  |   `i`    |
 | `i64`  |   `d`    |
 | `u8`   |   `C`    |
 | `u16`  |   `S`    |
 | `u32`  |   `I`    |
 | `u64`  |   `D`    |
 | `f32`  |   `h`    |
 | `f64`  |   `f`    |
 
 
 ## Struct Type
 - Syntax build-up:
    - A
    - t: if this is a template instantiation
    - name, prefixed w/ count
    - T: if this is a template instantiation
 
 ## Tuple Type
 - Syntax: B{num_elems}{elem_tys}
 - {num_elems}: number of elements in the tuple
 - {elem_tys}: the mangled element types
 
 
 Ideas:
 - well-known identifiers
    - ie, `_I` followed by a digit could be used to refer to well-known instance method names)
    - `A` followed by a non-digit that is not `t` could be used to refer to well-known type names
    - question: is only the identifier well-known (ie "this is an initializer"), or also the specific signature (ie "this is the copy constructor")
 - Encode type list multiplicity?
 - Option to refer to template args by "reference" (since the template arg is already in the mangled identifier, we don't need to include it again for each use)
 
 */


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
inline constexpr char kMangledTypeStructIsTemplatePrefix = 't';
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
    
    template <typename T>
    ManglingStringBuilder& append(const T& arg) {
        OS << arg;
        return *this;
    }
    
    ManglingStringBuilder& appendEncodedType(const irgen::Type*);
    
    std::string str() const { return OS.str(); }
};




#pragma mark - Canonical Mangling


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



#pragma mark - Mangling


static const std::map<irgen::NumericalType::NumericalTypeID, std::string_view> numericalTypeEncodings = {
    { irgen::NumericalType::NumericalTypeID::Bool, "b" },
    
    { irgen::NumericalType::NumericalTypeID::Int8,  "c" },
    { irgen::NumericalType::NumericalTypeID::Int16, "s" },
    { irgen::NumericalType::NumericalTypeID::Int32, "i" },
    { irgen::NumericalType::NumericalTypeID::Int64, "q" },
    
    { irgen::NumericalType::NumericalTypeID::UInt8,  "C" },
    { irgen::NumericalType::NumericalTypeID::UInt16, "S" },
    { irgen::NumericalType::NumericalTypeID::UInt32, "I" },
    { irgen::NumericalType::NumericalTypeID::UInt64, "Q" },
    
    { irgen::NumericalType::NumericalTypeID::Float32, "h" },
    { irgen::NumericalType::NumericalTypeID::Float64, "f" },
};



ManglingStringBuilder& ManglingStringBuilder::appendEncodedType(const irgen::Type *ty) {
    using TypeID = irgen::Type::TypeID;
    switch (ty->getTypeId()) {
        case TypeID::Void:
            return append(kMangledTypeVoidEncoding);
        
        case TypeID::Numerical: {
            auto *numTy = llvm::cast<irgen::NumericalType>(ty);
            return append(numericalTypeEncodings.at(numTy->getNumericalTypeID()));
        }
        
        case TypeID::Pointer: {
            auto *pointerTy = llvm::cast<irgen::PointerType>(ty);
            return append(kMangledTypePointerPrefix).appendEncodedType(pointerTy->getPointee());
        }
        
        case TypeID::Reference: {
            auto refTy = llvm::cast<irgen::ReferenceType>(ty);
            return append(kMangledTypeReferencePrefix).appendEncodedType(refTy->getReferencedType());
        }
        
        case TypeID::Function:
            LKFatalError("TODO");
        
        case TypeID::Tuple: {
            auto tupleTy = llvm::cast<irgen::TupleType>(ty);
            ManglingStringBuilder mangler(kMangledTypeTuplePrefix);
            mangler.append(tupleTy->memberCount());
            for (auto elemTy : tupleTy->getMembers()) {
                mangler.appendEncodedType(elemTy);
            }
            return append(mangler.str());
        }
        
        case TypeID::Struct: {
            return append(llvm::cast<irgen::StructType>(ty)->getName());
        }
    }
}


std::string mangleFullyResolved(const irgen::Type *type) {
    return ManglingStringBuilder().appendEncodedType(type).str();
}


// Mangled name includes type encodings for return- & parameter types
std::string mangleFullyResolved(std::shared_ptr<ast::FunctionDecl> funcDecl) {
    if (auto mangledName = funcDecl->getAttributes().mangledName; !mangledName.empty()) {
        return mangledName;
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
            mangler.appendEncodedType(funcDecl->getImplType());
            break;
        
        case ast::FunctionKind::StaticMethod:
            mangler.append(kPrefixStaticMethod);
            mangler.appendEncodedType(funcDecl->getImplType());
            break;
    }
    
    
    if (funcDecl->isOperatorOverload() && funcDecl->isOfFunctionKind(ast::FunctionKind::InstanceMethod)) {
        // instance method which is an operator overload
        mangler.append(kPrefixOperatorOverload);
    } else if (funcDecl->isOperatorOverload() && funcDecl->isOfFunctionKind(ast::FunctionKind::StaticMethod)) {
        LKFatalError("TODO support this as well?");
    }
    
    mangler.appendWithCount(funcDecl->getName());
    mangler.appendEncodedType(funcDecl->getSignature().returnType->getResolvedType());
    
    for (size_t idx = funcDecl->isOfFunctionKind(ast::FunctionKind::InstanceMethod); idx < funcDecl->getSignature().paramTypes.size(); idx++) {
        mangler.appendEncodedType(funcDecl->getSignature().paramTypes[idx]->getResolvedType());
    }
    
    if (!funcDecl->getResolvedTemplateArgTypes().empty()) {
        mangler.append(kMangledTemplateArgsListPrefix);
        mangler.append(funcDecl->getResolvedTemplateArgTypes().size());
        for (auto &ty : funcDecl->getResolvedTemplateArgTypes()) {
            mangler.appendEncodedType(ty);
        }
    }
    
    return mangler.str();
}



std::string mangleAsStruct(std::string_view name) {
    return ManglingStringBuilder(kMangledTypeStructPrefix).appendWithCount(name).str();
}

// TODO rewrite to use an irgen::StructType* object instead!!!!
std::string mangleFullyResolved(std::shared_ptr<ast::StructDecl> SD) {
    ManglingStringBuilder mangler(kMangledTypeStructPrefix);
    bool isTemplate = SD->resolvedTemplateArgTypes.size() > 0;
    
    if (isTemplate) {
        mangler.append(kMangledTypeStructIsTemplatePrefix);
    }
    
    mangler.appendWithCount(SD->name);
    
    if (isTemplate) {
        mangler.append(kMangledTemplateArgsListPrefix);
        mangler.append(SD->resolvedTemplateArgTypes.size());
        for (auto &ty : SD->resolvedTemplateArgTypes) {
            mangler.appendEncodedType(ty);
        }
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
        CASE(CR_Inc, "...")
        CASE(CR_Exc, "..<")
    }
#undef CASE
}



template <typename T>
void append_as_list(std::ostream &OS, const std::vector<T> &vec) {
    util::vector::iterl(vec, [&OS](auto &elem, bool isLast) {
        OS << elem;
        if (!isLast) OS << ", ";
    });
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
    
    std::vector<std::string> demangleTypeList();
    std::vector<std::string> demangleTemplateArgTypeList();
    
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
    if (input[0] == kMangledFunctionCommonPrefix) {
        input.remove_prefix(1);
    }
    
    auto [val, count] = this->readInt();
    if (count != 0 && input.size() == val + count) {
        LKFatalError("shouldn't end up here anymore");
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
    
    char functionKind = input[1];
    input.remove_prefix(2);
    
    std::ostringstream OS;
    std::string returnTy;
    std::optional<std::string> selfTy;
    std::vector<std::string> paramsTys, templateArgTys;
    
    switch (functionKind) {
        case kPrefixGlobalFunction:
            OS << extractString();
            break;
        
        case kPrefixStaticMethod:
        case kPrefixInstanceMethod: {
            selfTy = demangleType();
            OS << *selfTy;
            if (functionKind == kPrefixStaticMethod) {
                OS << "::";
            } else {
                LKAssert(functionKind == kPrefixInstanceMethod);
                OS << '.';
            }
            if (input[0] == kPrefixOperatorOverload) {
                input.remove_prefix(2); // 'O' and length of operator encoding (we know that the length is only 1 digit, since there are only a handful of operators (ie, never enough operators that the number of operators would be a number exceeding 9 digits)
                auto op = static_cast<ast::Operator>(extractInt());
                OS << "operator" << demangleOperatorIntoSymbol(op);
            } else {
                OS << extractString();
            }
            break;
        }
        
        case kPrefixOperatorOverload: {
            input.remove_prefix(1); // remove length of operator encoding
            auto op = static_cast<ast::Operator>(extractInt());
            OS << "operator" << demangleOperatorIntoSymbol(op);
            break;
        }
        
        default:
            util::fmt::print("FUCK {}", input);
            LKFatalError("");
    }
    
    returnTy = demangleType();
    paramsTys = demangleTypeList();
    
    if (input[0] == kMangledTemplateArgsListPrefix) {
        templateArgTys = demangleTemplateArgTypeList();
    }
    
    LKAssert(input.empty());
    
    if (!templateArgTys.empty()) {
        OS << '<';
        append_as_list(OS, templateArgTys);
        OS << '>';
    }
    
    OS << '(';
    if (selfTy.has_value()) {
        OS << '&' << *selfTy;
        if (!paramsTys.empty()) {
            OS << ", ";
        }
    }
    append_as_list(OS, paramsTys);
    OS << ") -> " << returnTy;
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
            std::ostringstream OS;
            bool isTemplateInstantiation = input[1] == kMangledTypeStructIsTemplatePrefix;
            input.remove_prefix(1 + isTemplateInstantiation);
            
            OS << extractString(); // typename
            
            if (isTemplateInstantiation) {
                LKAssert(input[0] == kMangledTemplateArgsListPrefix);
                OS << '<';
                append_as_list(OS, demangleTemplateArgTypeList());
                OS << '>';
            }
            return OS.str();
        }
        
        case kMangledTypeTuplePrefix: {
            input.remove_prefix(1);
            std::ostringstream OS;
            auto numElements = extractInt();
            OS << '(';
            for (auto idx = 0; idx < numElements; idx++) {
                OS << demangleType();
                if (idx < numElements - 1) {
                    OS << ", ";
                }
            }
            OS << ')';
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
    
    auto numElements = extractInt();
    std::ostringstream OS;
    OS << '(';
    
    for (auto idx = 0; idx < numElements; idx++) {
        OS << demangleType();
        if (idx < numElements - 1) {
            OS << ", ";
        }
    }
    
    OS << ')';
    return OS.str();
}


std::vector<std::string> Demangler::demangleTypeList() {
    auto at_end = [&]() {
        // second check since this might be a function template, in which case the template args are demangled elsewhere
        return input.empty() || input[0] == kMangledTemplateArgsListPrefix;
    };
    
    if (at_end()) return {};
    
    std::vector<std::string> types;
    while (true) {
        types.push_back(demangleType());
        if (at_end()) {
            break;
        }
    }
    return types;
}


std::vector<std::string> Demangler::demangleTemplateArgTypeList() {
    LKAssert(input[0] == kMangledTemplateArgsListPrefix);
    input.remove_prefix(1);
    
    auto numElements = extractInt();
    std::vector<std::string> types;
    
    for (size_t idx = 0; idx < numElements; idx++) {
        types.push_back(demangleType());
    }
    
    return types;
}



std::string demangle(std::string_view name) {
    return Demangler(name).demangle();
}


std::string demangleCanonicalName(std::string_view name) {
    std::ostringstream OS;
    
    switch (name[0]) {
        case kCanonicalPrefixOperatorOverload: {
            OS << demangleOperatorIntoSymbol(demangleCanonicalOperatorEncoding(name));
            break;
        }
        
        case kCanonicalPrefixStaticMethod: {
            util::fmt::print("{}", name);
            LKFatalError("TODO");
        }
        
        case kCanonicalPrefixInstanceMethod: {
            util::fmt::print("{}", name);
            LKFatalError("TODO");
        }
        default:
            OS << name;
            break;
    }
        
        
    return OS.str();
}

NS_END
