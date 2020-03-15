//
//  Attributes.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Attributes.h"
#include "util/util.h"
#include <optional>

using namespace yo;
using namespace yo::attributes;



template <typename T>
struct Mapping {
    using MemberT = typename util::typeinfo::member_ptr<T>::MemberT;
    
    const std::string_view key;
    const T memberPtr;
    
    Mapping(std::string_view key, T memberPtr) : key(key), memberPtr(memberPtr) {}
};



template <typename T, typename U>
void set_attr_val(U &dest, const Attribute &attr, T mapping) {
    dest.*(mapping.memberPtr) = std::get<typename T::MemberT>(attr.data);
}


#define IF_ATTR(attr, mapping)          \
if (attr.getKey() == mapping.key) {     \
    set_attr_val(*this, attr, mapping); \
    continue;                           \
}


namespace builtin_attributes {
#define ENTRY(scope, name_int, name_ext) \
static const auto name_int = Mapping(name_ext, &scope::name_int);

namespace func_decl {
ENTRY(FunctionAttributes, no_mangle, "no_mangle")
ENTRY(FunctionAttributes, mangledName, "mangle")
ENTRY(FunctionAttributes, extern_, "extern")
ENTRY(FunctionAttributes, inline_, "inline")
ENTRY(FunctionAttributes, always_inline, "always_inline")
ENTRY(FunctionAttributes, startup, "startup")
ENTRY(FunctionAttributes, shutdown, "shutdown")
ENTRY(FunctionAttributes, side_effects, "side_effects")
ENTRY(FunctionAttributes, intrinsic, "intrinsic")
ENTRY(FunctionAttributes, no_debug_info, "no_debug_info")
}


namespace struct_decl {
ENTRY(StructAttributes, no_init, "no_init")
ENTRY(StructAttributes, no_debug_info, "no_debug_info")
ENTRY(StructAttributes, trivial, "trivial")
}

namespace var_decl {
ENTRY(VarDeclAttributes, constexpr_, "constexpr")
}

#undef ENTRY
} // namespace builtin_attributes




namespace attr_vec_utils {
    std::optional<Attribute> get_with_key(const std::vector<Attribute> &attributes, std::string_view key) {
        return util::vector::first_where(attributes, [&key](const auto& attr) { return attr.getKey() == key; });
    }
    
    bool contains_key(const std::vector<Attribute> &attributes, std::string_view key) {
        return get_with_key(attributes, key).has_value();
    }
}



#pragma mark - Function Attributes


std::optional<SideEffect> sideEffectFromString(std::string_view str) {
    if (str == "none") return SideEffect::None;
    if (str == "io") return SideEffect::IO;
    if (str == "unknown") return SideEffect::Unknown;
    return std::nullopt;
}


std::vector<SideEffect> HandleSideEffectsAttribute(const Attribute &attribute) {
    LKAssert(attribute.key == builtin_attributes::func_decl::side_effects.key);
    
    auto values = std::get<std::vector<std::string>>(attribute.data);
    std::vector<SideEffect> sideEffects;
    bool containsNone = false;
    
    if (values.empty()) {
        return { SideEffect::Unknown };
    }
    
    for (const auto& value : values) {
        if (auto SE = sideEffectFromString(value)) {
            containsNone = containsNone || *SE == SideEffect::None;
            sideEffects.push_back(*SE);
        } else {
            LKFatalError("unknown value in side_effects attribute: '%s'", value.c_str());
        }
    }
    
    LKAssertImplication(containsNone, sideEffects.size() == 1);
    return sideEffects;
}


void ensure_mutual_exclusivity(const std::vector<std::string_view> &attributeNames, const std::initializer_list<std::string_view> &attributesToCheckFor) {
    std::vector<std::string_view> foundAttributeNames;
    for (const auto &attr_check : attributesToCheckFor) {
        if (util::vector::contains(attributeNames, attr_check)) {
            foundAttributeNames.push_back(attr_check);
        }
    }
    
    if (foundAttributeNames.size() > 1) {
        auto str = util::vector::reduce(foundAttributeNames, std::string(), [&foundAttributeNames](std::string &acc, const std::string_view &attr_name, int64_t idx) {
            acc.append("'").append(attr_name).append("'");
            if (idx + 2 < foundAttributeNames.size()) {
                acc.append(", ");
            } else if (idx + 2 == foundAttributeNames.size()) {
                acc.append(" and ");
            }
        });
        LKFatalError("Error: the attributes %s are mutually exclusive", str.c_str());
    }
}



FunctionAttributes::FunctionAttributes(const std::vector<Attribute> &attributes) : FunctionAttributes() {    
    if (attributes.empty()) return;
    
    std::vector<std::string_view> handledAttributes;

    for (const auto& attr : attributes) {
        LKAssert(!util::vector::contains(handledAttributes, attr.getKey()));
        handledAttributes.push_back(attr.getKey());
        
        IF_ATTR(attr, builtin_attributes::func_decl::no_mangle)
        IF_ATTR(attr, builtin_attributes::func_decl::mangledName)
        IF_ATTR(attr, builtin_attributes::func_decl::extern_)
        IF_ATTR(attr, builtin_attributes::func_decl::inline_)
        IF_ATTR(attr, builtin_attributes::func_decl::always_inline)
        IF_ATTR(attr, builtin_attributes::func_decl::startup)
        IF_ATTR(attr, builtin_attributes::func_decl::shutdown)
        IF_ATTR(attr, builtin_attributes::func_decl::intrinsic)
        IF_ATTR(attr, builtin_attributes::func_decl::no_debug_info)
        
        if (attr.key == builtin_attributes::func_decl::side_effects.key) {
            side_effects = HandleSideEffectsAttribute(attr);
            continue;
        }
      
        LKFatalError("unknown function attribute: '%s'", attr.key.c_str());
    }
    
    ensure_mutual_exclusivity(handledAttributes, {
        builtin_attributes::func_decl::mangledName.key,
        builtin_attributes::func_decl::no_mangle.key
    });
    
    ensure_mutual_exclusivity(handledAttributes, {
        builtin_attributes::func_decl::inline_.key,
        builtin_attributes::func_decl::always_inline.key
    });
    
    ensure_mutual_exclusivity(handledAttributes, {
        builtin_attributes::func_decl::extern_.key,
        builtin_attributes::func_decl::mangledName.key,
        builtin_attributes::func_decl::no_mangle.key
    });
}




#pragma mark - Struct Attributes


StructAttributes::StructAttributes(const std::vector<Attribute> &attributes) : StructAttributes() {
    if (attributes.empty()) return;
    
    std::vector<std::string_view> handledAttributes;
    
    for (const auto& attribute : attributes) {
        LKAssert(!util::vector::contains(handledAttributes, attribute.getKey()));
        handledAttributes.push_back(attribute.key);
        
        IF_ATTR(attribute, builtin_attributes::struct_decl::no_init)
        IF_ATTR(attribute, builtin_attributes::struct_decl::no_debug_info)
        IF_ATTR(attribute, builtin_attributes::struct_decl::trivial)
        
        LKFatalError("unknown struct attribute: '%s'", attribute.key.c_str());
    }
}


#pragma mark - VarDecl Attributes

VarDeclAttributes::VarDeclAttributes(const std::vector<Attribute> &attributes) : VarDeclAttributes() {
    if (attributes.empty()) return;
    
    std::vector<std::string_view> handledAttributes;
    
    for (const auto &attr : attributes) {
        LKAssert(!util::vector::contains(handledAttributes, attr.getKey()));
        
        IF_ATTR(attr, builtin_attributes::var_decl::constexpr_)
        
        LKFatalError("unhandled attribute: '%s'", attr.key.c_str());
    }
    
}
