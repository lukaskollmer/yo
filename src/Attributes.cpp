//
//  Attributes.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Attributes.h"

#include <optional>

using namespace yo::attributes;


namespace util::vector {
    template <typename T, typename F>
    bool contains_where(const std::vector<T> &vector, F fn) {
        for (auto &elem : vector) {
            if (fn(elem)) return true;
        }
        return false;
    }
    
    template <typename T, typename F>
    std::optional<T> first_where(const std::vector<T> &vector, F fn) {
        for (auto &elem : vector) {
            if (fn(elem)) return elem;
        }
        return std::nullopt;
    }
}



namespace attr_vec_utils {
    std::optional<Attribute> get_with_key(const std::vector<Attribute> &attributes, std::string_view key) {
        return util::vector::first_where(attributes, [&key](auto &attr) { return attr.getKey() == key; });
    }
    
    bool contains_key(const std::vector<Attribute> &attributes, std::string_view key) {
        return get_with_key(attributes, key).has_value();
    }
    
    template <typename T>
    T value_of_key(const std::vector<Attribute> &attributes, const std::string &key, T&& defaultValue) {
//        auto attr = get_with_key(<#const std::vector<Attribute> &attributes#>, <#std::string_view key#>)
//        if (!attr) return defaultValue;
//
//        if constexpr(std::is_same_v<T, bool>) {
//
//        }
    }
    
}

//#define ATTRIBUTE(scope, name) const std::string_view k##scope##AttributeKey__##name = #name;


#define ATTRIBUTE(scope, name) \
namespace builtin_attributes::scope::name { const std::string_view _yo_attr_key = #name; }

#define ATTR_MEMBER(scope, attr_name, member_name) \
namespace builtin_attributes::scope::attr_name { const std::string_view member_name = #member_name; }

ATTRIBUTE(function, no_mangle)
ATTRIBUTE(function, intrinsic)
ATTRIBUTE(function, variadic)
ATTRIBUTE(function, side_effects)

ATTR_MEMBER(function, side_effects, none)
ATTR_MEMBER(function, side_effects, io)
ATTR_MEMBER(function, side_effects, unknown)



std::vector<SideEffect> HandleSideEffectsAttribute(const Attribute &attribute) {
    precondition(attribute.key == builtin_attributes::function::side_effects::_yo_attr_key);
    
    auto values = std::get<std::vector<std::string>>(attribute.data);
    std::vector<SideEffect> sideEffects;
    bool containsNone = false;
    
    if (values.empty()) {
        return { SideEffect::Unknown };
    }
    
    for (auto &value : values) {
        if (value == builtin_attributes::function::side_effects::none) {
            containsNone = true;
            sideEffects.push_back(SideEffect::None);
        } else if (value == builtin_attributes::function::side_effects::io) {
            sideEffects.push_back(SideEffect::IO);
        } else {
            LKFatalError("unknown value in side_effects attribute: '%s'", value.c_str());
        }
    }
    
    assert_implication(containsNone, sideEffects.size() == 1);
    return sideEffects;
}




FunctionAttributes::FunctionAttributes(const std::vector<Attribute> &attributes) : FunctionAttributes() {    
    if (attributes.empty()) return;
    
    std::vector<std::string_view> handledAttributes;

    for (auto &attribute : attributes) {
        precondition(!util::vector::contains(handledAttributes, attribute.getKey()));
        handledAttributes.push_back(attribute.getKey());
        
        if (attribute.key == builtin_attributes::function::no_mangle::_yo_attr_key) {
            no_mangle = std::get<bool>(attribute.data);
        
        } else if (attribute.key == builtin_attributes::function::intrinsic::_yo_attr_key) {
            intrinsic = std::get<bool>(attribute.data);
        
        } else if (attribute.key == builtin_attributes::function::variadic::_yo_attr_key) {
            variadic = std::get<bool>(attribute.data);
        
        } else if (attribute.key == builtin_attributes::function::side_effects::_yo_attr_key) {
            side_effects = HandleSideEffectsAttribute(attribute);
        } else {
            LKFatalError("unknown attribute: '%s'", attribute.key.c_str());
        }
    }
}
