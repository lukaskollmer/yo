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




// Constructors
FunctionDeclAttributes::FunctionDeclAttributes(const std::vector<Attribute> &attributes) {
    throw;
//    std::vector<std::string_view> handledAttributes;
//
//    for (auto &attribute : attributes) {
//        precondition(!util::vector::contains(handledAttributes, attribute.getKey()), );
//    }
//
//    variadic = attr_vec_utils::contains_key(attributes, "variadic");
//    no_mangle = attr_vec_utils::contains_key(<#const std::vector<Attribute> &attributes#>, <#std::string_view key#>)
//    intrinsic =
//    side_effects = ;
    
}
