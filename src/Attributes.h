//
//  Attributes.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <string>
#include <vector>
#include <variant>
#include "util.h"


namespace yo::annotations {
    // legacy
    const std::string intrinsic = "intrinsic";
    const std::string variadic  = "variadic";
    const std::string no_mangle = "no_mangle";
}

namespace yo::attributes {
    class Attribute {
        enum class DataKind { Bool, String, Array };
        
        std::string key;
        DataKind dataKind;
        
        std::variant<
            bool,
            std::string,
            std::vector<std::string>
        > data;
        
    public:
        explicit Attribute(std::string_view key, bool value = true) : key(key), dataKind(DataKind::Bool), data(value) {}
        Attribute(std::string_view key, const std::string &value) : key(key), dataKind(DataKind::String), data(value) {}
        Attribute(std::string_view key, const std::vector<std::string> &elements) : key(key), dataKind(DataKind::Array), data(elements) {}
        
        std::string_view getKey() const { return key; }
    };
    
    
    namespace function_attribute_keys {
        const std::string no_mangle = "no_mangle";
        const std::string intrinsic = "intrinsic";
        const std::string variadic  = "variadic";
    }
    
    enum class SideEffects {
        All, None, IO
    };
    
    struct FunctionDeclAttributes {
        bool variadic;
        bool no_mangle;
        bool intrinsic;
        std::vector<SideEffects> side_effects;
        
        FunctionDeclAttributes() : variadic(false), no_mangle(false), intrinsic(false), side_effects({SideEffects::All}) {}
        
        explicit FunctionDeclAttributes(const std::vector<Attribute>&); // TODO implement
    };
    
    
    // default attributes
    
    //std::vector<Attribute> getFunctionDeclDefaultAttributes() {
    //
    //    std::vector<Attribute> X = {
    //        Attribute("no_mangle", false),
    //        Attribute("intrinsic", false),
    //        Attribute("variadic", false)
    //    };
    //
    //    return {};
    //}
    
}
