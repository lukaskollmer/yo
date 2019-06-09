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

namespace yo::attributes {
    class Attribute {
    public:
        enum class DataKind { Bool, String, Array };
        
        const std::string key;
        DataKind dataKind;
        
        const std::variant<
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
    
    
#pragma mark - Function Attributes
    
    enum class SideEffect {
        Unknown, None, IO
    };
    
    struct FunctionAttributes {
        bool variadic;
        bool no_mangle;
        bool intrinsic;
        bool arc;
        bool extern_;
        bool inline_;
        bool always_inline;
        std::string mangledName;
        std::vector<SideEffect> side_effects;
        
        FunctionAttributes() {
            variadic = false;
            no_mangle = false;
            intrinsic = false;
            arc = false;
            extern_ = false;
            mangledName = "";
            side_effects = { SideEffect::Unknown };
        }
        
        explicit FunctionAttributes(const std::vector<Attribute>&);
        
        bool operator ==(const FunctionAttributes &) const;
    };
    
    
    
#pragma mark - Struct Attributes
    
    struct StructAttributes {
        bool arc;
        bool no_init;
        
        StructAttributes() : arc(false), no_init(false) {}
        
        explicit StructAttributes(const std::vector<Attribute>&);
    };
}
