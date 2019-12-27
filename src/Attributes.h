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

namespace yo {
namespace attributes {
class Attribute {
public:
    enum class DataKind { Bool, String, Array };
    
    const std::string key;
    const DataKind dataKind;
    
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
    
    template <typename T>
    const T& getData() const { return std::get<T>(data); }
};


#pragma mark - Function Attributes

enum class SideEffect {
    Unknown, None, IO
};

struct FunctionAttributes {
    bool no_mangle = false;
    bool intrinsic = false;
    bool arc = false;
    bool extern_ = false;
    bool inline_ = false;
    bool always_inline = false;
    bool startup = false;
    bool shutdown = false;
    bool int_isCtor = false;
    bool int_isFwdDecl = false;
    bool int_isDelayed = false;
    bool int_isSynthesized = false;
    std::string mangledName = "";
    std::vector<SideEffect> side_effects = { SideEffect::Unknown };
    
    FunctionAttributes() {}
    explicit FunctionAttributes(const std::vector<Attribute>&);
    
    bool operator ==(const FunctionAttributes &) const;
};



#pragma mark - Struct Attributes

struct StructAttributes {
    bool arc;
    bool no_init;
    
    StructAttributes() : arc(false), no_init(false) {}
    
    explicit StructAttributes(const std::vector<Attribute>&);
    
    bool operator ==(const StructAttributes &other) const {
        return arc == other.arc && no_init == other.no_init;
    }
};



#pragma mark - Variable Attributes

struct VarDeclAttributes {
    bool constexpr_ = false;
    
    VarDeclAttributes() {}
    
    explicit VarDeclAttributes(const std::vector<Attribute>&);
    
    bool operator ==(const VarDeclAttributes &other) const {
        return constexpr_ == other.constexpr_;
    }
};


} // namespace attributes
} // namespace yo

