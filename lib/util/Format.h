//
//  Format.h
//  yo
//
//  Created by Lukas Kollmer on 2020-03-16.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"
#include <string>
#include <ostream>
#include <sstream>

NS_START(yo::util::fmt)

template <typename T>
struct formatter {
    static void format(std::ostream &OS, std::string_view flags, const T &arg) {
        OS << arg;
    }
};


template <>
struct formatter<bool> {
    static void format(std::ostream &OS, std::string_view flags, const bool &value) {
        OS << (value ? "true" : "false");
    }
};




template <typename T, typename... Ts>
void format_imp(std::ostream &OS, std::string_view format, T &&arg, Ts&&... args) {
    auto pos = format.find_first_of('{');
    if (pos == std::string_view::npos) {
        OS << format;
        return;
    }
    
    OS << format.substr(0, pos);
    if (format[pos + 1] == '}') {
        formatter<typeinfo::remove_cvref_t<T>>::format(OS, "", arg);
        format.remove_prefix(pos + 2);
    } else {
        auto end_pos = format.find_first_of('}', pos);
        LKAssert(end_pos != std::string_view::npos);
        std::string_view flags = format.substr(pos + 1, end_pos - 1 - pos);
        formatter<typeinfo::remove_cvref_t<T>>::format(OS, flags, arg);
        format.remove_prefix(end_pos + 1);
    }

    if constexpr(sizeof...(args) == 0) {
        LKAssert(format.find('{') == format.npos && "more format specifiers than arguments");
        OS << format;
    } else {
        LKAssert(format.find('{') != format.npos && "more arguments than format specifiers");
        format_imp(OS, format, std::forward<Ts>(args)...);
    }
}


template <typename... Ts>
std::string format(std::string_view format, Ts &&...args) {
    // TODO would be cool it this somehow worked
    //static_assert((format.find('{') != std::string_view::npos) == (sizeof...(args) > 0), "invalid format arguments");
    
    std::ostringstream OS;
    if constexpr(sizeof...(args) == 0) {
        LKAssert(format.find('{') == format.npos && "more format specifiers than arguments");
        OS << format;
    } else {
        LKAssert(format.find('{') != format.npos && "more arguments than format specifiers");
        format_imp(OS, format, std::forward<Ts>(args)...);
    }
    return OS.str();
}


template <typename... Ts>
void print(std::string_view format, Ts &&...args) {
    std::ostream &OS = std::cout;
    if constexpr(sizeof...(args) == 0) {
        LKAssert(format.find('{') == format.npos);
        OS << format;
    } else {
        format_imp(OS, format, std::forward<Ts>(args)...);
    }
    OS << std::endl;
}


NS_END // yo::util::fmt
