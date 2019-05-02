//
//  util.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-25.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "util.h"

#include <memory>
#include <csignal>
#include <cxxabi.h>
#include <cstring>
#include <cstdarg>


using namespace yo;

char *util::fmt_cstr(const char *format, ...) {
    if (format && strlen(format) > 0) {
        va_list args;
        va_start(args, format);
        
        auto char_count = vsnprintf(NULL, 0, format, args);
        char *str = (char *) calloc(char_count, sizeof(char));
        va_end(args);
        
        va_start(args, format);
        vsprintf(str, format, args);
        return str;
    } else {
        LKFatalError("invalid arguments");
    }
}





std::string util::typeinfo::demangle(const char *name) {
    int status;
    
    std::unique_ptr<char, void(*)(void*)> res {
        abi::__cxa_demangle(name, NULL, NULL, &status),
        std::free
    };
    
    return (status == 0) ? res.get() : name;
}


bool util::string::contains(const std::string_view String, const std::string_view Other) {
    return String.find(Other) != std::string_view::npos;
}


bool util::string::has_prefix(const std::string_view String, const std::string_view Prefix) {
    if (Prefix.length() > String.length()) {
        return false;
    }
    if (Prefix.length() == String.length()) {
        return Prefix == String;
    }
    return String.substr(0, Prefix.length()) == Prefix;
}


bool util::string::has_suffix(const std::string_view String, const std::string_view Suffix) {
    if (Suffix.length() > String.length()) {
        return false;
    }
    if (Suffix.length() == String.length()) {
        return String == Suffix;
    }
    return String.substr(String.length() - Suffix.length(), Suffix.length()) == Suffix;
}


std::string util::string::substr_with_range(const std::string String, Range Range) {
    return String.substr(Range.Location, Range.Length);
}

std::string util::string::substr_from_index(const std::string String, LKUInteger Index) {
    return String.substr(Index, String.length() - Index);
}

std::string util::string::substr_to_index(const std::string String, LKInteger Index) {
    if (Index < 0) {
        return String.substr(0, String.length() + Index);
    }
    return String.substr(0, Index);
}


std::string util::string::replace_all(const std::string String, const std::string Pattern, const std::string Replacement) {
    std::string Retval = String;
    size_t Index = 0;
    while (true) {
        // Locate next occurrence
        Index = Retval.find(Pattern, Index);
        if (Index == std::string::npos) break;
        
        Retval.replace(Index, Pattern.length(), Replacement);
        Index += Replacement.length();
    }
    
    return Retval;
}



std::vector<std::string> util::string::split(const std::string String, const std::string Delimiter) {
    std::vector<std::string> Retval;
    
    std::string::size_type Pos = 0;
    std::string::size_type Prev = 0;
    while ((Pos = String.find(Delimiter, Prev)) != std::string::npos) {
        if (Pos != Prev) {
            Retval.push_back(String.substr(Prev, Pos - Prev));
        }
        Prev = Pos + Delimiter.length();
    }
    
    // Append the last (or only, if delimiter wasn't found) substring
    if (Prev != String.length()) {
        Retval.push_back(String.substr(Prev));
    }
    
    return Retval;
}



std::string util::string::join(const std::vector<std::string> &Strings, const std::string Delimiter) {
    std::string Retval;
    
    for (auto It = Strings.begin(); It != Strings.end(); It++) {
        Retval += *It;
        if (It + 1 != Strings.end()) {
            Retval += Delimiter;
        }
    }
    return Retval;
}


std::string& util::string::append_with_indentation(std::string &Target, std::string &&Other, unsigned int IndentCount) {
    std::string Indent(IndentCount, ' ');
    
    std::vector<std::string> Indented;
    for (auto Line : split(Other, "\n")) {
        Indented.push_back(Indent + Line);
    }
    
    Target += join(Indented, "\n");
    return Target;
}


std::string util::string::lastPathCompotent(std::string &Path) {
    auto Pos = Path.rfind('/');
    if (Pos == std::string::npos) {
        return Path;
    }
    return Path.substr(Pos + 1);
}

std::string util::string::excludingLastPathComponent(std::string &Path) {
    auto Pos = Path.rfind('/');
    if (Pos == std::string::npos) {
        return Path;
    }
    return Path.substr(0, Pos);
}

std::string util::string::excludingFileExtension(const std::string &Path) {
    auto Pos = Path.rfind('.');
    if (Pos == std::string::npos) {
        return Path;
    }
    return Path.substr(0, Pos);
}








/*
 namespace fmt {
 
 
 template <typename T>
 void format_helper(std::ostringstream &OSS, std::string_view flags, T&& arg) {
 std::cout << util::typeinfo::LKTypeInfo<T>::Name << std::endl;
 OSS << arg;
 }
 
 template <>
 void format_helper(std::ostringstream &OSS, std::string_view flags, char* &&arg) {
 OSS << "FUUUUUUCK";
 }
 
 
 template <typename T>
 struct formatter {
 static void format(std::ostringstream &OSS, std::string_view flags, T&& arg) {
 OSS << arg;
 }
 };
 
 
 template <typename T, typename... Args>
 void format_imp(std::ostringstream &OSS, std::string_view format, T&& arg, Args &&...args) {
 auto pos = format.find_first_of("{");
 if (pos == format.npos) {
 OSS << format;
 return;
 }
 OSS << format.substr(0, pos);
 if (format[pos + 1] == '}') {
 format_helper(OSS, "", arg);
 //OSS << arg;
 format.remove_prefix(pos + 2);
 } else {
 LKFatalError("TODO: implement");
 }
 
 if constexpr(sizeof...(args) == 0) {
 OSS << format;
 } else {
 format_imp(OSS, format, std::forward<Args>(args)...);
 }
 }
 
 
 template <typename... Args>
 std::string format(std::string_view format, Args &&...args) {
 std::ostringstream OS;
 format_imp(OS, format, std::forward<Args>(args)...);
 return OS.str();
 }
 }
 */
