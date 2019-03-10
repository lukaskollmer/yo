//
//  util.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-25.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "util.h"

#include <csignal>
#include <cxxabi.h>


void _LKFatalError_imp(const char *funcname, int line, const char *format, ...) {
    
    if (format && strlen(format) > 0) {
        va_list args;
        va_start(args, format);
        auto char_count = vsnprintf(nullptr, 0, format, args);
        char buffer[char_count];
        va_end(args);
        
        va_start(args, format);
        vsprintf(buffer, format, args);
        
        printf("[%s:%i] Fatal Error: %s\n", funcname, line, buffer);
        va_end(args);
    } else {
        printf("[%s:%i] Fatal Error\n", funcname, line);
    }
    
    exit(EXIT_FAILURE);
}


void _precondition_imp(const char *func, const char *file, int line, const char *expr) {
    printf("Precondition failed: (%s), function %s, file %s, line %i\n", expr, func, file, line);
    //exit(EXIT_FAILURE);
    raise(SIGABRT);
}





std::string util::typeinfo::demangle(const char *name) {
    int status;
    
    std::unique_ptr<char, void(*)(void*)> res {
        abi::__cxa_demangle(name, NULL, NULL, &status),
        std::free
    };
    
    return (status == 0) ? res.get() : name;
}





std::string util::string::repeating(const char C, std::string::size_type N) {
    std::string String;
    String.reserve(N);
    for (int I = 0; I < N; I++) {
        String.push_back(C);
    }
    return String;
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



// Not tested
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
    auto Indent = repeating(' ', IndentCount);
    
    std::vector<std::string> Indented;
    for (auto Line : split(Other, "\n")) {
        Indented.push_back(Indent + Line);
    }
    
    Target += join(Indented, "\n");
    return Target;
}

