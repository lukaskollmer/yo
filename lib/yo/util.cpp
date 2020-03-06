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
#include <fstream>
#include <sstream>
#include <sys/stat.h>


using namespace yo;

char* util::fmt_cstr(const char *fmt, va_list va_args) {
    if (!fmt) LKFatalError("invalid arguments");
    
    va_list args;
    va_copy(args, va_args);
    
    auto char_count = vsnprintf(nullptr, 0, fmt, args);
    auto str = reinterpret_cast<char *>(calloc(char_count, sizeof(char)));
    va_end(args);
    
    va_copy(args, va_args);
    vsprintf(str, fmt, args);
    va_end(args);
    
    return str;
}


char* util::fmt_cstr(const char *format, ...) {
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



#pragma mark - util::string

bool util::string::contains(const std::string_view string, const std::string_view other) {
    return string.find(other) != std::string_view::npos;
}


bool util::string::has_prefix(const std::string_view string, const std::string_view prefix) {
    if (prefix.length() > string.length()) {
        return false;
    }
    if (prefix.length() == string.length()) {
        return prefix == string;
    }
    return string.substr(0, prefix.length()) == prefix;
}


bool util::string::has_suffix(const std::string_view string, const std::string_view suffix) {
    if (suffix.length() > string.length()) {
        return false;
    }
    if (suffix.length() == string.length()) {
        return string == suffix;
    }
    return string.substr(string.length() - suffix.length(), suffix.length()) == suffix;
}


void util::string::pad_left(std::string &str, size_t length, char c) {
    if (str.size() < length) {
        str.insert(0, length - str.size(), c);
    }
}

void util::string::pad_right(std::string &str, size_t length, char c) {
    if (str.size() < length) {
        str.append(length - str.size(), c);
    }
}



std::string util::string::substr_with_range(const std::string &string, Range range) {
    return string.substr(range.location, range.length);
}

std::string util::string::substr_from_index(const std::string &string, uint64_t index) {
    return string.substr(index, string.length() - index);
}

std::string util::string::substr_to_index(const std::string &string, uint64_t index) {
    if (index < 0) {
        return string.substr(0, string.length() + index);
    }
    return string.substr(0, index);
}


std::string util::string::replace_all(const std::string &string, const std::string &pattern, const std::string &replacement) {
    std::string retval = string;
    size_t index = 0;
    while (true) {
        // Locate next occurrence
        index = retval.find(pattern, index);
        if (index == std::string::npos) break;
        
        retval.replace(index, pattern.length(), replacement);
        index += replacement.length();
    }
    
    return retval;
}



std::vector<std::string> util::string::split(const std::string &string, const std::string &delimiter) {
    std::vector<std::string> retval;
    
    std::string::size_type pos = 0;
    std::string::size_type prev = 0;
    while ((pos = string.find(delimiter, prev)) != std::string::npos) {
        if (pos != prev) {
            retval.push_back(string.substr(prev, pos - prev));
        }
        prev = pos + delimiter.length();
    }
    
    // Append the last (or only, if delimiter wasn't found) substring
    if (prev != string.length()) {
        retval.push_back(string.substr(prev));
    }
    
    return retval;
}



std::string util::string::join(const std::vector<std::string> &strings, const std::string &delimiter) {
    std::string retval;
    
    for (auto it = strings.begin(); it != strings.end(); it++) {
        retval += *it;
        if (it + 1 != strings.end()) {
            retval += delimiter;
        }
    }
    return retval;
}


std::string& util::string::append_with_indentation(std::string &target, const std::string &other, unsigned int indentCount) {
    const std::string indent(indentCount, ' ');
    
    std::vector<std::string> indented;
    for (auto line : split(other, "\n")) {
        indented.push_back(indent + line);
    }
    
    target += join(indented, "\n");
    return target;
}


std::ostream& util::string::append_with_indentation(std::ostream &OS, const std::string &other, unsigned int indentCount) {
    const std::string indent(indentCount, ' ');
    
    auto lines = split(other, "\n");
    for (auto it = lines.begin(); it != lines.end(); it++) {
        OS << indent << *it;
        if (it + 1 != lines.end()) {
            OS << '\n';
        }
    }
    
    return OS;
}


std::string util::string::lastPathCompotent(const std::string &path) {
    auto pos = path.rfind('/');
    if (pos == std::string::npos) {
        return path;
    }
    return path.substr(pos + 1);
}

std::string util::string::excludingLastPathComponent(const std::string &path) {
    auto pos = path.rfind('/');
    if (pos == std::string::npos) {
        return path;
    }
    return path.substr(0, pos);
}

std::string util::string::excludingFileExtension(const std::string &path) {
    auto pos = path.rfind('.');
    if (pos == std::string::npos) {
        return path;
    }
    return path.substr(0, pos);
}


std::pair<std::string, std::string> util::string::extractPathAndFilename(const std::string &path) {
    auto pos = path.rfind('/');
    if (pos == std::string::npos) {
        return { "", path };
    }
    return {
        path.substr(0, pos),
        path.substr(pos+1)
    };
}


bool util::fs::file_exists(const std::string &path) {
    struct stat s;
    return stat(path.c_str(), &s) == 0;
}


std::string util::fs::read_file(const std::string& path) {
    // TODO interpret path == "-" as stdin
    std::ifstream file(path);
    std::ostringstream contents;
    contents << file.rdbuf();
    file.close();
    return contents.str();
}


// zero-based line number
std::string util::fs::read_specific_line(const std::string& path, uint64_t lineNumber) {
    lineNumber += 1;
    uint64_t currentLineNumber = 0;
    std::ifstream file(path);
    std::string line;
    
    while (currentLineNumber != lineNumber && std::getline(file, line)) {
        currentLineNumber += 1;
    };
    
    if (currentLineNumber == lineNumber) {
        return line;
    } else {
        LKFatalError("Unable to read %s:%llu", path.c_str(), lineNumber);
    }
}



std::string util::fs::path_utils::getFilename(const std::string &path) {
    if (path == "-") {
        return "<stdin>";
    }
    
    auto pos = path.rfind('/');
    if (pos == std::string::npos) {
        return path;
    }
    return path.substr(pos + 1);
}
