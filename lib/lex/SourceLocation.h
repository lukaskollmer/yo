//
//  SourceLocation.h
//  yo
//
//  Created by Lukas Kollmer on 2020-03-15.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <string>
#include <cstdint>
#include <iostream>


namespace yo::lex {

class SourceLocation {
    std::string filepath;
    uint64_t line;
    uint64_t column;
    uint64_t length;
    
public:
    SourceLocation() : filepath{}, line(0), column(0), length(0) {}
    SourceLocation(const std::string &filepath, uint64_t line, uint64_t column, uint64_t length)
    : filepath(filepath), line(line), column(column), length(length) {}
    
    const std::string& getFilepath() const {
        return filepath;
    }
    
    uint64_t getLine() const {
        return line;
    }
    
    uint64_t getColumn() const {
        return column;
    }
    
    uint64_t getLength() const {
        return length;
    }
    
    bool isEmpty() const {
        return filepath.empty() && line == 0 && column == 0 && length == 0;
    }
};

inline std::ostream& operator<<(std::ostream &OS, const SourceLocation &loc) {
    return OS << loc.getFilepath() << ":" << loc.getLine() << ":" << loc.getColumn();
}

} // ns yo::lex
