//
//  CharacterSet.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//


#include "CharacterSet.h"

using namespace yo;

void AssertValidCharRange(char a, char b) {
    LKAssert(a <= b);
}


CharacterSet::CharacterSet(char start, char end) {
    AssertValidCharRange(start, end);
    for (char c = start; c <= end; c++) {
        characters.push_back(c);
    }
}


CharacterSet::CharacterSet(std::string string) {
    for (auto &c : string) {
        characters.push_back(c);
    }
}


CharacterSet::CharacterSet(std::vector<std::pair<char, char>> ranges) {
    for (auto &range : ranges) {
        AssertValidCharRange(range.first, range.second);
        for (char c = range.first; c <= range.second; c++) {
            characters.push_back(c);
        }
    }
}


bool CharacterSet::contains(Character char_) {
    for (auto c : characters) {
        if (c == char_) return true;
    }
    return false;
}

bool CharacterSet::containsAllCharactersInString(std::string string) {
    for (auto c : string) {
        if (!contains(c)) return false;
    }
    return true;
}


CharacterSet CharacterSet::joined(CharacterSet other) {
    auto chars = characters;
    chars.reserve(chars.size() + other.characters.size());
    chars.insert(chars.end(), other.characters.begin(), other.characters.end());
    return CharacterSet(chars);
}
