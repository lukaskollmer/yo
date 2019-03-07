//
//  CharacterSet.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//


#include "CharacterSet.h"

void AssertValidCharRange(char A, char B) {
    if (A > B) {
        LKLog("Invalid char range: '%c' > '%c'", A, B);
        assert(false);
    }
}


CharacterSet::CharacterSet(char Start, char End) {
    AssertValidCharRange(Start, End);
    
    for (char C = Start; C <= End; C++) {
        Characters.push_back(C);
    }
}


CharacterSet::CharacterSet(std::string String) {
    for (auto &C : String) {
        Characters.push_back(C);
    }
}


CharacterSet::CharacterSet(std::vector<std::pair<char, char>> Ranges) {
    for (auto &Range : Ranges) {
        AssertValidCharRange(Range.first, Range.second);
        for (char C = Range.first; C <= Range.second; C++) {
            Characters.push_back(C);
        }
    }
}


bool CharacterSet::Contains(Character Char) {
    for (auto &C : Characters) {
        if (C == Char) return true;
    }
    return false;
}

bool CharacterSet::ContainsAllCharactersInString(std::string String) {
    for (auto &C : String) {
        if (!Contains(C)) return false;
    }
    return true;
}


CharacterSet CharacterSet::Joined(CharacterSet Other) {
    auto Chars = Characters;
    Chars.reserve(Chars.size() + Other.Characters.size());
    Chars.insert(Chars.end(), Other.Characters.begin(), Other.Characters.end());
    
    return CharacterSet(Chars);
}
