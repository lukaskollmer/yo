//
//  CharacterSet.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <string>
#include <vector>
#include <utility>

#include "util.h"


NS_START(yo)

class CharacterSet {
public:
    using Character = char;
    
    explicit CharacterSet(std::vector<char> Chars) : Characters(Chars) {}
    explicit CharacterSet(std::string String);
    CharacterSet(char Start, char End);
    CharacterSet(std::vector<std::pair<char, char>> Ranges);
    
    bool Contains(Character Char);
    bool ContainsAllCharactersInString(std::string String);
    
    CharacterSet Joined(CharacterSet Other);
    
private:
    std::vector<Character> Characters;
};


NS_END

