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
    
    explicit CharacterSet(std::vector<Character> chars) : characters(chars) {}
    explicit CharacterSet(std::string string);
    CharacterSet(Character start, Character end);
    CharacterSet(std::vector<std::pair<Character, Character>> ranges);
    
    bool contains(Character c);
    bool containsAllCharactersInString(std::string string);
    
    CharacterSet joined(CharacterSet other);
    
private:
    std::vector<Character> characters;
};


NS_END

