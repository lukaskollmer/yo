//
//  mangling.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-02-15.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#include "yo/Mangling.h"
#include "yo/util.h"

#include "gtest/gtest.h"

#include <iostream>
#include <string>
#include <vector>


using namespace yo;

const char raw_test_input[] = {
#include "mangling_input.txt.in"
, 0x0
};



struct ManglingTestCase {
    std::string_view input, expected;
};


std::vector<ManglingTestCase> loadTestCases() {
    std::string_view input = raw_test_input;
    std::vector<ManglingTestCase> testCases;
    
    auto read_line = [&]() -> std::string_view {
        auto pos = input.find('\n');
        LKAssert(pos != std::string_view::npos);
        auto line = input.substr(0, pos);
        input.remove_prefix(pos + 1);
        return line;
    };
    
    int64_t prev_size = -1;
    
    while (!input.empty()) {
        if (prev_size == -1) {
            prev_size = input.size();
        } else {
            LKAssert(prev_size > input.size() && "bad recursion?");
        }
        
        if (input.find('\n') == std::string_view::npos) {
            break;
        }
        
        auto line1 = read_line();
        if (line1.empty() || util::string::has_prefix(line1, "#")) {
            continue;
        }
        
        auto line2 = read_line();
        LKAssert(!line2.empty());
        testCases.push_back({line1, line2});
    }
    
    return testCases;
}



TEST(yo, mangling) {
    for (ManglingTestCase testCase : loadTestCases()) {
        EXPECT_EQ(mangling::demangle(testCase.input), testCase.expected);
    }
}
