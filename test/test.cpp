//
//  test.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-02-15.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#include "Mangling.h"
#include "util.h"

#include "gtest/gtest.h"

#include <iostream>
#include <string>
#include <vector>


const char raw_test_input[] = {
#include "mangling_input.txt.in"
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
    
    while (!input.empty()) {
        if (input.find('\n') == std::string_view::npos) {
            break;
        }
        
        auto line1 = read_line();
        if (line1.empty() || yo::util::string::has_prefix(line1, "#")) {
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
        EXPECT_EQ(yo::mangling::demangle(testCase.input), testCase.expected);
    }
}
