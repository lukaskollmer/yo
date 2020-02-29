//
//  yo-demangle.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-02-18.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#include "yo/Mangling.h"
#include "yo/util.h"

#include "llvm/Support/CommandLine.h"

#include <string>
#include <iostream>


static llvm::cl::list<std::string> inputNames(llvm::cl::Positional,
                                              llvm::cl::desc("[mangled name...]"),
                                              llvm::cl::ZeroOrMore);


int main(int argc, const char * argv[]) {
    llvm::cl::ParseCommandLineOptions(argc, argv);
    
    if (inputNames.empty()) {
        // demangle from stdin
        std::string input;
        while (std::getline(std::cin, input)) {
            std::cout << yo::mangling::demangle(input) << std::endl;
        }
    } else {
        for (const auto &input : inputNames) {
            std::cout << yo::mangling::demangle(input) << std::endl;
        }
    }
    
    return EXIT_SUCCESS;
}

