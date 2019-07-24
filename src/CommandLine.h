//
//  CommandLine.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <string>
#include <vector>


namespace yo::cl {
    void init(int argc, const char *const *argv);
}

namespace yo::cl::opts {
    bool pygmentize();
    bool run();
    const std::vector<std::string>& runArgs();
    
    const std::string& inputFile();
    const std::string& outputFile();
    
    const std::string& stdlibRoot();
    
    bool printAST();
    bool emitLLVM();
    bool dumpLLVM();
    bool dumpLLVMPreOpt();
    
    // experimental flags
    bool optimize();
    bool arc();
}
