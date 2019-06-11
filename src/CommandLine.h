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
    void Init(int argc, const char *const *argv);
}

namespace yo::cl::opts {
    bool Run();
    const std::vector<std::string>& RunArgs();
    
    const std::string& InputFilename();
    const std::string& OutputFilename();
    
    const std::string& StdlibRoot();
    
    bool PrintAST();
    bool EmitLLVM();
    bool DumpLLVM();
    bool DumpLLVMPreOpt();
    
    bool Optimize();
}
