//
//  CommandLine.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <string>
#include "llvm/Support/CommandLine.h"


namespace cl {
    extern llvm::cl::opt<std::string> InputFilename;
    extern llvm::cl::opt<bool> PrintAST;
    extern llvm::cl::opt<bool> EmitLLVM;
    
    void Init(int argc, const char *const *argv);
}
