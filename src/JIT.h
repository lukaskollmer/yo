//
//  JIT.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-27.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <memory.h>
#include "llvm/IR/Module.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"


class JIT {
    llvm::ExecutionEngine *EE;
    
public:
    explicit JIT(std::unique_ptr<llvm::Module> Module);
    
    int RunMain(const char *const *envp);
};
