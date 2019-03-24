//
//  JIT.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-27.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "JIT.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"


JIT::JIT(std::unique_ptr<llvm::Module> Module) {
#if 0
    llvm::InitializeNativeTarget();
    
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();
    LLVMLinkInMCJIT();
    
    
    std::string err;
    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, err);
    auto targetMachine = target->createTargetMachine(targetTriple, "", "", llvm::TargetOptions(), llvm::None, llvm::CodeModel::Model::Large, llvm::CodeGenOpt::None);
    
    Module->setDataLayout(targetMachine->createDataLayout());
    
    EE = llvm::EngineBuilder(std::move(Module))
        .setErrorStr(&err)
        .setEngineKind(llvm::EngineKind::JIT)
        .create(targetMachine);
    
    EE->finalizeObject();
#endif
}


int JIT::RunMain(const char *const *envp) {
#if 0
    // TODO pass proper args!!!
    std::vector<std::string> args = {"hello", "wrld"};
    auto F_main = EE->FindFunctionNamed("main");
    return EE->runFunctionAsMain(F_main, args, envp);
#endif
}
