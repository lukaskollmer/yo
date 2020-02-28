//
//  Driver.h
//  yo
//
//  Created by Lukas Kollmer on 2020-02-15.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"

#include <string>


NS_START(yo::driver)

enum class OutputFileType : uint8_t {
    Binary = 1,
    ObjectFile,
    LLVM_IR,
    LLVM_BC,
    Assembly
};



struct Options {
    std::string inputFile;
    std::string stdlibRoot;
    bool optimize;
    bool emitDebugMetadata;
    util::OptionSet<OutputFileType> outputFileTypes;
    
    bool fnoInline;
    bool fzeroInitialize;
    
    bool dumpLLVM;
    bool dumpLLVMPreOpt;
    bool dumpAST;
    
    bool int_trapOnFatalError;
    
    std::string outputDirectory;
};


bool run(Options);

NS_END
