//
//  Driver.h
//  yo
//
//  Created by Lukas Kollmer on 2020-02-15.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util/util.h"
#include "util/OptionSet.h"

#include <string>


NS_START(yo::driver)

enum class OutputFileType : uint8_t {
    Binary     = 1 << 0,
    ObjectFile = 1 << 1,
    LLVM_IR    = 1 << 2,
    LLVM_BC    = 1 << 3,
    Assembly   = 1 << 4
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
