//
//  CommandLine.h
//  yo
//
//  Created by Lukas Kollmer on 2019-10-09.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once


#include "util.h"
#include <vector>
#include <string>


namespace yo {
enum class OutputFileType {
    Binary,
    ObjectFile,
    LLVM_IR,
    LLVM_BC,
    Assembly,
    None
};


namespace cl {

struct Options {
    std::string inputFile;
    bool dumpLLVM;
    bool dumpLLVMPreOpt;
    bool optimize;
    bool printAST;
    bool pygmentize;
    std::string stdlibRoot;
    bool emitDebugMetadata;
    bool run;
    std::vector<OutputFileType> emit;
    std::vector<std::string> runArgs;
    bool int_trapOnFatalError;
    bool fzeroInitialize;
    bool fnoInline;
};


Options& get_options();


void init(int argc, const char * argv[]);
bool hasRawOption(const std::string&);

} // end namespace cl
} // end namespace yo
