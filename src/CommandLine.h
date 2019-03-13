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
#include "util.h"


NS_START(cl)


extern llvm::cl::opt<std::string> InputFilename;
extern llvm::cl::opt<bool> PrintAST;
extern llvm::cl::opt<bool> EmitLLVM;


//llvm::cl::opt<std::string> InputFilename(llvm::cl::Positional, llvm::cl::desc("<input file>"), llvm::cl::Required);
//llvm::cl::opt<bool> PrintAST("print-ast", llvm::cl::desc("Print the Abstract Syntax Tree"));

void Init(int argc, const char *const *argv);

NS_END
