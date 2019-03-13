//
//  CommandLine.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "CommandLine.h"


llvm::cl::opt<std::string> cl::InputFilename(llvm::cl::Positional, llvm::cl::desc("<input file>"), llvm::cl::Required);

llvm::cl::opt<bool> cl::PrintAST("print-ast", llvm::cl::desc("Print the Abstract Syntax Tree"));
llvm::cl::opt<bool> cl::EmitLLVM("emit-llvm", llvm::cl::desc("Emit LLVM IR"));

void print_version(llvm::raw_ostream &OS) {
    OS << "0.0.1";
    // TODO: a) get the version from somewhere, b) should this include a newline?
}

void cl::Init(int argc, const char *const *argv) {
    // TODO
    // - add groups, etc?
    // - is there an option to force 2 dashes for arguments that aren't just a single letter?
    // - remove all the default llvm stuff
    // - is there an option to have default values?
    llvm::cl::SetVersionPrinter(&print_version);
    llvm::cl::ParseCommandLineOptions(argc, argv);
}
