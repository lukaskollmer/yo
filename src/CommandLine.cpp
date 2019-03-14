//
//  CommandLine.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "CommandLine.h"
#include "Version.h"



llvm::cl::opt<std::string> cl::InputFilename(llvm::cl::Positional, llvm::cl::desc("<input file>"), llvm::cl::Required);

llvm::cl::opt<bool> cl::PrintAST("print-ast", llvm::cl::desc("Print the Abstract Syntax Tree"));
llvm::cl::opt<bool> cl::EmitLLVM("emit-llvm", llvm::cl::desc("Emit LLVM IR"));

void print_version(llvm::raw_ostream &OS) {
    OS << "yo " << YO_VERSION << " (" << __DATE__ << ", " << __TIME__ << ")\n";
    OS << "- LLVM: " << YO_LLVM_VERSION << "\n";
    OS << "- " << COMPILER << "\n";
}

void cl::Init(int argc, const char *const *argv) {
    // TODO
    // - remove all the default llvm stuff
    // - is there an option to have default values?
    llvm::cl::SetVersionPrinter(&print_version);
    llvm::cl::ParseCommandLineOptions(argc, argv, "the yo programming language v" YO_VERSION "\n");
}
