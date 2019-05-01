//
//  CommandLine.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-26.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "CommandLine.h"
#include "Version.h"
#include <iostream>

static llvm::cl::OptionCategory CLIOptionsCategory("General Options");


llvm::cl::opt<std::string> cl::InputFilename(llvm::cl::Positional,
                                             llvm::cl::desc("<input file>"),
                                             llvm::cl::Required,
                                             llvm::cl::cat(CLIOptionsCategory));

llvm::cl::opt<bool> cl::Run("run",
                            llvm::cl::desc("Run the generated executable after codegen"),
                            llvm::cl::cat(CLIOptionsCategory));

llvm::cl::opt<bool> cl::PrintAST("print-ast",
                                 llvm::cl::desc("Print the Abstract Syntax Tree"),
                                 llvm::cl::cat(CLIOptionsCategory));

llvm::cl::opt<bool> cl::EmitLLVM("emit-llvm",
                                 llvm::cl::desc("Emit LLVM IR"),
                                 llvm::cl::cat(CLIOptionsCategory));

llvm::cl::opt<bool> cl::DumpLLVM("dump-llvm",
                                 llvm::cl::desc("Dump LLVM IR"),
                                 llvm::cl::cat(CLIOptionsCategory));

llvm::cl::list<std::string> cl::RunArgs(llvm::cl::ConsumeAfter,
                                        llvm::cl::desc("<run args>..."),
                                        llvm::cl::cat(CLIOptionsCategory));


void print_version(llvm::raw_ostream &OS) {
    OS << "yo " << YO_VERSION << " (" << __DATE__ << ", " << __TIME__ << ")\n";
    OS << "- LLVM: " << YO_LLVM_VERSION << "\n";
    OS << "- Compiled with: " << COMPILER << "\n";
}

void cl::Init(int argc, const char *const *argv) {
    llvm::cl::SetVersionPrinter(&print_version);
    llvm::cl::HideUnrelatedOptions(CLIOptionsCategory);
    llvm::cl::ParseCommandLineOptions(argc, argv, "the yo programming language v" YO_VERSION "\n");
}
