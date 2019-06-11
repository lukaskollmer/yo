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
#include "llvm/Support/CommandLine.h"
#include "util.h"


using namespace yo::cl;


NS_START(yo::cl::internal)

// These are in a separate namespace to avoid name lookup ambiguity in the getter functions

static llvm::cl::OptionCategory CLIOptionsCategory("General Options");

static llvm::cl::opt<std::string> InputFilename(llvm::cl::Positional,
                                                   llvm::cl::desc("<input file>"),
                                                   llvm::cl::Required,
                                                   llvm::cl::cat(CLIOptionsCategory));

static llvm::cl::opt<bool> Run("run", llvm::cl::desc("Run the generated executable after codegen"),
                                  llvm::cl::cat(CLIOptionsCategory));

static llvm::cl::opt<bool> PrintAST("print-ast", llvm::cl::desc("Print the Abstract Syntax Tree"),
                                       llvm::cl::cat(CLIOptionsCategory));

static llvm::cl::opt<bool> EmitLLVM("emit-llvm", llvm::cl::desc("Emit LLVM IR"),
                                    llvm::cl::cat(CLIOptionsCategory));

static llvm::cl::opt<bool> DumpLLVM("dump-llvm", llvm::cl::desc("Dump LLVM IR"),
                                    llvm::cl::cat(CLIOptionsCategory));

static llvm::cl::list<std::string> RunArgs(llvm::cl::ConsumeAfter, llvm::cl::desc("<run args>..."),
                                           llvm::cl::cat(CLIOptionsCategory));

static llvm::cl::opt<bool> Optimize("O", llvm::cl::desc("Enable Optimizations"),
                                    llvm::cl::cat(CLIOptionsCategory));

static llvm::cl::opt<bool> DumpLLVMPreOpt("dump-llvm-pre-opt",
                                          llvm::cl::desc("Dump LLVM IR prior to running optimizations"),
                                          llvm::cl::cat(CLIOptionsCategory));

static llvm::cl::opt<std::string> StdlibRoot("stdlib-root",
                                             llvm::cl::desc("Load stdlib modules from <path>, instead of using the bundled ones"),
                                             llvm::cl::value_desc("path"),
                                             llvm::cl::cat(CLIOptionsCategory));

NS_END


bool yo::cl::opts::Run() {
    return internal::Run;
}

const std::vector<std::string>& yo::cl::opts::RunArgs() {
    return internal::RunArgs;
}

const std::string& yo::cl::opts::InputFilename() {
    return internal::InputFilename;
}

//const std::string& yo::cl::opts::OutputFilename() {
//    return yo::cl::OutputFilename;
//}

const std::string& yo::cl::opts::StdlibRoot() {
    return internal::StdlibRoot;
}

bool yo::cl::opts::PrintAST() {
    return internal::PrintAST;
}

bool yo::cl::opts::EmitLLVM() {
    return internal::EmitLLVM;
}

bool yo::cl::opts::DumpLLVM() {
    return internal::DumpLLVM;
}

bool yo::cl::opts::DumpLLVMPreOpt() {
    return internal::DumpLLVMPreOpt;
}

bool yo::cl::opts::Optimize() {
    return internal::Optimize;
}



void print_version(llvm::raw_ostream &OS) {
    OS << "yo " << YO_VERSION << " (" << __DATE__ << ", " << __TIME__ << ")\n";
    OS << "- LLVM: " << YO_LLVM_VERSION << "\n";
    OS << "- Compiled with: " << COMPILER << "\n";
}

void yo::cl::Init(int argc, const char *const *argv) {
    llvm::cl::SetVersionPrinter(&print_version);
    llvm::cl::HideUnrelatedOptions(internal::CLIOptionsCategory);
    llvm::cl::ParseCommandLineOptions(argc, argv, "the yo programming language v" YO_VERSION "\n");
}
