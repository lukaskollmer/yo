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


static llvm::cl::opt<std::string> inputFile(llvm::cl::Positional, llvm::cl::desc("<input file>"),
                                            llvm::cl::Required, llvm::cl::cat(CLIOptionsCategory));


static llvm::cl::opt<bool> pygmentize("pygmentize", llvm::cl::desc("Lex input, then print pygmentized HTML to stdout"),
                                      llvm::cl::cat(CLIOptionsCategory));


static llvm::cl::opt<bool> printAST("print-ast", llvm::cl::desc("Print the Abstract Syntax Tree"),
                                    llvm::cl::cat(CLIOptionsCategory));


static llvm::cl::opt<bool> emitLLVM("emit-llvm", llvm::cl::desc("Emit LLVM IR"),
                                    llvm::cl::cat(CLIOptionsCategory));


static llvm::cl::opt<bool> dumpLLVM("dump-llvm", llvm::cl::desc("Dump LLVM IR"),
                                    llvm::cl::cat(CLIOptionsCategory));


static llvm::cl::opt<bool> optimize("O", llvm::cl::desc("Enable Optimizations"),
                                    llvm::cl::cat(CLIOptionsCategory));


static llvm::cl::opt<bool> dumpLLVMPreOpt("dump-llvm-pre-opt", llvm::cl::desc("Dump LLVM IR prior to running optimizations"),
                                          llvm::cl::cat(CLIOptionsCategory));


static llvm::cl::opt<std::string> stdlibRoot("stdlib-root", llvm::cl::desc("Load stdlib modules from <path>, instead of using the bundled ones"),
                                             llvm::cl::value_desc("path"), llvm::cl::cat(CLIOptionsCategory));


static llvm::cl::opt<bool> arc("arc", llvm::cl::desc("[internal] enable arc"), llvm::cl::cat(CLIOptionsCategory));


static llvm::cl::opt<bool> emitDebugMetadata("g", llvm::cl::desc("Emit debug metadata"), llvm::cl::cat(CLIOptionsCategory));



static llvm::cl::opt<std::string> outputPath("o", llvm::cl::desc("Write output to <file>"), llvm::cl::value_desc("file"),
                                             llvm::cl::cat(CLIOptionsCategory));


static llvm::cl::opt<bool> run("run", llvm::cl::value_desc("run_val"), llvm::cl::desc("Run the generated executable after codegen"),
                               llvm::cl::cat(CLIOptionsCategory));


static llvm::cl::list<std::string> runArgs(llvm::cl::ConsumeAfter, /*llvm::cl::desc("..."),*/ llvm::cl::cat(CLIOptionsCategory));

NS_END


bool yo::cl::opts::pygmentize() {
    return internal::pygmentize;
}

bool yo::cl::opts::run() {
    return internal::run;
}

const std::vector<std::string>& yo::cl::opts::runArgs() {
    return internal::runArgs;
}

const std::string& yo::cl::opts::inputFile() {
    return internal::inputFile;
}

const std::string& yo::cl::opts::outputFile() {
    return internal::outputPath;
}

const std::string& yo::cl::opts::stdlibRoot() {
    return internal::stdlibRoot;
}

bool yo::cl::opts::printAST() {
    return internal::printAST;
}

bool yo::cl::opts::emitLLVM() {
    return internal::emitLLVM;
}

bool yo::cl::opts::dumpLLVM() {
    return internal::dumpLLVM;
}

bool yo::cl::opts::dumpLLVMPreOpt() {
    return internal::dumpLLVMPreOpt;
}

bool yo::cl::opts::optimize() {
    return internal::optimize;
}

bool yo::cl::opts::arc() {
    return internal::arc;
}

bool yo::cl::opts::emitDebugMetadata() {
    return internal::emitDebugMetadata;
}



void print_version(llvm::raw_ostream &OS) {
    OS << "yo " << YO_VERSION << "\n"; // << " (" << __DATE__ << ", " << __TIME__ << ")\n";
    OS << "- LLVM: " << YO_LLVM_VERSION << "\n";
    OS << "- Compiled with: " << COMPILER << "\n";
}

void yo::cl::init(int argc, const char *const *argv) {
    llvm::cl::SetVersionPrinter(&print_version);
    llvm::cl::HideUnrelatedOptions(internal::CLIOptionsCategory);
    llvm::cl::ParseCommandLineOptions(argc, argv, "the yo programming language v" YO_VERSION "\n");
}
