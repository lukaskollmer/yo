//
//  CommandLine.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-10-09.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "CommandLine.h"
#include "Version.h"

#include "llvm/Support/CommandLine.h"


using namespace yo;
using namespace yo::cl;


static int _argc = 0;
static const char **_argv = nullptr;
static Options options;
static llvm::cl::OptionCategory CLIOptionsCategory("General Options");



#define CLI_OPT(type, name, flag, _desc, ...) static llvm::cl::opt<type, true> name(flag, llvm::cl::desc(_desc), llvm::cl::cat(CLIOptionsCategory), llvm::cl::location(options.name), ## __VA_ARGS__);

static llvm::cl::opt<std::string, true> inputFile(llvm::cl::Positional, llvm::cl::desc("<input file>"), llvm::cl::Required, llvm::cl::cat(CLIOptionsCategory), llvm::cl::location(options.inputFile));

static llvm::cl::list<std::string> runArgs("run-args", llvm::cl::desc("Argv to be used when executing the produced binary. Implies `-run`"),
llvm::cl::CommaSeparated, llvm::cl::cat(CLIOptionsCategory));

CLI_OPT(bool, dumpLLVM, "dump-llvm", "Dump LLVM IR")
CLI_OPT(bool, dumpLLVMPreOpt, "dump-llvm-pre-opt", "Dump LLVM IR prior to running optimizations")
CLI_OPT(bool, emitDebugMetadata, "g", "Emit debug metadata")
CLI_OPT(bool, farc, "farc", "(experimental) enable ARC")
CLI_OPT(bool, fnoInline, "fno-inline", "Disable all function inlining")
CLI_OPT(bool, fzeroInitialize, "fzero-initialize", "allow uninitialized variables and zero-initialize them")
CLI_OPT(bool, int_trapOnFatalError, "int_trap-on-fatal-error", "", llvm::cl::Hidden)
CLI_OPT(bool, optimize, "O", "Enable Optimizations")
CLI_OPT(bool, printAST, "print-ast", "Print the Abstract Syntax Tree")
CLI_OPT(bool, pygmentize, "pygmentize", "Lex input, then print pygmentized HTML to stdout")
CLI_OPT(bool, run, "run", "Run the generated executable after codegen. Implies `--emit bin`")
CLI_OPT(std::string, stdlibRoot, "stdlib-root", "Load stdlib modules from <path>, instead of using the bundled ones", llvm::cl::value_desc("path"))

static llvm::cl::list<OutputFileType> emit("emit", llvm::cl::desc("Output format(s)"),
                                           llvm::cl::values(clEnumValN(OutputFileType::Assembly, "asm", "Assembly"),
                                                            clEnumValN(OutputFileType::LLVM_IR, "llvm-ir", "LLVM IR"),
                                                            clEnumValN(OutputFileType::LLVM_BC, "llvm-bc", "LLVM Bitcode"),
                                                            clEnumValN(OutputFileType::Binary, "bin", "Binary"),
                                                            clEnumValN(OutputFileType::ObjectFile, "obj", "Object File"),
                                                            clEnumValN(OutputFileType::None, "none", "None")),
                                           llvm::cl::cat(CLIOptionsCategory));



bool LKCompilerInternalOptionSigabrtOnFatalError() {
    return options.int_trapOnFatalError;
}


bool yo::cl::hasRawOption(const std::string &str) {
    LKAssert(_argv && "cl::init not called");
    
    for (int i = 0; i < _argc; i++) {
        if (str == _argv[i]) return true;
    }
    return false;
}


Options& yo::cl::get_options_mut() { return options; }
const Options& yo::cl::get_options() { return options; }


void version_printer(llvm::raw_ostream &OS) {
    OS << "yo " << YO_VERSION << "\n"; // << " (" << __DATE__ << ", " << __TIME__ << ")\n";
    OS << "- LLVM: " << YO_LLVM_VERSION << "\n";
    OS << "- Compiled with: " << COMPILER << "\n";
}

void yo::cl::init(int argc, const char * argv[]) {
    _argc = argc; _argv = argv;
    
    if (hasRawOption("--print-all-options")) {
        for (int i = 0; i < argc; i++) {
            printf("[%i] %s\n", i, argv[i]);
        }
    }
    
    llvm::cl::SetVersionPrinter(&version_printer);
    llvm::cl::HideUnrelatedOptions(CLIOptionsCategory);
    llvm::cl::ParseCommandLineOptions(argc, argv, "The Yo Programming Language v" YO_VERSION "\n");
    
    llvm::cl::PrintOptionValues();
    
    // llvm::cl::list does not seem to support external storage?
    options.emit = emit;
    options.runArgs = runArgs;
}


