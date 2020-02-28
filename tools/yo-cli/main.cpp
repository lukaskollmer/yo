//
//  main.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-23.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Version.h"
#include "yo/Driver.h"
#include "yo/util.h"
#include "yo/util_llvm.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/CommandLine.h"

#include <string>
#include <cstdlib>
#include <unistd.h>



using namespace yo;
using yo::driver::OutputFileType;

static int _argc = 0;
static const char **_argv = nullptr;

namespace cl_options {

static llvm::cl::OptionCategory CLIOptionCategory("General Options");

#define CLI_OPT(type, name, flag, _desc, ...)   \
static llvm::cl::opt<type> name(                \
    flag,                                       \
    llvm::cl::desc(_desc),                      \
    llvm::cl::cat(CLIOptionCategory),           \
    ## __VA_ARGS__);



CLI_OPT(bool, dumpLLVM, "dump-llvm", "Dump LLVM IR to stdout")
CLI_OPT(bool, dumpLLVMPreOpt, "dump-llvm-pre-opt", "Dump LLVM IR to stdout, prior to running optimizations")
CLI_OPT(bool, dumpAST, "dump-ast", "Print the Abstract Syntax Tree to stdout")
CLI_OPT(bool, emitDebugMetadata, "g", "Emit debug metadata")
CLI_OPT(bool, fnoInline, "fno-inline", "Disable all function inlining")
CLI_OPT(bool, fzeroInitialize, "fzero-initialize", "Allow uninitialized variables and zero-initialize them")
CLI_OPT(bool, int_trapOnFatalError, "int_trap-on-fatal-error", "", llvm::cl::Hidden)
CLI_OPT(bool, optimize, "O", "Enable optimizations")
CLI_OPT(bool, run, "run", "Run the generated executable after codegen. Implies `--emit bin`")
CLI_OPT(std::string, stdlibRoot, "stdlib-root", "Load stdlib modules from <path>, instead of using the bundled ones", llvm::cl::value_desc("path"))

static llvm::cl::opt<std::string> inputFile(llvm::cl::Positional,
                                            llvm::cl::desc("<input file>"),
                                            llvm::cl::Required,
                                            llvm::cl::cat(CLIOptionCategory));


static llvm::cl::list<OutputFileType> outputFileTypes("emit", llvm::cl::desc("Output format(s)"),
                                                      llvm::cl::values(clEnumValN(OutputFileType::Assembly, "asm", "Assembly"),
                                                                       clEnumValN(OutputFileType::LLVM_IR, "llvm-ir", "LLVM IR"),
                                                                       clEnumValN(OutputFileType::LLVM_BC, "llvm-bc", "LLVM Bitcode"),
                                                                       clEnumValN(OutputFileType::Binary, "bin", "Binary"),
                                                                       clEnumValN(OutputFileType::ObjectFile, "obj", "Object File")),
                                                      llvm::cl::cat(CLIOptionCategory));

static llvm::cl::list<std::string> runArgs("run-args",
                                           llvm::cl::desc("Argv to be used when executing the produced binary. Implies `-run`"),
                                           llvm::cl::CommaSeparated,
                                           llvm::cl::cat(CLIOptionCategory));

} // namespace cl_options


bool hasRawOption(const std::string &str) {
    LKAssert(_argc && _argv && "_argc and _argv not initialized");
    
    for (int i = 1; i < _argc; i++) {
        if (str == _argv[i]) return true;
    }
    return false;
}


void version_printer(llvm::raw_ostream &OS) {
    OS << "yo " << YO_VERSION << "\n";
    OS << "- LLVM: " << YO_LLVM_VERSION << "\n";
    OS << "- Compiled with: " << COMPILER << "\n";
}



int main(int argc, const char * argv[], const char *const *envp) {
    _argc = argc; _argv = argv;
    
    if (hasRawOption("--print-all-options")) {
        for (int i = 0; i < argc; i++) {
            printf("[%i]: %s\n", i, argv[i]);
        }
    }
    
    llvm::cl::SetVersionPrinter(&version_printer);
    llvm::cl::HideUnrelatedOptions(cl_options::CLIOptionCategory);
    llvm::cl::ParseCommandLineOptions(argc, argv, "The Yo Programming Language v" YO_VERSION "\n");
    
    llvm::cl::PrintOptionValues();
    
    
    driver::Options options;
    options.inputFile = cl_options::inputFile;
    options.stdlibRoot = cl_options::stdlibRoot;
    options.optimize = cl_options::optimize;
    options.fnoInline = cl_options::fnoInline;
    options.fzeroInitialize = cl_options::fzeroInitialize;
    options.dumpLLVM = cl_options::dumpLLVM;
    options.dumpLLVMPreOpt = cl_options::dumpLLVMPreOpt;
    options.dumpAST = cl_options::dumpAST;
    options.emitDebugMetadata = cl_options::emitDebugMetadata;
    options.int_trapOnFatalError = cl_options::int_trapOnFatalError;
    
    llvm::SmallString<255> cwd;
    if (llvm::sys::fs::current_path(cwd)) {
        LKFatalError("?");
    }
    
    for (OutputFileType type : cl_options::outputFileTypes) {
        options.outputFileTypes.insert(type);
    }
    
    LKAssertImplication(!cl_options::runArgs.empty(), cl_options::run);
    
    if (cl_options::run) {
        options.outputFileTypes.insert(OutputFileType::Binary);
    }
    
    if (!driver::run(options)) {
        return EXIT_FAILURE;
    }
    
    if (cl_options::run) {
        auto executablePath = util::fmt::format("{}/a.out", cwd);
        const auto &runArgs = cl_options::runArgs;
        
        int argc = runArgs.size();
        auto argv = static_cast<char **>(calloc(argc + 2, sizeof(char *))); // +2 bc the first element is the executable path and the array is null terminated
        argv[0] = strdup(executablePath.c_str());

        for (size_t i = 0; i < argc; i++) {
            argv[i + 1] = strdup(runArgs[i].c_str());
        }
        llvm::llvm_shutdown();

        auto status = execve(argv[0], (char *const *) argv, (char *const *) envp);
        perror("execve failed");
        return status;
    }
}

