//
//  main.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-23.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "util.h"
#include "CommandLine.h"
#include "Lexer.h"
#include "Parser.h"
#include "AST.h"
#include "IRGen.h"
#include "Pygmentize.h"

#include <fstream>
#include <sstream>
#include <unistd.h>
#include <iostream>
#include <memory>

#include "llvm/Target/TargetOptions.h"
#include "llvm/Target/TargetMachine.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/Program.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRPrintingPasses.h"

#include "llvm/Analysis/TargetTransformInfo.h"

#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"




void addOptimizationPasses(llvm::legacy::PassManager& MPM, llvm::legacy::FunctionPassManager& FPM) {
    llvm::PassRegistry &PR = *llvm::PassRegistry::getPassRegistry();
    llvm::initializeCore(PR);
    llvm::initializeIPO(PR);
    //llvm::initializeRegToMemPass(<#PassRegistry &#>)
    llvm::initializeSimpleInlinerPass(PR);
    
    llvm::PassManagerBuilder PMBuilder;
    PMBuilder.OptLevel = yo::cl::opts::optimize() ? 1 : 0;
    PMBuilder.SizeLevel = 0;
    
    if (PMBuilder.OptLevel > 1) {
        PMBuilder.Inliner = llvm::createFunctionInliningPass(PMBuilder.OptLevel, PMBuilder.SizeLevel, false);
    } else {
        PMBuilder.Inliner = llvm::createAlwaysInlinerLegacyPass();
    }

    PMBuilder.populateFunctionPassManager(FPM);
    PMBuilder.populateModulePassManager(MPM);
}






// returns true on success
bool emitExecutable(std::unique_ptr<llvm::Module> module, const std::string& filename, std::string& executableOutputPath) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();
    
    std::string error;
    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    
    if (llvm::Triple(targetTriple).isOSDarwin()) {
        module->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);
    }
    
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
    
    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialise the
    // TargetRegistry or we have a bogus target triple.
    if (!target) {
        llvm::errs() << error;
        return false;
    }
    
    auto CPU = "generic";
    auto features = "";
    
    llvm::TargetOptions opt;
    auto RM = llvm::Optional<llvm::Reloc::Model>();
    auto targetMachine = target->createTargetMachine(targetTriple, CPU, features, opt, RM);
    
    module->setDataLayout(targetMachine->createDataLayout());
    module->setTargetTriple(targetTriple);
    
    
    std::error_code EC;
    
    llvm::SmallVector<char, 256> cwd;
    EC = llvm::sys::fs::current_path(cwd);
    LKAssert(!EC);
    cwd.push_back('\0');
    
    std::string debugDirPath(cwd.data());
    EC = llvm::sys::fs::create_directories(debugDirPath);
    
    if (EC) {
        LKFatalError("Unable to create directory: %s", EC.message().c_str());
    }
    
    
    // Create and run passes
    
    llvm::legacy::PassManager PM;
    llvm::legacy::FunctionPassManager FPM(module.get());
    
    PM.add(llvm::createTargetTransformInfoWrapperPass(targetMachine->getTargetIRAnalysis()));
    FPM.add(llvm::createTargetTransformInfoWrapperPass(targetMachine->getTargetIRAnalysis()));
    
    PM.add(llvm::createVerifierPass());
    
    if (yo::cl::opts::dumpLLVMPreOpt()) {
        PM.add(llvm::createPrintModulePass(llvm::outs(), "Pre-Optimized IR:", true));
    }
    addOptimizationPasses(PM, FPM);
    
    FPM.doInitialization();
    for (llvm::Function &F : *module) {
        FPM.run(F);
    }
    FPM.doFinalization();
    
    if (yo::cl::opts::dumpLLVM()) {
        std::string banner = !yo::cl::opts::optimize() ? "Final IR:" : "Final IR (Optimized):";
        PM.add(llvm::createPrintModulePass(llvm::outs(), banner, true));
    }
    
    
    // Emit object file
    
    auto objectFilePath = "a.o";
    llvm::raw_fd_ostream dest(objectFilePath, EC, llvm::sys::fs::CreationDisposition::CD_OpenAlways);
    
    if (EC) {
        llvm::outs() << "Could not open file: " << EC.message();
        return false;
    }
    
    if (targetMachine->addPassesToEmitFile(PM, dest, nullptr, llvm::TargetMachine::CGFT_ObjectFile)) {
        llvm::errs() << "TargetMachine can't emit a file of this type";
        return false;
    }
    
    PM.run(*module);
    dest.flush();
    dest.close();
    
    
    // Link object file into executable
    
    executableOutputPath = "a.out";
    
    // TODO use lld if available? would that even matter?
    const auto linkerName = "ld";
    auto ld = llvm::sys::findProgramByName(linkerName);
    if (auto EC = ld.getError()) {
        LKFatalError("unable to find '%s': %s", linkerName, EC.message().c_str());
    }
    
    std::vector<llvm::StringRef> ld_argv = {
        ld.get(),
        objectFilePath,
        "-lc",
        "-o", executableOutputPath
    };
    
    auto res = llvm::sys::ExecuteAndWait(ld_argv[0], ld_argv);
    LKAssert(res == 0 && "ld returned non-zero exit code");
    
    return true;
}



int runExecutable(const std::string& executablePath, const char *const *envp) {
    auto argc = yo::cl::opts::runArgs().size();
    auto argv = static_cast<char **>(calloc(argc + 2, sizeof(char *))); // +2 bc the first element is the executable path and the array is null terminated
    argv[0] = strdup(executablePath.c_str());
    
    for (size_t i = 0; i < argc; i++) {
        argv[i + 1] = strdup(yo::cl::opts::runArgs()[i].c_str());
    }
    llvm::llvm_shutdown();
    
    auto status = execve(argv[0], (char *const *) argv, (char *const *) envp);
    perror("execve failed");
    return status;
}





int main(int argc, const char * argv[], const char *const *envp) {
    yo::cl::init(argc, argv);
    LKAssertImplication(yo::cl::opts::runArgs().size() > 0, yo::cl::opts::run());
    LKAssert((yo::cl::opts::run() && yo::cl::opts::pygmentize()) == false); // TODO implement more sophisticated mutual exclusion checking
    
    std::string inputFile = yo::cl::opts::inputFile();
    std::string inputFilename = yo::util::fs::path_utils::getFilename(inputFile);
    
    if (yo::cl::opts::pygmentize()) {
        auto tokens = yo::parser::Lexer().lex(yo::util::fs::read_file(inputFile), inputFilename, true);
        std::cout << yo::lex::pygmentize(tokens) << std::endl;
        return EXIT_SUCCESS;
    }
    
    
    yo::parser::Parser parser;
    
    if (!yo::cl::opts::stdlibRoot().empty()) {
        parser.setCustomStdlibRoot(yo::cl::opts::stdlibRoot());
    }
    
    auto ast = parser.parse(inputFile);
    
    //filename = yo::util::string::lastPathCompotent(filename);
    
    if (yo::cl::opts::printAST()) {
        std::cout << yo::ast::description(ast) << std::endl;
        return EXIT_SUCCESS;
    }
    
    yo::irgen::IRGenOptions options{
        yo::cl::opts::arc(),
        yo::cl::opts::emitDebugMetadata()
    };
    
    yo::irgen::IRGenerator codegen("main", options);
    codegen.codegen(ast);
    
    auto M = codegen.getModule();
    
    if (yo::cl::opts::emitLLVM()) {
        std::error_code EC;
        auto outputFile = std::string(inputFilename).append(".ll");
        llvm::raw_fd_ostream OS(outputFile, EC);
        M->print(OS, nullptr, true, true);
        return EXIT_SUCCESS;
    }
    
    std::string executablePath;
    
    if (!emitExecutable(std::move(M), inputFilename, executablePath)) {
        return EXIT_FAILURE;
    }
    
    if (yo::cl::opts::run()) {
        // Only returns if execve failed
        runExecutable(executablePath, envp);
        return EXIT_FAILURE;
    }
    
    return EXIT_SUCCESS;
}

