//
//  main.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-23.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "util.h"
#include "CommandLine.h"
#include "Parser.h"
#include "AST.h"
#include "IRGen.h"
#include "AST.h"
#include "Parser.h"
#include "Mangling.h"

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




void addOptimizationPasses(llvm::legacy::PassManager &MPM, llvm::legacy::FunctionPassManager &FPM) {
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






// Returns EXIT_FAILURE when something went wrong, otherwise EXIT_SUCCESS
int emitExecutable(std::unique_ptr<llvm::Module> module, const std::string &filename, std::string &executableOutputPath) {
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
        return EXIT_FAILURE;
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
    
    
    //auto DebugDirPath = LKStringUtils_FormatIntoNewBuffer("%s/build/Debug/", cwd.data());
    std::string debugDirPath(cwd.data());
    
    EC = llvm::sys::fs::create_directories(debugDirPath);
    
    if (EC) {
        LKFatalError("Unable to create directory: %s", EC.message().c_str());
    }
    
    
    auto objectFilePath = yo::util::fmt_cstr("%s/%s.o",
                                             debugDirPath.c_str(),
                                             yo::util::string::excludingFileExtension(filename).c_str());
    llvm::raw_fd_ostream dest(objectFilePath, EC, llvm::sys::fs::CreationDisposition::CD_OpenAlways);
    
    if (EC) {
        llvm::outs() << "Could not open file: " << EC.message();
        return EXIT_FAILURE;
    }
    
    
    // !!!
    // https://stackoverflow.com/a/36973557/2513803
    
    
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
    
    if (targetMachine->addPassesToEmitFile(PM, dest, nullptr, llvm::TargetMachine::CGFT_ObjectFile)) {
        llvm::errs() << "TargetMachine can't emit a file of this type";
        return EXIT_FAILURE;
    }
    
    PM.run(*module);
    dest.flush();
    
    
    
    
    auto clangPath = llvm::sys::findProgramByName("clang");
    const auto executablePath = yo::util::fmt_cstr("%s/%s", debugDirPath.c_str(),
                                                   yo::util::string::excludingFileExtension(filename).c_str());
    executableOutputPath = executablePath;
    
    if (EC) {
        llvm::outs() << "Error creating output directory: " << EC.message();
        return EXIT_FAILURE;
    }
    auto cmd = yo::util::fmt_cstr("%s %s -o %s -lc",
                                  clangPath->c_str(),
                                  objectFilePath,
                                  executablePath);
    
    if (system(cmd) != 0) {
        return EXIT_FAILURE;
    }
    
    return EXIT_SUCCESS;
}



int runExecutable(const std::string &executablePath, const char *const *envp) {
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
    
    yo::parser::Parser parser;
    
    if (!yo::cl::opts::stdlibRoot().empty()) {
        parser.setCustomStdlibRoot(yo::cl::opts::stdlibRoot());
    }
    
    std::string filename = yo::cl::opts::inputFilename();
    auto ast = parser.parse(filename);
    
    filename = yo::util::string::lastPathCompotent(filename);
    
    if (yo::cl::opts::printAST()) {
        std::cout << yo::ast::description(ast) << std::endl;
        return EXIT_SUCCESS;
    }
    
    
    yo::irgen::IRGenerator codegen("main");
    codegen.enableARC = yo::cl::opts::arc();
    codegen.codegen(ast);
    
    auto M = codegen.getModule();
    
    if (yo::cl::opts::emitLLVM()) {
        std::error_code EC;
        auto outputFile = filename.append(".ll");
        llvm::raw_fd_ostream OS(outputFile, EC);
        M->print(OS, nullptr, true, true);
        return EXIT_SUCCESS;
    }
    
    std::string executablePath;
    
    if (auto status = emitExecutable(std::move(M), filename, executablePath); status != EXIT_SUCCESS) {
        return status;
    }
    
    if (yo::cl::opts::run()) {
        // Only returns if execve failed
        return runExecutable(executablePath, envp);
    }
    
    return EXIT_SUCCESS;
}

