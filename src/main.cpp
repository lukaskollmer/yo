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




void AddOptimizationPasses(llvm::legacy::PassManager &MPM, llvm::legacy::FunctionPassManager &FPM) {
    precondition(yo::cl::Optimize == true);
    
    llvm::PassRegistry &PR = *llvm::PassRegistry::getPassRegistry();
    llvm::initializeCore(PR);
    llvm::initializeIPO(PR);
    
    llvm::PassManagerBuilder PMBuilder;
    PMBuilder.OptLevel = yo::cl::Optimize ? 1 : 0;
    
    PMBuilder.Inliner = llvm::createFunctionInliningPass(PMBuilder.OptLevel, PMBuilder.SizeLevel, false);
    
    PMBuilder.populateFunctionPassManager(FPM);
    PMBuilder.populateModulePassManager(MPM);
}






// Returns EXIT_FAILURE when something went wrong, otherwise EXIT_SUCCESS
int EmitExecutable(std::unique_ptr<llvm::Module> Module, const std::string &Filename, std::string &ExecutableOutputPath) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();
    
    std::string Error;
    auto TargetTriple = llvm::sys::getDefaultTargetTriple();
    
    if (llvm::Triple(TargetTriple).isOSDarwin()) {
        Module->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);
    }
    
    auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);
    
    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialise the
    // TargetRegistry or we have a bogus target triple.
    if (!Target) {
        llvm::errs() << Error;
        return EXIT_FAILURE;
    }
    
    auto CPU = "generic";
    auto Features = "";
    
    llvm::TargetOptions opt;
    auto RM = llvm::Optional<llvm::Reloc::Model>();
    auto TargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);
    
    Module->setDataLayout(TargetMachine->createDataLayout());
    Module->setTargetTriple(TargetTriple);
    
    
    std::error_code EC;
    
    llvm::SmallVector<char, 256> cwd;
    EC = llvm::sys::fs::current_path(cwd);
    precondition(!EC);
    cwd.push_back('\0');
    
    
    //auto DebugDirPath = LKStringUtils_FormatIntoNewBuffer("%s/build/Debug/", cwd.data());
    std::string DebugDirPath(cwd.data());
    
    EC = llvm::sys::fs::create_directories(DebugDirPath);
    
    if (EC) {
        LKFatalError("Unable to create directory: %s", EC.message().c_str());
    }
    
    
    auto ObjectFilePath = yo::util::fmt_cstr("%s/%s.o",
                                             DebugDirPath.c_str(),
                                             yo::util::string::excludingFileExtension(Filename).c_str());
    llvm::raw_fd_ostream dest(ObjectFilePath, EC, llvm::sys::fs::CreationDisposition::CD_OpenAlways);
    
    if (EC) {
        llvm::outs() << "Could not open file: " << EC.message();
        return EXIT_FAILURE;
    }
    
    
    // !!!
    // https://stackoverflow.com/a/36973557/2513803
    
    
    llvm::legacy::PassManager PM;
    llvm::legacy::FunctionPassManager FPM(Module.get());
    
    PM.add(llvm::createTargetTransformInfoWrapperPass(TargetMachine->getTargetIRAnalysis()));
    FPM.add(llvm::createTargetTransformInfoWrapperPass(TargetMachine->getTargetIRAnalysis()));
    
    FPM.add(llvm::createVerifierPass());
    PM.add(llvm::createVerifierPass());
    
    if (yo::cl::Optimize) {
        AddOptimizationPasses(PM, FPM);
    }
    
    
    FPM.doInitialization();
    for (llvm::Function &F : *Module) {
        FPM.run(F);
    }
    FPM.doFinalization();
    
    
    if (yo::cl::DumpLLVM) {
        PM.add(llvm::createPrintModulePass(llvm::outs(), "Final IR:", true));
    }
    
    if (TargetMachine->addPassesToEmitFile(PM, dest, nullptr, llvm::TargetMachine::CGFT_ObjectFile)) {
        llvm::errs() << "TargetMachine can't emit a file of this type";
        return EXIT_FAILURE;
    }
    
    PM.run(*Module);
    dest.flush();
    
    
    
    
    auto clangPath = llvm::sys::findProgramByName("clang");
    const auto ExecutablePath = yo::util::fmt_cstr("%s/%s", DebugDirPath.c_str(),
                                                   yo::util::string::excludingFileExtension(Filename).c_str());
    ExecutableOutputPath = ExecutablePath;
    
    if (EC) {
        llvm::outs() << "Error creating output directory: " << EC.message();
        return EXIT_FAILURE;
    }
    auto cmd = yo::util::fmt_cstr("%s %s -o %s -lc",
                                  clangPath->c_str(),
                                  ObjectFilePath,
                                  ExecutablePath);
    
    if (system(cmd) != 0) {
        return EXIT_FAILURE;
    }
    
    return EXIT_SUCCESS;
}



int RunExecutable(const std::string &ExecutablePath, const char *const *envp) {
    auto argc = yo::cl::RunArgs.size();
    auto argv = static_cast<char **>(calloc(argc + 2, sizeof(char *))); // +2 bc the first element is the executable path and the array is null terminated
    argv[0] = strdup(ExecutablePath.c_str());
    
    for (auto I = 0; I < argc; I++) {
        argv[I + 1] = strdup(yo::cl::RunArgs[I].c_str());
    }
    llvm::llvm_shutdown();
    
    auto Status = execve(argv[0], (char *const *) argv, (char *const *) envp);
    perror("execve failed");
    return Status;
}





int main(int argc, const char * argv[], const char *const *envp) {
    yo::cl::Init(argc, argv);
    assert_implication(yo::cl::RunArgs.size() > 0, yo::cl::Run);
    
    std::string Filename = yo::cl::InputFilename;
    auto Ast = yo::parser::Parser().Parse(Filename);
    
    Filename = yo::util::string::lastPathCompotent(Filename);
    
    if (yo::cl::PrintAST) {
        std::cout << yo::ast::Description(Ast) << std::endl;
        return EXIT_SUCCESS;
    }
    
    
    yo::irgen::IRGenerator Codegen("main");
    Codegen.Codegen(Ast);
    
    auto M = Codegen.GetModule();
    
    if (yo::cl::EmitLLVM) {
        std::error_code EC;
        auto OutputFile = Filename.append(".ll");
        llvm::raw_fd_ostream OS(OutputFile, EC);
        M->print(OS, nullptr, true, true);
        return EXIT_SUCCESS;
    }
    
    std::string ExecutablePath;
    
    if (auto Status = EmitExecutable(std::move(M), Filename, ExecutablePath); Status != EXIT_SUCCESS) {
        return Status;
    }
    
    if (yo::cl::Run) {
        // Only returns if execve failed
        return RunExecutable(ExecutablePath, envp);
    }
    
    return EXIT_SUCCESS;
}

