//
//  main.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-23.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include <iostream>
#include <memory>

#include "util.h"
#include "CommandLine.h"
#include "Parser.h"
#include "AST.h"
#include "IRGen.h"

#include "AST.h"
#include "Parser.h"

#include <fstream>
#include <sstream>
#include <unistd.h>

#include "Mangling.h"


#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/Program.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Support/FileSystem.h"





// Returns EXIT_FAILURE when something went wrong, otherwise EXIT_SUCCESS
int EmitExecutable(std::unique_ptr<llvm::Module> Module, const std::string &Filename, std::string &ExecutableOutputPath) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();
    
    std::string Error;
    auto TargetTriple = llvm::sys::getDefaultTargetTriple();
    
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
    
    
    auto ObjectFilePath = LKStringUtils_FormatIntoNewBuffer("%s/%s",
                                                            DebugDirPath.c_str(),
                                                            util::string::excludingFileExtension(Filename).c_str());
    llvm::raw_fd_ostream dest(ObjectFilePath, EC, llvm::sys::fs::F_None);
    
    if (EC) {
        llvm::outs() << "Could not open file: " << EC.message();
        return EXIT_FAILURE;
    }
    
    llvm::legacy::PassManager pass;
    auto FileType = llvm::TargetMachine::CGFT_ObjectFile;
    
    if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
        llvm::errs() << "TargetMachine can't emit a file of this type";
        return EXIT_FAILURE;
    }
    
    pass.run(*Module);
    dest.flush();
    
    auto clangPath = llvm::sys::findProgramByName("clang");
    const auto ExecutablePath = LKStringUtils_FormatIntoNewBuffer("%s/%s",
                                                                  DebugDirPath.c_str(),
                                                                  util::string::excludingFileExtension(Filename).c_str());
    ExecutableOutputPath = ExecutablePath;
    
    if (EC) {
        llvm::outs() << "Error creating output directory: " << EC.message();
        return EXIT_FAILURE;
    }
    auto cmd = LKStringUtils_FormatIntoNewBuffer("%s %s -o %s -lSystem",
                                                 clangPath->c_str(),
                                                 ObjectFilePath,
                                                 ExecutablePath);
    
    if (system(cmd) != 0) {
        return EXIT_FAILURE;
    }
    
    return EXIT_SUCCESS;
}




int RunExecutable(const std::string &ExecutablePath, const char *const *envp) {
    auto argc = cl::RunArgs.size();
    auto argv = static_cast<char **>(calloc(argc + 2, sizeof(char *))); // +2 bc the first element is the executable path and the array is null terminated
    argv[0] = strdup(ExecutablePath.c_str());
    
    for (auto I = 0; I < argc; I++) {
        argv[I + 1] = strdup(cl::RunArgs[I].c_str());
    }
    llvm::llvm_shutdown();
    
    auto Status = execve(argv[0], (char *const *) argv, (char *const *) envp);
    perror("execve failed");
    return Status;
}





int main(int argc, const char * argv[], const char *const *envp) {
    cl::Init(argc, argv);
    
    std::string Filename = cl::InputFilename;
    
    auto Ast = Parser().Parse(Filename);
    
    Filename = util::string::lastPathCompotent(Filename);
    
    if (cl::PrintAST) {
        std::cout << ast::Description(Ast) << std::endl;
        return EXIT_SUCCESS;
    }
    
    irgen::IRGenerator Codegen("main");
    Codegen.Codegen(Ast);
    
    auto M = Codegen.GetModule();
    
    if (cl::EmitLLVM) {
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
    
    if (cl::Run) {
        // Only returns if execve failed
        return RunExecutable(ExecutablePath, envp);
    }
    
    return EXIT_SUCCESS;
}

