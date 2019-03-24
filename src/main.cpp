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
#include "JIT.h"

#include "AST.h"
#include "Parser.h"

#include <fstream>
#include <sstream>

#include "Mangling.h"


#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/Program.h"





// Returns EXIT_FAILURE when something went wrong, otherwise EXIT_SUCCESS
int EmitExecutable(std::unique_ptr<llvm::Module> Module, std::string &Filename) {
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
    auto ExecutablePath = LKStringUtils_FormatIntoNewBuffer("%s/%s",
                                                            DebugDirPath.c_str(),
                                                            util::string::excludingFileExtension(Filename).c_str());
    
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
    
    std::cout << "Executable written to " << ExecutablePath << std::endl;
    
    return EXIT_SUCCESS;
}








int main(int argc, const char * argv[], const char *const *envp) {
    cl::Init(argc, argv);
    
    std::string Filename = cl::InputFilename;
    
    Parser P(cl::StdlibPath);
    auto Ast = P.Parse(Filename);
    
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
    
//    JIT JIT(std::move(M));
//
//    auto retval = JIT.RunMain(envp);
//    std::cout << "Retval: " << retval << std::endl;
    
    return EmitExecutable(std::move(M), Filename);
    
    return EXIT_SUCCESS;
}

