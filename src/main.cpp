//
//  main.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-23.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "util.h"
#include "Lexer.h"
#include "AST.h"
#include "Parser.h"
#include "IRGen.h"
#include "Pygmentize.h"
#include "Version.h"
#include "CommandLine.h"

#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/CommandLine.h"

#include <fstream>
#include <sstream>
#include <unistd.h>
#include <iostream>
#include <memory>


using namespace yo;





#pragma mark - Optimizations


void addOptimizationPasses(llvm::legacy::PassManager& MPM, llvm::legacy::FunctionPassManager& FPM) {
    llvm::PassRegistry &PR = *llvm::PassRegistry::getPassRegistry();
    llvm::initializeCore(PR);
    llvm::initializeIPO(PR);
    //llvm::initializeRegToMemPass(<#PassRegistry &#>)
    llvm::initializeSimpleInlinerPass(PR);
    
    llvm::PassManagerBuilder PMBuilder;
    PMBuilder.OptLevel = cl::get_options().optimize ? 1 : 0;
    PMBuilder.SizeLevel = 0;
    
    if (PMBuilder.OptLevel > 1) {
        PMBuilder.Inliner = llvm::createFunctionInliningPass(PMBuilder.OptLevel, PMBuilder.SizeLevel, false);
    } else {
        PMBuilder.Inliner = llvm::createAlwaysInlinerLegacyPass();
    }

    PMBuilder.populateFunctionPassManager(FPM);
    PMBuilder.populateModulePassManager(MPM);
}





#pragma mark - EmitModule


bool shouldEmitType(OutputFileType type) {
    return util::vector::contains(cl::get_options().emit, type);
};



bool emit(llvm::Module &M, llvm::TargetMachine *TM, llvm::raw_pwrite_stream &OS, llvm::TargetMachine::CodeGenFileType CGFT) {
    llvm::legacy::PassManager PM;
    
    if (TM->addPassesToEmitFile(PM, OS, nullptr, CGFT)) {
        return false;
    }
    
    PM.run(M);
    return true;
}


void emitIR(llvm::Module &M, llvm::raw_pwrite_stream &OS) {
    llvm::legacy::PassManager PM;
    PM.add(llvm::createPrintModulePass(OS));
    PM.run(M);
}



// returns true on success
bool emitModule(std::unique_ptr<llvm::Module> module, const std::string &filename, std::string &outputPath) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();
    
    std::string error;
    std::error_code EC;
    
    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    auto hostCPU = llvm::sys::getHostCPUName();
    
    if (llvm::Triple(targetTriple).isOSDarwin()) {
        module->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);
    }
    
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
    if (!target) {
        llvm::errs() << error;
        return false;
    }
    
    // auto CPU = "generic";
    auto features = "";
    
    llvm::TargetOptions opt;
    auto RM = llvm::Optional<llvm::Reloc::Model>();
    auto targetMachine = target->createTargetMachine(targetTriple, hostCPU, features, opt, RM);
    
    module->setDataLayout(targetMachine->createDataLayout());
    module->setTargetTriple(targetTriple);
    
    
    
    // Create and run passes
    
    llvm::legacy::PassManager PM;
    llvm::legacy::FunctionPassManager FPM(module.get());
    
    PM.add(llvm::createTargetTransformInfoWrapperPass(targetMachine->getTargetIRAnalysis()));
    FPM.add(llvm::createTargetTransformInfoWrapperPass(targetMachine->getTargetIRAnalysis()));
    PM.add(llvm::createVerifierPass());

    if (cl::get_options().dumpLLVMPreOpt) {
        PM.add(llvm::createPrintModulePass(llvm::outs(), "Pre-Optimized IR:", true));
    }
    addOptimizationPasses(PM, FPM);

    FPM.doInitialization();
    for (llvm::Function &F : *module) {
        if (!F.isDeclaration()) {
            FPM.run(F);
        }
    }
    FPM.doFinalization();

    if (cl::get_options().dumpLLVM) {
        std::string banner = !cl::get_options().optimize ? "Final IR:" : "Final IR (Optimized):";
        PM.add(llvm::createPrintModulePass(llvm::outs(), banner, true));
    }

    PM.run(*module);

    
    if (cl::get_options().emit.size() == 1 && cl::get_options().emit[0] == OutputFileType::None) {
        return true;
    }


    if (shouldEmitType(OutputFileType::LLVM_IR)) {
        llvm::raw_fd_ostream OS(std::string(filename).append(".ll"), EC);
        emitIR(*module, OS);
    }

    
    
    // Emit object file and/or assembly
    std::string objectFilePath;
    
    if (shouldEmitType(OutputFileType::Assembly)) {
        llvm::raw_fd_ostream OS(std::string(filename).append(".s"), EC);
        emit(*module, targetMachine, OS, llvm::TargetMachine::CodeGenFileType::CGFT_AssemblyFile);
    }
    
    if (shouldEmitType(OutputFileType::ObjectFile)) {
        objectFilePath = std::string(filename).append(".o");
        llvm::raw_fd_ostream OS(objectFilePath, EC);
        emit(*module, targetMachine, OS, llvm::TargetMachine::CodeGenFileType::CGFT_ObjectFile);
    }
    
    

    
    if (!shouldEmitType(OutputFileType::Binary)) {
        return true;
    }

    // Link object file into executable


    // Using clang/gcc to link since that seems to work more reliable than directly calling ld
    auto linkerPath = llvm::sys::findProgramByName("clang") ?: llvm::sys::findProgramByName("gcc");
    if (!linkerPath) {
        LKFatalError("unable to find clang or gcc");
    }

    outputPath = "a.out";

    std::vector<llvm::StringRef> ld_argv = {
        linkerPath.get(),
        objectFilePath,
        "-lc",
        "-o", outputPath
    };


    auto res = llvm::sys::ExecuteAndWait(ld_argv[0], ld_argv);
    if (res != 0) {
        LKFatalError("'%s' returned with non-zero exit code %i", linkerPath->c_str(), res);
    }

    return true;
}









#pragma mark - RunExecutable


// Returns std::nullopt if the compiler can run an executable,
// otherwise a string explaining why it is not possible
std::optional<std::string> canRunExecutable() {
    if (getenv("YO_DISABLE_EXEC") != nullptr) {
        return "YO_DISABLE_EXEC environment variable prevents execution";
    }
    
    return std::nullopt;
}


int runExecutable(const std::string& executablePath, const char *const *envp) {
    auto argc = cl::get_options().runArgs.size();
    auto argv = static_cast<char **>(calloc(argc + 2, sizeof(char *))); // +2 bc the first element is the executable path and the array is null terminated
    argv[0] = strdup(executablePath.c_str());

    for (size_t i = 0; i < argc; i++) {
        argv[i + 1] = strdup(cl::get_options().runArgs[i].c_str());
    }
    llvm::llvm_shutdown();

    auto status = execve(argv[0], (char *const *) argv, (char *const *) envp);
    perror("execve failed");
    return status;
}




#pragma mark - Main


int main(int argc, const char * argv[], const char *const *envp) {
    cl::init(argc, argv);
    
    
    if (!cl::get_options().runArgs.empty()) {
        cl::get_options_mut().run = true;
    }
    
    const std::string inputFile = cl::get_options().inputFile;
    const std::string inputFilename = util::fs::path_utils::getFilename(inputFile);
    
    if (cl::get_options().pygmentize) {
        auto tokens = parser::Lexer().lex(util::fs::read_file(inputFile), inputFilename, true);
        std::cout << lex::pygmentize(tokens) << std::endl;
        return EXIT_SUCCESS;
    }
    
    
    parser::Parser parser;
    
    if (!cl::get_options().stdlibRoot.empty()) {
        parser.setCustomStdlibRoot(cl::get_options().stdlibRoot);
    }
    
    auto ast = parser.parse(inputFile);
    
    if (cl::get_options().printAST) {
        std::cout << ast::description(ast) << std::endl;
        return EXIT_SUCCESS;
    }
    
    irgen::IRGenerator codegen(inputFile);
    codegen.codegen(ast);
    
    std::unique_ptr<llvm::Module> M = codegen.getModule();
    
    if (cl::get_options().run && !shouldEmitType(OutputFileType::Binary)) {
        cl::get_options_mut().emit.push_back(OutputFileType::Binary);
    }
    
    if (shouldEmitType(OutputFileType::Binary) && !shouldEmitType(OutputFileType::ObjectFile)) {
        cl::get_options_mut().emit.push_back(OutputFileType::ObjectFile);
    }
    
    LKAssertImplication(shouldEmitType(OutputFileType::None), cl::get_options().emit.size() == 1);
    
    std::string executablePath;
    
    if (!emitModule(std::move(M), inputFilename, executablePath)) {
        return EXIT_FAILURE;
    }
    
    if (cl::get_options().run) {
        if (auto reason = canRunExecutable()) {
            LKFatalError("Unable to run executable: %s", reason->c_str());
        }
        
        // Only returns if execve failed
        runExecutable(executablePath, envp);
        return EXIT_FAILURE;
    }
    
    return EXIT_SUCCESS;
}

