//
//  Driver.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-02-15.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#include "lex/Diagnostics.h"
#include "parse/Parser.h"
#include "Driver.h"
#include "IRGen.h"
#include "util/util.h"

#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/IPO.h"
//#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Program.h"

#include <fstream>
#include <sstream>
#include <iostream>
#include <memory>
#include <filesystem>


using namespace yo;
using namespace yo::driver;





#pragma mark - Optimizations


void addOptimizationPasses(const Options &options, llvm::PassRegistry &PR, llvm::legacy::PassManager &MPM, llvm::legacy::FunctionPassManager &FPM) {
    // TODO https://llvm.org/docs/NewPassManager.html
////    llvm::initializeIPO(PR);
////    llvm::initializeRegToMemPass(PR);
////    llvm::initializeSimpleInlinerPass(PR);
////    llvm::initializeStripDeadPrototypesLegacyPassPass(PR);
//    
//    llvm::PassManagerBuilder PMBuilder;
//    PMBuilder.OptLevel = options.optimize ? 1 : 0;
//    PMBuilder.SizeLevel = 0;
//    
//    if (!options.fnoInline) {
//        if (PMBuilder.OptLevel > 1) {
//            PMBuilder.Inliner = llvm::createFunctionInliningPass(PMBuilder.OptLevel, PMBuilder.SizeLevel, false);
//        } else {
//            PMBuilder.Inliner = llvm::createAlwaysInlinerLegacyPass();
//        }
//    }
//    
//    PMBuilder.populateFunctionPassManager(FPM);
//    PMBuilder.populateModulePassManager(MPM);
//    
////    MPM.add(llvm::createStripDeadPrototypesPass()); // TODO
}





#pragma mark - EmitModule



bool emit(llvm::Module &M, llvm::TargetMachine *TM, llvm::raw_pwrite_stream &OS, llvm::CodeGenFileType CGFT) {
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
bool emitModule(const Options &options, std::unique_ptr<llvm::Module> module, const std::string &filename) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();
    
    llvm::PassRegistry &PR = *llvm::PassRegistry::getPassRegistry();
    //llvm::initializeCore(PR);
    
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
    auto RM = std::optional<llvm::Reloc::Model>();
    auto targetMachine = target->createTargetMachine(targetTriple, hostCPU, features, opt, RM);
    
    module->setDataLayout(targetMachine->createDataLayout());
    module->setTargetTriple(targetTriple);
    
    
//    llvm::outs() << *module << '\n';
    
    // Create and run passes
    
    llvm::legacy::PassManager PM;
    llvm::legacy::FunctionPassManager FPM(module.get());
    
    PM.add(llvm::createTargetTransformInfoWrapperPass(targetMachine->getTargetIRAnalysis()));
    FPM.add(llvm::createTargetTransformInfoWrapperPass(targetMachine->getTargetIRAnalysis()));
    PM.add(llvm::createVerifierPass());

    if (options.dumpLLVMPreOpt) {
        PM.add(llvm::createPrintModulePass(llvm::outs(), "Pre-Optimized IR:", true));
    }
    addOptimizationPasses(options, PR, PM, FPM);

    FPM.doInitialization();
    for (llvm::Function &F : *module) {
        if (!F.isDeclaration()) {
            FPM.run(F);
        }
    }
    FPM.doFinalization();

    if (options.dumpLLVM) {
        std::string banner = !options.optimize ? "Final IR:" : "Final IR (Optimized):";
        PM.add(llvm::createPrintModulePass(llvm::outs(), banner, true));
    }

    PM.run(*module);

    if (options.outputFileTypes.isEmpty()) {
        return true;
    }


    if (options.outputFileTypes.contains(OutputFileType::LLVM_IR)) {
        llvm::raw_fd_ostream OS(util::fmt::format("{}.ll", filename), EC);
        emitIR(*module, OS);
    }

    
    
    // Emit object file and/or assembly
    std::string objectFilePath;
    
    if (options.outputFileTypes.contains(OutputFileType::Assembly)) {
        llvm::raw_fd_ostream OS(util::fmt::format("{}.s", filename), EC);
        emit(*module, targetMachine, OS, llvm::CodeGenFileType::CGFT_AssemblyFile);
    }
    
    if (options.outputFileTypes.contains(OutputFileType::ObjectFile)) {
        objectFilePath = util::fmt::format("{}.o", filename);
        llvm::raw_fd_ostream OS(objectFilePath, EC);
        emit(*module, targetMachine, OS, llvm::CodeGenFileType::CGFT_ObjectFile);
    }
    
    if (options.outputFileTypes.contains(OutputFileType::LLVM_BC)) {
        llvm::raw_fd_ostream OS(util::fmt::format("{}.bc", filename), EC);
        llvm::WriteBitcodeToFile(*module, OS);
    }

    
    if (!options.outputFileTypes.contains(OutputFileType::Binary)) {
        return true;
    }

    // Link object file into executable


    // Using clang/gcc to link since that seems to work more reliable than directly calling ld
    auto linkerPath = llvm::sys::findProgramByName("clang") ?: llvm::sys::findProgramByName("gcc");
    if (!linkerPath) {
        LKFatalError("unable to find clang or gcc");
    }

    std::vector<llvm::StringRef> ld_argv = {
        linkerPath.get(),
        objectFilePath,
        "-lc",
        "-o", "a.out"
    };


    auto res = llvm::sys::ExecuteAndWait(ld_argv[0], ld_argv);
    if (res != 0) {
        LKFatalError("'%s' returned with non-zero exit code %i", linkerPath->c_str(), res);
    }

    return true;
}



#pragma mark - Run

static bool shouldSigabrtOnFatalError = false;

extern "C" bool LKCompilerInternalOptionSigabrtOnFatalError() {
    return shouldSigabrtOnFatalError;
}

extern "C" void LKCompilerSetInternalOptionSigabrtOnFatalError(bool newValue) {
    shouldSigabrtOnFatalError = newValue;
}


bool driver::run(driver::Options options) {
    auto cwd = std::filesystem::current_path();
    std::cout << "cwd: " << cwd << std::endl;
    
    shouldSigabrtOnFatalError = options.int_trapOnFatalError;
    
    if (!util::fs::file_exists(options.inputFile)) {
        diagnostics::emitError(util::fmt::format("input file '{}' does not exist", options.inputFile));
    }
    
    const std::string inputFile = options.inputFile;
    const std::string inputFilename = util::fs::path_get_filename(inputFile);
    
    parser::Parser parser;
    
    if (!options.stdlibRoot.empty()) {
        parser.setCustomStdlibRoot(options.stdlibRoot);
    }
    
    auto ast = parser.parse(inputFile);
//    ast::print_ast(ast);
    
    if (options.dumpAST) {
        std::cout << ast::description(ast) << std::endl;
    }
    
    irgen::IRGenerator irgen(ast, inputFile, options);
    irgen.runCodegen();
    
    std::unique_ptr<llvm::Module> M = irgen.getModule();
    
    if (options.outputFileTypes.contains(OutputFileType::Binary)) {
        options.outputFileTypes.insert(OutputFileType::ObjectFile);
    }
    
    if (emitModule(options, std::move(M), inputFilename)) {
        return true;
    }
    return false;
}


