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


int main(int argc, const char * argv[], const char *const *envp) {
    //cl::Init(argc, argv);
    
    std::string Filename("/Users/lukas/Developer/yo/temp/program.yo");
    
    std::ifstream File(Filename);
    std::ostringstream Contents;
    Contents << File.rdbuf();
    File.close();
    
    Lexer Lexer;
    auto S = Contents.str();
    auto Tokens = Lexer.Lex(S, Filename);
    
    
    Parser P;
    auto Ast = P.Parse(Tokens);
    
    
    if (cl::PrintAST) {
        std::cout << ast::Description(Ast) << std::endl;
        return EXIT_SUCCESS;
    }
    
    irgen::IRGenerator Codegen("main");
    Codegen.Codegen(Ast);
    
    auto M = Codegen.GetModule();
    
    if (cl::EmitLLVM) {
        std::error_code EC;
        llvm::raw_fd_ostream OS("main.ll", EC); // TODO use the actual filename!!!
        M->print(OS, nullptr, true, true);
        return EXIT_SUCCESS;
    }
    
    JIT JIT(std::move(M));
    
    auto retval = JIT.RunMain(envp);
    std::cout << "Retval: " << retval << std::endl;
    
    return EXIT_SUCCESS;
}

