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

#define Parser_test

static llvm::LLVMContext C;

int main(int argc, const char * argv[], const char *const *envp) {
#ifdef Parser_test
    std::ifstream File("/Users/lukas/Developer/yo/temp/program.yo");
    std::ostringstream Contents;
    Contents << File.rdbuf();
    File.close();
    
    Lexer Lexer;
    auto S = Contents.str();
    auto Tokens = Lexer.Lex(S);
    
    for (auto &Token : Tokens) {
        std::cout << *Token << std::endl;
    }
    
    
    Parser P;
    auto Ast = P.Parse(Tokens);
    
    //dump_ast(Ast);
    
    std::cout << Ast[0]->Description() << std::endl;
    
#else
    cl::Init(argc, argv);
    
    Parser P;
    auto A = P.ParseFile(cl::InputFilename);
    
    
    irgen::IRGenerator Codegen(C, "main");
    Codegen.Codegen(A);
    
    auto M = Codegen.GetModule();
    
    std::cout << "IR:" << std::endl;
    M->print(llvm::outs(), nullptr, false, false);
    
    JIT JIT(std::move(M));
    
    auto retval = JIT.RunMain(envp);
    std::cout << "Retval: " << retval << std::endl;
    
    return EXIT_SUCCESS;
#endif
}

