//
//  IRGen.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <memory>

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

#include "AST.h"
#include "Scope.h"
#include "util.h"



NS_START(irgen)




class IRGenerator {
    std::unique_ptr<llvm::Module> Module;
    llvm::Module *M;
    llvm::IRBuilder<> Builder;
    
    Scope Scope;
    
    llvm::Type *i8, *i16, *i32, *i64;
    llvm::Type *i8_ptr;
    llvm::Type *Void, *Bool, *Double;
    
public:
    static llvm::LLVMContext C;
    
    explicit IRGenerator(const std::string ModuleName);
    
    
    void Codegen(ast::AST &Ast);
    
    std::unique_ptr<llvm::Module> GetModule() {
        return std::move(Module);
    }
    
private:
    void Preflight(ast::AST &Ast);
    void RegisterFunctionSignature(std::shared_ptr<ast::FunctionSignature> Signature, bool MangleName = true);
    
    
    // Codegen
    llvm::Value *Codegen(std::shared_ptr<ast::Node>);
    llvm::Value *Codegen(std::shared_ptr<ast::TopLevelStmt>);
    llvm::Value *Codegen(std::shared_ptr<ast::LocalStmt>);
    llvm::Value *Codegen(std::shared_ptr<ast::Expr>);
    
    llvm::Value *Codegen(std::shared_ptr<ast::FunctionDecl>, bool MangleName = true);
    llvm::Value *Codegen(std::shared_ptr<ast::Composite>);
    llvm::Value *Codegen(std::shared_ptr<ast::ReturnStmt>);
    
    llvm::Value *Codegen(std::shared_ptr<ast::NumberLiteral>);
    
    
    // Types
    llvm::Type *GetLLVMType(TypeInfo *TI);
};



NS_END
