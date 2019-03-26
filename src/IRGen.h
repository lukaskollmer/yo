//
//  IRGen.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <memory>
#include <optional>

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

#include "AST.h"
#include "Scope.h"
#include "TypeCache.h"
#include "util.h"



NS_START(irgen)



// TODO
//class StringLiteralCache {
//    std::map<std::string, llvm::Value *> Cache;
//
//public:
//    llvm::Value *Get(std::string)
//};


class IRGenerator {
    std::unique_ptr<llvm::Module> Module;
    llvm::Module *M;
    llvm::IRBuilder<> Builder;
    
    irgen::Scope Scope;
    irgen::TypeCache TypeCache;
    
    llvm::Type *i8, *i16, *i32, *i64;
    llvm::Type *i8_ptr;
    llvm::Type *Void, *Bool, *Double;
    
    std::map<std::string, std::shared_ptr<ast::FunctionSignature>> ExternalFunctions;
    
public:
    static llvm::LLVMContext C;
    
    explicit IRGenerator(const std::string ModuleName);
    
    
    void Codegen(ast::AST &Ast);
    
    std::unique_ptr<llvm::Module> GetModule() {
        return std::move(Module);
    }
    
private:
    struct FunctionCodegenOptions {
        bool IsVariadic;
        bool IsExternal;
        bool ShouldMangleName;
        std::optional<std::string> Typename;
        
        FunctionCodegenOptions() : IsVariadic(false), IsExternal(false), ShouldMangleName(true), Typename(std::nullopt) {}
    };
    
    void Preflight(ast::AST &Ast);
    void RegisterFunctionSignature(std::shared_ptr<ast::FunctionSignature> Signature, FunctionCodegenOptions Options = FunctionCodegenOptions());
    void RegisterStructDecl(std::shared_ptr<ast::StructDecl> Struct);
    void RegisterImplBlock(std::shared_ptr<ast::ImplBlock> ImplBlock);
    
    
    // In some situations (for example when handling an lvalue), we need Codegen to return an address instead of a dereferenced value
    enum class CodegenReturnValueKind {
        Value, Address
    };
    
    // Codegen
    llvm::Value *Codegen(std::shared_ptr<ast::TopLevelStmt>);
    llvm::Value *Codegen(std::shared_ptr<ast::LocalStmt>);
    llvm::Value *Codegen(std::shared_ptr<ast::Expr>, CodegenReturnValueKind = CodegenReturnValueKind::Value);
    
    llvm::Value *Codegen(std::shared_ptr<ast::FunctionDecl>, FunctionCodegenOptions Options = FunctionCodegenOptions());
    llvm::Value *Codegen(std::shared_ptr<ast::StructDecl>);
    llvm::Value *Codegen(std::shared_ptr<ast::ImplBlock>);
    
    
    llvm::Value *Codegen(std::shared_ptr<ast::Composite>, bool IsFunctionBody = false);
    llvm::Value *Codegen(std::shared_ptr<ast::ReturnStmt>);
    llvm::Value *Codegen(std::shared_ptr<ast::FunctionCall>, unsigned ArgumentOffset = 0);
    llvm::Value *Codegen(std::shared_ptr<ast::VariableDecl>);
    llvm::Value *Codegen(std::shared_ptr<ast::IfStmt>);
    llvm::Value *Codegen(std::shared_ptr<ast::WhileStmt>);
    llvm::Value *Codegen(std::shared_ptr<ast::Assignment>);
    
    llvm::Value *Codegen(std::shared_ptr<ast::NumberLiteral>);
    llvm::Value *Codegen(std::shared_ptr<ast::StringLiteral>);
    llvm::Value *Codegen(std::shared_ptr<ast::CharLiteral>);
    llvm::Value *Codegen(std::shared_ptr<ast::Typecast>);
    llvm::Value *Codegen(std::shared_ptr<ast::BinaryOperation>);
    llvm::Value *Codegen(std::shared_ptr<ast::Identifier>, CodegenReturnValueKind);
    
    llvm::Value *Codegen(std::shared_ptr<ast::MemberAccess>, CodegenReturnValueKind);
    
    llvm::Value *Codegen(std::shared_ptr<ast::Comparison>);
    llvm::Value *Codegen(std::shared_ptr<ast::LogicalOperation>);
    
    
    // Types
    llvm::Type *GetLLVMType(TypeInfo *TI);
    bool IsSignedType(llvm::Type *T) {
        return true;
    }
    
    bool TypecheckAndApplyTrivialCastIfPossible(llvm::Value **V, llvm::Type *DestType);
    bool Binop_AttemptToResolvePotentialIntTypeMismatchesByCastingNumberLiteralsIfPossible(llvm::Value **LHS, llvm::Value **RHS);
    
    
    llvm::Value *GenerateStructInitializer(std::shared_ptr<ast::StructDecl> Struct);
    
    
    // Other stuff
    std::shared_ptr<ast::FunctionSignature> GetExternalFunctionWithName(std::string &Name);
};



NS_END
