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


//class Counter {
//    uint64_t Value;
//    const uint64_t InitialValue;
//
//public:
//    explicit Counter(uint64_t InitialValue = 0) : Value(InitialValue), InitialValue(InitialValue) {}
//
//    uint64_t Increment() { return ++Value; }
//    uint64_t GetCurrent() { return Value; }
//    void Reset() { Value = InitialValue; }
//};

struct ResolvedFunction {
    std::shared_ptr<ast::FunctionDecl> Decl;
    llvm::Function *LLVMFunction; // nullptr if Decl is a template function
    
    ResolvedFunction(std::shared_ptr<ast::FunctionDecl> Decl, llvm::Function *LLVMFunction) : Decl(Decl), LLVMFunction(LLVMFunction) {}
};

class IRGenerator {
    std::unique_ptr<llvm::Module> Module;
    llvm::Module *M;
    llvm::IRBuilder<> Builder;
    
    irgen::Scope Scope;
    irgen::TypeCache TypeCache;
    
    llvm::Type *i8, *i16, *i32, *i64;
    llvm::Type *i8_ptr;
    llvm::Type *Void, *Bool, *Double;
    
    std::map<std::string, std::vector<std::shared_ptr<ast::FunctionDecl>>> TemplateFunctions;
    
    
    // key: canonical function name
    std::map<std::string, std::vector<ResolvedFunction>> Functions;
    // key: fully resolved function name
    std::map<std::string, std::shared_ptr<ast::FunctionSignature>> ResolvedFunctions;
    
    
//    irgen::Counter Counter;
    
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
        std::optional<std::string> Typename;
        
        FunctionCodegenOptions() : IsVariadic(false), IsExternal(false), Typename(std::nullopt) {}
    };
    
    void Preflight(ast::AST &Ast);
    void RegisterFunction(std::shared_ptr<ast::FunctionDecl> Function);
    void RegisterFunction(std::shared_ptr<ast::ExternFunctionDecl> Function);
    void RegisterStructDecl(std::shared_ptr<ast::StructDecl> Struct);
    void RegisterImplBlock(std::shared_ptr<ast::ImplBlock> ImplBlock);
    
    void VerifyFunctionDeclarations();
    
    
    // lvalue/rvalue
    enum class CodegenReturnValueKind {
        Value, Address
    };
    
    // Codegen
    llvm::Value *Codegen(std::shared_ptr<ast::TopLevelStmt>);
    llvm::Value *Codegen(std::shared_ptr<ast::LocalStmt>);
    llvm::Value *Codegen(std::shared_ptr<ast::Expr>, CodegenReturnValueKind = CodegenReturnValueKind::Value);
    
    llvm::Value *Codegen(std::shared_ptr<ast::FunctionDecl>);
    llvm::Value *Codegen(std::shared_ptr<ast::StructDecl>);
    llvm::Value *Codegen(std::shared_ptr<ast::ImplBlock>);
    
    
    llvm::Value *Codegen(std::shared_ptr<ast::Composite>, bool IsFunctionBody = false);
    llvm::Value *Codegen(std::shared_ptr<ast::ReturnStmt>);
    llvm::Value *Codegen(std::shared_ptr<ast::FunctionCall>, unsigned ArgumentOffset = 0, std::shared_ptr<ast::FunctionSignature> *SelectedOverload = nullptr);
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
    
    llvm::Value *Codegen_HandleIntrinsic(std::shared_ptr<ast::FunctionSignature> Signature, std::shared_ptr<ast::FunctionCall>, unsigned ArgumentOffset = 0);
    
    // Types
    llvm::Type *GetLLVMType(TypeInfo *TI);
    
    // Applying trivial number literal casts
    
    // The following 2 methods to two things:
    // 1. Check whether the type of `Expr` is the same as `ExpectedType`
    // 2. Apply a trivial cast to Expr, if and only if Expr is a number literal that can fit in the expected type
    // Version 2 does the same thing, but for binops. if necessary, it might cast either `Lhs` or `Rhs` to the type of `Rhs` or `Lhs` (respectively)
    // both return true on success
    
    // `TypeOfExpr`: initial (unchanged) type of `Expr`
    bool TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *Expr, TypeInfo *ExpectedType, TypeInfo **InitialTypeOfExpr = nullptr);
    
    // `{Lhs|Rhs}Ty`: type of lhs/rhs, after applying typecasts, if casts were applied
    bool TypecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *Lhs, std::shared_ptr<ast::Expr> *Rhs, TypeInfo **LhsTy, TypeInfo **RhsTy);
    
    
    llvm::Value *GenerateStructInitializer(std::shared_ptr<ast::StructDecl> Struct);
    
    
    // Other stuff
    std::shared_ptr<ast::FunctionSignature> GetResolvedFunctionWithName(std::string &Name);
    ResolvedFunction ResolveCall(std::shared_ptr<ast::FunctionCall> Call, unsigned ArgumentOffset);
    std::optional<std::map<std::string, TypeInfo *>> AttemptToResolveTemplateArgumentTypesForCall(std::shared_ptr<ast::FunctionDecl> TemplateFunction, std::shared_ptr<ast::FunctionCall> Call, unsigned ArgumentOffset);
    
    ResolvedFunction InstantiateTemplateFunctionForCall(std::shared_ptr<ast::FunctionDecl> TemplateFunction, std::shared_ptr<ast::FunctionCall> Call, unsigned ArgumentOffset, std::map<std::string, TypeInfo *> TemplateArgumentMapping);
    
    TypeInfo *GuessType(std::shared_ptr<ast::Expr> Expr);
    TypeInfo *GuessType(std::shared_ptr<ast::MemberAccess> MemberAccess);
    
    // Returns true if SrcType is trivially convertible to DestType
    bool IsTriviallyConvertible(TypeInfo *SrcType, TypeInfo *DestType);
    bool ValueIsTriviallyConvertibleTo(std::shared_ptr<ast::NumberLiteral> Number, TypeInfo *TI);
    
    
    
    // Utils
    
    template <typename F>
    std::invoke_result_t<F> WithCleanSlate(F Fn) {
        auto PrevScope = Scope;
        auto PrevInsertBlock = Builder.GetInsertBlock();
        
        Scope = irgen::Scope();
        
        auto Retval = Fn();
        
        Scope = PrevScope;
        Builder.SetInsertPoint(PrevInsertBlock);
        
        return Retval;
    }
};

NS_END
