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
#include "llvm/IR/DIBuilder.h"

#include "AST.h"
#include "Scope.h"
#include "TypeCache.h"
#include "util.h"



NS_START(yo::irgen)



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


// TODO somehow consolidate ResolvedFunction and NEW_ResolvedFunction into a single type!

struct ResolvedFunction {
    std::shared_ptr<ast::FunctionDecl> decl;
    llvm::Function *llvmFunction; // nullptr if Decl is a template function
    uint8_t argumentOffset;
    
    ResolvedFunction(std::shared_ptr<ast::FunctionDecl> decl, llvm::Function *llvmFunction, uint8_t argumentOffset = 0) : decl(decl), llvmFunction(llvmFunction), argumentOffset(argumentOffset) {}
};


struct NEW_ResolvedFunction {
    // what do we need and why?
    // - signature      (why? parameters, return type, attributes)
    // - llvm value     (why? Builder.CreateCall)
    // - argumentOffset (why? instance method invocations)
    
    std::shared_ptr<ast::FunctionSignature> signature;
    llvm::Value *llvmValue;
    uint8_t argumentOffset;
    
    NEW_ResolvedFunction(std::shared_ptr<ast::FunctionSignature> signature, llvm::Value *llvmValue, uint8_t argumentOffset) : signature(signature), llvmValue(llvmValue), argumentOffset(argumentOffset) {}
};


// State of the function currently being generated
struct FunctionState {
    std::shared_ptr<ast::FunctionDecl> decl; // TODO just the signature is enough
    llvm::Function *llvmFunction;
    llvm::BasicBlock *returnBB;
    llvm::Value *retvalAlloca;
    
    FunctionState() : decl(nullptr), llvmFunction(nullptr), returnBB(nullptr), retvalAlloca(nullptr) {}
    FunctionState(std::shared_ptr<ast::FunctionDecl> decl, llvm::Function *llvmFunction, llvm::BasicBlock *returnBB, llvm::Value *retvalAlloca)
    : decl(decl), llvmFunction(llvmFunction), returnBB(returnBB), retvalAlloca(retvalAlloca) {}
};





class IRGenerator {
    std::unique_ptr<llvm::Module> module;
    llvm::Module *M;
    llvm::IRBuilder<> builder;
    
    
    // Debug Metadata
    struct {
        llvm::DIBuilder builder;
        llvm::DICompileUnit *compileUnit;
        std::vector<llvm::DIScope *> lexicalBlocks;
    } debugInfo;
    
    
    irgen::Scope scope;
    irgen::TypeCache typeCache;
    
    llvm::Type *i8, *i16, *i32, *i64;
    llvm::Type *i8_ptr;
    llvm::Type *Void, *Bool, *Double, *i1;
    
    std::map<std::string, std::vector<std::shared_ptr<ast::FunctionDecl>>> templateFunctions;
    
    
    /// key: canonical function name
    std::map<std::string, std::vector<ResolvedFunction>> functions;
    // key: fully resolved function name
    std::map<std::string, std::shared_ptr<ast::FunctionSignature>> resolvedFunctions;
    
    
    // The function currently being generated
    irgen::FunctionState currentFunction;

    
public:
    static llvm::LLVMContext C;
    
    explicit IRGenerator(const std::string moduleName);
    
    void codegen(ast::AST &ast);
    
    std::unique_ptr<llvm::Module> getModule() {
        return std::move(module);
    }
    
    
    // Options
    bool enableARC = false;
    
private:
//    struct FunctionCodegenOptions {
//        bool isVariadic;
//        bool isExternal;
//        std::optional<std::string> typename_;
//
//        FunctionCodegenOptions() : isVariadic(false), isExternal(false), typename_(std::nullopt) {}
//    };
    
    
    void preflight(ast::AST &ast);
    void registerFunction(std::shared_ptr<ast::FunctionDecl> function);
    void registerStructDecl(std::shared_ptr<ast::StructDecl> structDecl);
    void registerImplBlock(std::shared_ptr<ast::ImplBlock> implBlock);
    
    void verifyDeclarations();
    
    // Debug Metadata
    void emitDebugLocation(const std::shared_ptr<ast::Node> &node);
    
    
    // lvalue/rvalue
    enum class CodegenReturnValueKind {
        Value, Address
    };
    
    // Codegen
    llvm::Value *codegen(std::shared_ptr<ast::TopLevelStmt>);
    llvm::Value *codegen(std::shared_ptr<ast::LocalStmt>);
    llvm::Value *codegen(std::shared_ptr<ast::Expr>, CodegenReturnValueKind = CodegenReturnValueKind::Value);
    
    llvm::Value *codegen(std::shared_ptr<ast::FunctionDecl>);
    llvm::Value *codegen(std::shared_ptr<ast::StructDecl>);
    llvm::Value *codegen(std::shared_ptr<ast::ImplBlock>);
    
    
    llvm::Value *codegen(std::shared_ptr<ast::Composite>);
    llvm::Value *codegen(std::shared_ptr<ast::ReturnStmt>);
    llvm::Value *codegen(std::shared_ptr<ast::VariableDecl>);
    llvm::Value *codegen(std::shared_ptr<ast::Assignment>);
    llvm::Value *codegen(std::shared_ptr<ast::IfStmt>);
    llvm::Value *codegen(std::shared_ptr<ast::WhileStmt>);
    llvm::Value *codegen(std::shared_ptr<ast::ForLoop>);
    
    llvm::Value *codegen(std::shared_ptr<ast::NumberLiteral>);
    llvm::Value *codegen(std::shared_ptr<ast::StringLiteral>);
    
    llvm::Value *codegen(std::shared_ptr<ast::Typecast>);
    llvm::Value *codegen(std::shared_ptr<ast::BinaryOperation>);
    llvm::Value *codegen(std::shared_ptr<ast::UnaryExpr>);
    
    llvm::Value *codegen(std::shared_ptr<ast::Identifier>, CodegenReturnValueKind);
    llvm::Value *codegen(std::shared_ptr<ast::RawLLVMValueExpr>);
    
    llvm::Value *codegen(std::shared_ptr<ast::Comparison>);
    llvm::Value *codegen(std::shared_ptr<ast::LogicalOperation>);
    
    llvm::Value *codegen(std::shared_ptr<ast::SubscriptExpr>, CodegenReturnValueKind);
    llvm::Value *codegen(std::shared_ptr<ast::MemberExpr>, CodegenReturnValueKind);
    llvm::Value *codegen(std::shared_ptr<ast::ExprStmt>);
    llvm::Value *codegen(std::shared_ptr<ast::CallExpr>);
    
    llvm::Value *codegen_HandleIntrinsic(std::shared_ptr<ast::FunctionSignature> signature, std::shared_ptr<ast::CallExpr>);
    
    
    // Match Expr
    llvm::Value *codegen(std::shared_ptr<ast::MatchExpr>);
    
    struct MatchExprPatternCodegenInfo {
        TypeInfo *targetType; // type of the expression we're matching against
        std::shared_ptr<ast::Expr> targetExpr; // the expression we're matching against
        llvm::Value *targetLLVMValue; // Codegen result for TargetExpr
        std::shared_ptr<ast::Expr> patternExpr;
    };
    // TODO? this should be the central point that handles all pattern checks and returns the correct expressions, based on the input types
    llvm::Value *codegen_HandleMatchPatternExpr(MatchExprPatternCodegenInfo);
    
    
    // Types
    llvm::Type *getLLVMType(TypeInfo *TI);
    llvm::DIType *getDIType(TypeInfo *TI);
    
    
    llvm::DISubroutineType *_toDISubroutineType(ast::FunctionSignature *);
    
    // Applying trivial number literal casts
    
    // The following 2 methods to two things:
    // 1. Check whether the type of `Expr` is the same as `ExpectedType`
    // 2. Apply a trivial cast to Expr, if and only if Expr is a number literal that can fit in the expected type
    // Version 2 does the same thing, but for binops. if necessary, it might cast either `Lhs` or `Rhs` to the type of `Rhs` or `Lhs` (respectively)
    // both return true on success
    
    // `TypeOfExpr`: initial (unchanged) type of `Expr`
    bool typecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *expr, TypeInfo *expectedType, TypeInfo **initialTypeOfExpr = nullptr);
    
    // `{Lhs|Rhs}Ty`: type of lhs/rhs, after applying typecasts, if casts were applied
    bool typecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *lhs, std::shared_ptr<ast::Expr> *rhs, TypeInfo **lhsTy, TypeInfo **rhsTy);
    
    
    llvm::Value *generateStructInitializer(std::shared_ptr<ast::StructDecl> structDecl);
    
    
    // Other stuff
    std::shared_ptr<ast::FunctionSignature> getResolvedFunctionWithName(const std::string &name);
    
    TypeInfo *instantiateTemplatedType(TypeInfo *TI);
    
    // set omitCodegen to true if you only care about the return type of the call
    // for each callExpr, omitCodegen should be false exactly once!!!
    NEW_ResolvedFunction resolveCall(std::shared_ptr<ast::CallExpr>, bool omitCodegen);
    std::optional<std::map<std::string, TypeInfo *>> attemptToResolveTemplateArgumentTypesForCall(std::shared_ptr<ast::FunctionDecl> templateFunction, std::shared_ptr<ast::CallExpr> call, unsigned argumentOffset);
    
    TypeInfo *guessType(std::shared_ptr<ast::Expr>);
    TypeInfo *guessType(std::shared_ptr<ast::NumberLiteral>);
    
    // Returns true if SrcType is trivially convertible to DestType
    bool isTriviallyConvertible(TypeInfo *srcType, TypeInfo *destType);
    bool valueIsTriviallyConvertibleTo(std::shared_ptr<ast::NumberLiteral> number, TypeInfo *TI);
    
    
    
    // Utils
    
    template <typename F>
    std::invoke_result_t<F> withCleanSlate(F fn) {
        auto prevScope = scope;
        auto prevCurrentFunction = currentFunction;
        auto prevInsertBlock = builder.GetInsertBlock();
        
        scope = irgen::Scope();
        
        auto retval = fn();
        
        scope = prevScope;
        currentFunction = prevCurrentFunction;
        builder.SetInsertPoint(prevInsertBlock);
        
        return retval;
    }
};

NS_END
