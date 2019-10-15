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
#include "Type.h"
#include "Scope.h"
#include "util.h"
#include "CommandLine.h"


NS_START(yo::irgen)


enum ValueKind {
    LValue, RValue
};


struct ValueBinding {
    enum Flags : uint8_t {
        CanRead  = 1 << 0,
        CanWrite = 1 << 1
    };
    
    using ReadImp  = std::function<llvm::Value*(void)>;
    using WriteImp = std::function<void(llvm::Value*)>;
    
    Type *type;
    llvm::Value *value;
    ReadImp read;
    WriteImp write;
    uint8_t flags;
    
    bool hasFlag(Flags F) const { return flags & F; }
    
    ValueBinding(Type *type, llvm::Value *value, ReadImp read, WriteImp write, uint8_t F)
    : type(type), value(value), read(read), write(write), flags(F) {}
};


struct ResolvedCallable {
    ast::FunctionSignature signature;
    std::shared_ptr<ast::FunctionDecl> funcDecl; // only nonnull if the callable is a function decl
    llvm::Value *llvmValue;
    uint8_t argumentOffset;
    
    ResolvedCallable(ast::FunctionSignature sig, std::shared_ptr<ast::FunctionDecl> funcDecl, llvm::Value *llvmValue, uint8_t argumentOffset)
    : signature(sig), funcDecl(funcDecl), llvmValue(llvmValue), argumentOffset(argumentOffset) {}
    
    ResolvedCallable(std::shared_ptr<ast::FunctionDecl> funcDecl, llvm::Value *llvmValue, uint8_t argumentOffset)
    : signature(funcDecl->getSignature()), funcDecl(funcDecl), llvmValue(llvmValue), argumentOffset(argumentOffset) {}
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
    llvm::IRBuilder<> builder;
    
    
    // Debug Metadata
    struct {
        llvm::DIBuilder builder;
        llvm::DICompileUnit *compileUnit;
        std::vector<llvm::DIScope *> lexicalBlocks;
    } debugInfo;
    
    const cl::Options &CLIOptions;
    
    NamedScope<ValueBinding> localScope;
    
    // Finalized nominal types
    // If the type is a template specialization, the key is the mangled name
    NamedScope<Type *> nominalTypes;
    
    llvm::Type *i8, *i16, *i32, *i64;
    llvm::Type *i8_ptr;
    llvm::Type *Void, *Double, *i1;
    
    std::map<std::string, std::vector<std::shared_ptr<ast::FunctionDecl>>> templateFunctions;
    
    
    /// key: canonical function name
    std::map<std::string, std::vector<ResolvedCallable>> functions;
    // TODO: what's the point of `resolvedFunctions`?
    // key: fully resolved function name
    std::map<std::string, ResolvedCallable> resolvedFunctions;
    
    
    // The function currently being generated
    irgen::FunctionState currentFunction;

    
public:
    static llvm::LLVMContext C;
    
    explicit IRGenerator(const std::string& translationUnitPath);
    
    void codegen(const ast::AST& ast);
    
    std::unique_ptr<llvm::Module> getModule() {
        return std::move(module);
    }
    
    
    
private:
    void preflight(const ast::AST& ast);
    void registerFunction(std::shared_ptr<ast::FunctionDecl> function);
    void registerStructDecl(std::shared_ptr<ast::StructDecl> structDecl);
    void registerImplBlock(std::shared_ptr<ast::ImplBlock> implBlock);
    
    // Debug Metadata
    void emitDebugLocation(const std::shared_ptr<ast::Node> &node);
    
    
    
    // Codegen
    llvm::Value *codegen(std::shared_ptr<ast::TopLevelStmt>);
    llvm::Value *codegen(std::shared_ptr<ast::LocalStmt>);
    llvm::Value *codegen(std::shared_ptr<ast::Expr>, ValueKind = RValue); // TODO should this really default to rvalue?
    
    llvm::Value *codegen(std::shared_ptr<ast::FunctionDecl>);
    llvm::Value *codegen(std::shared_ptr<ast::StructDecl>);
    llvm::Value *codegen(std::shared_ptr<ast::ImplBlock>);
    
    
    llvm::Value *codegen(const std::vector<std::shared_ptr<ast::LocalStmt>>&);
    llvm::Value *codegen(std::shared_ptr<ast::Composite>);
    llvm::Value *codegen(std::shared_ptr<ast::ReturnStmt>);
    llvm::Value *codegen(std::shared_ptr<ast::VarDecl>);
    llvm::Value *codegen(std::shared_ptr<ast::Assignment>);
    llvm::Value *codegen(std::shared_ptr<ast::IfStmt>);
    llvm::Value *codegen(std::shared_ptr<ast::WhileStmt>);
    llvm::Value *codegen(std::shared_ptr<ast::ForLoop>);
    
    llvm::Value *codegen(std::shared_ptr<ast::NumberLiteral>);
    llvm::Value *codegen(std::shared_ptr<ast::StringLiteral>);
    
    llvm::Value *codegen(std::shared_ptr<ast::CastExpr>);
    llvm::Value *codegen(std::shared_ptr<ast::UnaryExpr>);
    
    llvm::Value *codegen(std::shared_ptr<ast::Ident>, ValueKind);
    llvm::Value *codegen(std::shared_ptr<ast::RawLLVMValueExpr>);
    
    llvm::Value *codegen(std::shared_ptr<ast::BinOp>);
    
    llvm::Value *codegen(std::shared_ptr<ast::SubscriptExpr>, ValueKind);
    llvm::Value *codegen(std::shared_ptr<ast::MemberExpr>, ValueKind);
    llvm::Value *codegen(std::shared_ptr<ast::ExprStmt>);
    llvm::Value *codegen(std::shared_ptr<ast::CallExpr>);
    
    llvm::Value *codegen_HandleIntrinsic(std::shared_ptr<ast::FunctionDecl>, std::shared_ptr<ast::CallExpr>);
    llvm::Value *codegen_HandleArithmeticIntrinsic(ast::Operator, std::shared_ptr<ast::Expr> lhs, std::shared_ptr<ast::Expr> rhs);
    llvm::Value *codegen_HandleComparisonIntrinsic(ast::Operator, std::shared_ptr<ast::Expr> lhs, std::shared_ptr<ast::Expr> rhs);
    llvm::Value *codegen_HandleLogOpIntrinsic(ast::Operator, std::shared_ptr<ast::Expr> lhs, std::shared_ptr<ast::Expr> rhs);
    
    
    // Match Expr
    llvm::Value *codegen(std::shared_ptr<ast::MatchExpr>);
    
    struct MatchExprPatternCodegenInfo {
        Type *targetType; // type of the expression we're matching against
        std::shared_ptr<ast::Expr> targetExpr; // the expression we're matching against
        llvm::Value *targetLLVMValue; // Codegen result for TargetExpr
        std::shared_ptr<ast::Expr> patternExpr;
    };
    // TODO? this should be the central point that handles all pattern checks and returns the correct expressions, based on the input types
    llvm::Value *codegen_HandleMatchPatternExpr(MatchExprPatternCodegenInfo);
    
    
    // Types
    Type* resolveTypeDesc(std::shared_ptr<ast::TypeDesc>, bool setInternalResolvedType = true);
    llvm::Type *getLLVMType(Type *);
    llvm::DIType *getDIType(Type *);
    uint8_t argumentOffsetForCallingConvention(CallingConvention cc);
    
    
    
    llvm::DISubroutineType *_toDISubroutineType(const ast::FunctionSignature&);
    
    // Applying trivial number literal casts
    
    // The following 2 methods to two things:
    // 1. Check whether the type of `Expr` is the same as `ExpectedType`
    // 2. Apply a trivial cast to Expr, if and only if Expr is a number literal that can fit in the expected type
    // Version 2 does the same thing, but for binops. if necessary, it might cast either `Lhs` or `Rhs` to the type of `Rhs` or `Lhs` (respectively)
    // both return true on success
    
    // `TypeOfExpr`: initial (unchanged) type of `Expr`
    bool typecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *expr, Type *expectedType, Type **initialTypeOfExpr = nullptr);
    
    // `{Lhs|Rhs}Ty`: type of lhs/rhs, after applying typecasts, if casts were applied
    bool typecheckAndApplyTrivialNumberTypeCastsIfNecessary(std::shared_ptr<ast::Expr> *lhs, std::shared_ptr<ast::Expr> *rhs, Type **lhsTy, Type **rhsTy);
    
    
    llvm::Value *generateStructInitializer(std::shared_ptr<ast::StructDecl> structDecl);
    
    
    // Other stuff
    std::optional<ResolvedCallable> getResolvedFunctionWithName(const std::string &name);
    
    Type *instantiateTemplatedType(std::shared_ptr<ast::TypeDesc>);
    
    llvm::Value *NEW_synthesizeStructInitializer(std::shared_ptr<ast::StructDecl>);
    
    
    // set omitCodegen to true if you only care about the return type of the call
    // for each callExpr, omitCodegen should be false exactly once!!!
    ResolvedCallable resolveCall(std::shared_ptr<ast::CallExpr>, bool omitCodegen);
    std::optional<std::map<std::string, std::shared_ptr<ast::TypeDesc>>> attemptToResolveTemplateArgumentTypesForCall(std::shared_ptr<ast::FunctionDecl> templateFunction, std::shared_ptr<ast::CallExpr> call, unsigned argumentOffset);
    
    //TypeInfo *guessType(std::shared_ptr<ast::Expr>);
    //TypeInfo *guessType(std::shared_ptr<ast::NumberLiteral>);
    Type* guessType(std::shared_ptr<ast::Expr>);
    
    bool valueIsTriviallyConvertibleTo(std::shared_ptr<ast::NumberLiteral>, Type *);
    
    bool equal(const ast::FunctionSignature &lhs, const ast::FunctionSignature &rhs);
    
    
    // Globals etc
    void handleStartupAndShutdownFunctions();
    
    
    
    
    
    // Utils
    
    
    // TODO this seems like a bad idea?
    // Assuming this is only ever used for registering synthesized functions, what about just having a `queueSynthFunction` function that registers the llvm::Function, and then puts the FuncDecl in a queue which is handled after regular codegen finished?
    template <typename F>
    std::invoke_result_t<F> withCleanSlate(F &&fn) {
        auto prevLocalScope = localScope;
        auto prevCurrentFunction = currentFunction;
        auto prevInsertBlock = builder.GetInsertBlock();
        
        localScope = {};
        
        auto retval = fn();
        
        localScope = prevLocalScope;
        currentFunction = prevCurrentFunction;
        builder.SetInsertPoint(prevInsertBlock);
        
        return retval;
    }
};

NS_END
