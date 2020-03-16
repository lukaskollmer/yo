//
//  IRGen.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "lex/SourceLocation.h"
#include "parse/AST.h"
#include "Driver.h"
#include "Type.h"
#include "util/NamedScope.h"
#include "util/util.h"
#include "util/Format.h"
#include "util/Counter.h"
#include "util/OptionSet.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DIBuilder.h"

#include <vector>
#include <string>
#include <stack>
#include <memory>
#include <optional>


NS_START(yo::irgen)

enum class Intrinsic : uint8_t;
class IRGenerator;

using TemplateTypeMapping = std::map<std::string, std::shared_ptr<ast::TypeDesc>>;


StructType* getUnderlyingStruct(Type *ty);
std::string mangleFullyResolved(const std::shared_ptr<ast::FunctionDecl>&);
uint8_t argumentOffsetForFunctionKind(ast::FunctionKind);
bool integerLiteralFitsInIntegralType(uint64_t, Type *);
llvm::DIFile* DIFileForSourceLocation(llvm::DIBuilder&, const lex::SourceLocation&);
std::shared_ptr<ast::CallExpr> subscriptExprToCall(std::shared_ptr<ast::SubscriptExpr>);

std::shared_ptr<ast::Ident> makeIdent(const std::string&, lex::SourceLocation = lex::SourceLocation());

std::shared_ptr<ast::Ident> formatTupleMemberAtIndex(size_t index);


// TODO is it a good idea to put these here?
inline constexpr unsigned kInstanceMethodCallArgumentOffset = 1;
static const std::string kInitializerMethodName = "init";
static const std::string kSynthesizedDeallocMethodName = "__dealloc";
static const std::string kRetvalAllocaIdentifier = "__retval";
static const std::string kIteratorMethodName = "iterator";
static const std::string kIteratorHasNextMethodName = "hasNext";
static const std::string kIteratorNextMethodName = "next";




enum ValueKind { // TODO rename to ValueCategory?
    LValue, RValue
};


struct ValueBinding {
    enum Flags : uint8_t {
        None        = 0,
        CanRead     = 1 << 0,
        CanWrite    = 1 << 1,
        DontDestroy = 1 << 2,
        ReadWrite   = CanRead | CanWrite
    };
    
    using ReadImp  = std::function<llvm::Value*(void)>;
    using WriteImp = std::function<void(llvm::Value*)>;
    
    Type *type;
    llvm::Value *value;
    ReadImp read;
    WriteImp write;
    util::OptionSet<Flags> flags;
    
    bool hasFlag(Flags F) const { return flags.contains(F); }
    
    ValueBinding(Type *type, llvm::Value *value, ReadImp read, WriteImp write, Flags F)
    : type(type), value(value), read(read), write(write), flags(F) {}
    
    ValueBinding(Type *type, llvm::Value *value, ReadImp read, WriteImp write, std::initializer_list<Flags> F)
    : type(type), value(value), read(read), write(write), flags(F) {}
};


struct ResolvedCallable {
    // TODO add a "kind of callable" field (global function, instance/static method, lambda, etc). that could also replace the argumentOffset field
    ast::FunctionSignature signature;
    std::shared_ptr<ast::FunctionDecl> funcDecl; // only nonnull if the callable is a function decl
    llvm::Value *llvmValue; // nullptr if this is a yet to be instantiated function template
    uint8_t argumentOffset;
    
    ResolvedCallable(ast::FunctionSignature sig, std::shared_ptr<ast::FunctionDecl> funcDecl, llvm::Value *llvmValue, uint8_t argumentOffset)
    : signature(sig), funcDecl(funcDecl), llvmValue(llvmValue), argumentOffset(argumentOffset) {}
    
    ResolvedCallable(std::shared_ptr<ast::FunctionDecl> funcDecl, llvm::Value *llvmValue, uint8_t argumentOffset)
    : signature(funcDecl->getSignature()), funcDecl(funcDecl), llvmValue(llvmValue), argumentOffset(argumentOffset) {}
};



// State of the function currently being generated
struct FunctionState {
    struct BreakContDestinations {
        llvm::BasicBlock *breakDest, *contDest;
    };
    std::shared_ptr<ast::FunctionDecl> decl = nullptr;
    llvm::Function *llvmFunction = nullptr;
    llvm::BasicBlock *returnBB = nullptr;
    llvm::Value *retvalAlloca = nullptr;
    uint64_t tmpIdentCounter = 0;
    util::NamedScope<ValueBinding>::Marker stackTopMarker = 0; // Beginning of function body
    std::stack<BreakContDestinations> breakContDestinations;
    
    FunctionState() {}
    FunctionState(std::shared_ptr<ast::FunctionDecl> decl, llvm::Function *llvmFunction, llvm::BasicBlock *returnBB, llvm::Value *retvalAlloca, util::NamedScope<ValueBinding>::Marker STM)
    : decl(decl), llvmFunction(llvmFunction), returnBB(returnBB), retvalAlloca(retvalAlloca), stackTopMarker(STM) {}
    
    std::string getTmpIdent() {
        return util::fmt::format("__tmp_{}", tmpIdentCounter++);
    }
    
    uint64_t getCounter() {
        return tmpIdentCounter++;
    }
};



enum class CandidateViabilityComparisonResult {
    MoreViable,
    LessViable,
    Ambiguous
};

/// A possible candidate for a call expression
struct FunctionCallTargetCandidate {
    using ID = uint64_t;
    
    ID id;
    ResolvedCallable target;
    std::vector<uint32_t> implicitConversionArgumentIndices; // args which need implicit converison
    TemplateTypeMapping templateArgumentMapping; // empty unless this is a function template
    
    FunctionCallTargetCandidate(ID id, const ResolvedCallable& RC) : id(id), target(RC) {}
    
    const ast::FunctionSignature& getSignature() const {
        return target.signature;
    }
    
    CandidateViabilityComparisonResult compare(IRGenerator &irgen, const FunctionCallTargetCandidate &other, std::shared_ptr<ast::CallExpr>, const std::vector<Type *> &argTys) const;
};





class IRGenerator {
    friend struct FunctionCallTargetCandidate;
    
    std::unique_ptr<llvm::Module> module;
    llvm::IRBuilder<> builder;
    
    // Debug Metadata
    struct {
        llvm::DIBuilder builder;
        llvm::DICompileUnit *compileUnit;
        std::vector<llvm::DIScope *> lexicalBlocks;
    } debugInfo;
    
    // Builtin Types
    struct {
        struct {
            llvm::IntegerType *i8, *i16, *i32, *i64, *i1;
            llvm::Type *Void, *Float, *Double;
            llvm::PointerType *i8Ptr;
        } llvm;
        struct {
            NumericalType *u8, *u16, *u32, *u64;
            NumericalType *i8, *i16, *i32, *i64, *Bool, *f32, *f64;
            Type *Void;
            PointerType *i8Ptr;
        } yo;
    } builtinTypes;
    
    const driver::Options &driverOptions;
    
    /// The local scope
    util::NamedScope<ValueBinding> localScope;
    
    // Finalized nominal types
    // If the type is a template specialization, the key is the mangled name
    util::NamedScope<Type *> nominalTypes;
    
    /// All registered non-template struct types
    std::map<std::string, std::shared_ptr<ast::StructDecl>> structDecls;
    std::map<std::string, std::shared_ptr<ast::StructDecl>> structTemplateDecls;
    
    // key: canonical function name
    std::map<std::string, std::vector<ResolvedCallable>> functions;
    // TODO: what's the point of `resolvedFunctions`?
    // key: fully resolved function name
    std::map<std::string, ResolvedCallable> resolvedFunctions;
    
    /// The function currently being generated
    irgen::FunctionState currentFunction;

    
public:
    static llvm::LLVMContext C;
    
    IRGenerator(const std::string& translationUnitPath, const driver::Options&);
    
    void codegen(const ast::AST&);
    
    std::unique_ptr<llvm::Module> getModule() {
        return std::move(module);
    }
    
    
    
private:
    void preflight(const ast::AST&);
    void registerFunction(std::shared_ptr<ast::FunctionDecl>);
    StructType* registerStructDecl(std::shared_ptr<ast::StructDecl>);
    void registerTypealias(std::shared_ptr<ast::TypealiasDecl>);
    
    
    // Debug Metadata
    void emitDebugLocation(const std::shared_ptr<ast::Node>&);
    
    // Whether irgen should emit debug metadata
    // Note: if we're in a function, this also takes the function's `no_debug_info` attribute into account
    bool shouldEmitDebugInfo() const {
        if (currentFunction.decl && currentFunction.decl->getAttributes().no_debug_info) {
            return false;
        }
        return driverOptions.emitDebugMetadata;
    }
    
    
    //
    // CODEGEN
    //
    
    llvm::Value *codegenTLS(std::shared_ptr<ast::TopLevelStmt>);
    llvm::Value *codegenLocalStmt(std::shared_ptr<ast::LocalStmt>);
    llvm::Value *codegenExpr(std::shared_ptr<ast::Expr>, ValueKind = RValue, bool insertImplicitLoadInst = true); // TODO should this really default to rvalue?
    
    // ast::TopLevelStmt
    llvm::Value *codegenFunctionDecl(std::shared_ptr<ast::FunctionDecl>);
    llvm::Value *codegenStructDecl(std::shared_ptr<ast::StructDecl>);
    
    // ast::LocalStmt
    llvm::Value *codegenCompoundStmt(std::shared_ptr<ast::CompoundStmt>);
    llvm::Value *codegenReturnStmt(std::shared_ptr<ast::ReturnStmt>);
    llvm::Value *codegenVarDecl(std::shared_ptr<ast::VarDecl>);
    llvm::Value *codegenAssignment(std::shared_ptr<ast::Assignment>);
    llvm::Value *codegenIfStmt(std::shared_ptr<ast::IfStmt>);
    llvm::Value *codegenWhileStmt(std::shared_ptr<ast::WhileStmt>);
    llvm::Value *codegenForLoop(std::shared_ptr<ast::ForLoop>);
    llvm::Value *codegenBreakContStmt(std::shared_ptr<ast::BreakContStmt>);
    llvm::Value *codegenExprStmt(std::shared_ptr<ast::ExprStmt>);
    
    // ast::Expr
    llvm::Value *codegenNumberLiteral(std::shared_ptr<ast::NumberLiteral>, ValueKind);
    llvm::Value *codegenStringLiteral(std::shared_ptr<ast::StringLiteral>, ValueKind);
    llvm::Value *codegenCastExpr(std::shared_ptr<ast::CastExpr>, ValueKind);
    llvm::Value *codegenUnaryExpr(std::shared_ptr<ast::UnaryExpr>, ValueKind);
    llvm::Value *codegenIdent(std::shared_ptr<ast::Ident>, ValueKind);
    llvm::Value *codegenRawLLVMValueExpr(std::shared_ptr<ast::RawLLVMValueExpr>, ValueKind);
    llvm::Value *codegenBinOp(std::shared_ptr<ast::BinOp>, ValueKind);
    llvm::Value *codegenSubscriptExpr(std::shared_ptr<ast::SubscriptExpr>, ValueKind);
    llvm::Value *codegenMemberExpr(std::shared_ptr<ast::MemberExpr>, ValueKind);
    llvm::Value *codegenCallExpr(std::shared_ptr<ast::CallExpr>, ValueKind);
    llvm::Value *codegenLambdaExpr(std::shared_ptr<ast::LambdaExpr>, ValueKind);
    llvm::Value *codegenArrayLiteralExpr(std::shared_ptr<ast::ArrayLiteralExpr>, ValueKind);
    llvm::Value *codegenTupleExpr(std::shared_ptr<ast::TupleExpr>, ValueKind);
    llvm::Value *codegenMatchExpr(std::shared_ptr<ast::MatchExpr>, ValueKind);
    
    // Intrinsics
    llvm::Value *codegen_HandleIntrinsic(std::shared_ptr<ast::FunctionDecl>, std::shared_ptr<ast::CallExpr>);
    llvm::Value *codegen_HandleArithmeticIntrinsic(ast::Operator, std::shared_ptr<ast::CallExpr>);
    llvm::Value *codegen_HandleComparisonIntrinsic(ast::Operator, std::shared_ptr<ast::CallExpr>);
    llvm::Value *codegen_HandleLogOpIntrinsic(Intrinsic, std::shared_ptr<ast::CallExpr>);
    
    
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
    llvm::Type* getLLVMType(Type *);
    llvm::DIType* getDIType(Type *);
    llvm::DISubroutineType* toDISubroutineType(const ast::FunctionSignature&);
    uint8_t argumentOffsetForCallingConvention(CallingConvention cc);
    Type* resolvePrimitiveType(std::string_view name);
    
    bool isTemporary(std::shared_ptr<ast::Expr>);
    
    bool typeIsConstructible(Type *);
    bool typeIsDestructible(Type *);
    bool typeIsSubscriptable(Type *);
    
    /// Whether a specific instance method can be invoked on a value of a type
    bool implementsInstanceMethod(Type *, std::string_view);
    
    llvm::Value* constructStruct(StructType *, std::shared_ptr<ast::CallExpr> ctorCall, bool putInLocalScope, ValueKind);
    llvm::Value* constructCopyIfNecessary(Type *, std::shared_ptr<ast::Expr>, bool *didConstructCopy = nullptr);
    
    /// Destructs a value by invoking the type's `dealloc` method, if defined
    /// value parameter should be the value's memory location
    llvm::Value* destructValueIfNecessary(Type *, llvm::Value *, bool includeReferences);
    
    /// Creates a call to the type's `dealloc` function, if defined
    /// Returns nullptr if the type does not have a `dealloc` method
    std::shared_ptr<ast::LocalStmt> createDestructStmtIfDefined(Type *, std::shared_ptr<ast::Expr>, bool includeReferences);
    std::shared_ptr<ast::LocalStmt> createDestructStmtIfDefined(Type *, llvm::Value *, bool includeReferences);
    
    /// Put the value into the local scope (thus including it in stack cleanup destructor calls)
    void includeInStackDestruction(Type *, llvm::Value *);
    
    void destructLocalScopeUntilMarker(util::NamedScope<ValueBinding>::Marker, bool removeFromLocalScope);
    
    
    
    
    // Applying trivial number literal casts
    
    // The following 2 methods to two things:
    // 1. Check whether the type of `Expr` is the same as `ExpectedType`
    // 2. Apply a trivial cast to Expr, if and only if Expr is a number literal that can fit in the expected type
    // Version 2 does the same thing, but for binops. if necessary, it might cast either `Lhs` or `Rhs` to the type of `Rhs` or `Lhs` (respectively)
    // both return true on success
    
    
    // `{Lhs|Rhs}Ty`: type of lhs/rhs, after applying typecasts, if casts were applied
    bool typecheckAndApplyTrivialNumberTypeCastsIfNecessary_binop(std::shared_ptr<ast::Expr> *lhs, std::shared_ptr<ast::Expr> *rhs, Type **lhsTy, Type **rhsTy);
    
    bool isImplicitConversionAvailable(Type *src, Type *dst);
    
    // Returns false if there is no implicit conversion to the expected type
    bool applyImplicitConversionIfNecessary(std::shared_ptr<ast::Expr> &expr, Type *expectedType);
    
    
    // Other stuff
    std::optional<ResolvedCallable> getResolvedFunctionWithName(const std::string &name);
        
    // Whether a function should skip actual codegen
    enum SkipCodegenOption {
        kRunCodegen,
        kSkipCodegen
    };
    
    ResolvedCallable specializeTemplateFunctionDeclForCallExpr(std::shared_ptr<ast::FunctionDecl>, TemplateTypeMapping, uint8_t argumentOffset, SkipCodegenOption);
    ResolvedCallable resolveCall(std::shared_ptr<ast::CallExpr>, SkipCodegenOption);
    
    
    TemplateTypeMapping resolveStructTemplateParametersFromExplicitTemplateArgumentList(std::shared_ptr<ast::StructDecl>, std::shared_ptr<ast::TemplateParamArgList>);
    
    std::optional<TemplateTypeMapping>
    attemptToResolveTemplateArgumentTypesForCall(const ast::FunctionSignature&, std::shared_ptr<ast::CallExpr> call, unsigned argumentOffset);
    
    /// Instantiate a template struct, using a fully resolved typeDesc to resolve the template parameters
    StructType* instantiateTemplateStruct(std::shared_ptr<ast::StructDecl>, std::shared_ptr<ast::TypeDesc>);
    /// Instantiate a template struct from a type mapping
    StructType* instantiateTemplateStruct(std::shared_ptr<ast::StructDecl>, const TemplateTypeMapping&);
    
    Type* getType(std::shared_ptr<ast::Expr>);
    
    bool valueIsTriviallyConvertible(std::shared_ptr<ast::NumberLiteral>, Type *);
    
    bool equal(const ast::FunctionSignature &lhs, const ast::FunctionSignature &rhs);
    
    
    // Globals etc
    void handleStartupAndShutdownFunctions();
    
    
    // Types
    
    llvm::Value* synthesizeDefaultMemberwiseInitializer(std::shared_ptr<ast::StructDecl>, SkipCodegenOption);
    llvm::Value* synthesizeDefaultCopyConstructor(std::shared_ptr<ast::StructDecl>, SkipCodegenOption);
    llvm::Value* synthesizeDefaultDeallocMethod(std::shared_ptr<ast::StructDecl>, SkipCodegenOption);
    
    StructType* synthesizeLambdaExpr(std::shared_ptr<ast::LambdaExpr>);
    StructType* synthesizeUnderlyingStructTypeForTupleType(TupleType *tupleTy);
    
    
    
    // Utils
    
    // TODO this seems like a bad idea?
    // Assuming this is only ever used for registering synthesized functions, what about just having a `queueSynthFunction` function that registers the llvm::Function, and then puts the FuncDecl in a queue which is handled after regular codegen finished?
    template <typename F>
    std::invoke_result_t<F> withCleanSlate(F &&fn) {
        // TODO this also need to take debugInfo.lexicalBlocks into account !!!!!!!!!!
        auto prevLocalScope = localScope;
        auto prevCurrentFunction = currentFunction;
        auto prevInsertBlock = builder.GetInsertBlock();
        
        localScope = {};
        currentFunction = {};
        
        util::DeferHandle H([&]() {
            localScope = prevLocalScope;
            currentFunction = prevCurrentFunction;
            builder.SetInsertPoint(prevInsertBlock);
        });
        
        return fn();
    }
};

NS_END
