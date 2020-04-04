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
#include "NameLookup.h"
#include "MatchMaker.h"
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
#include <utility>


namespace yo::irgen {

enum class Intrinsic : uint8_t;
class IRGenerator;

using TemplateTypeMapping = std::map<std::string, std::shared_ptr<ast::TypeDesc>>;

inline void dump_tmpl_mapping(const TemplateTypeMapping &M) {
    util::fmt::print("Template Type Mapping:");
    for (const auto &[name, typeDesc] : M) {
        util::fmt::print("- {}: {}", name, typeDesc);
    }
}


StructType* getUnderlyingStruct(Type *ty);
std::string mangleFullyResolved(const std::shared_ptr<ast::FunctionDecl>&);
bool integerLiteralFitsInIntegralType(uint64_t, Type *);
llvm::DIFile* DIFileForSourceLocation(llvm::DIBuilder&, const lex::SourceLocation&);
std::shared_ptr<ast::CallExpr> subscriptExprToCall(std::shared_ptr<ast::SubscriptExpr>);
std::shared_ptr<ast::Ident> makeIdent(const std::string&, lex::SourceLocation = lex::SourceLocation());

inline std::string formatTupleMemberAtIndex(size_t index) {
    return util::fmt::format("__{}", index);
}


// TODO is it a good idea to put these here?
inline constexpr unsigned kInstanceMethodCallArgumentOffset = 1;
static const std::string kInitializerMethodName = "init";
static const std::string kDeallocMethodName = "dealloc";
static const std::string kSynthesizedDeallocMethodName = "__dealloc";
static const std::string kRetvalAllocaIdentifier = "__retval";
static const std::string kIteratorMethodName = "iterator";
static const std::string kIteratorHasNextMethodName = "hasNext";
static const std::string kIteratorNextMethodName = "next";




enum ValueKind : uint8_t { // TODO rename to ValueCategory?
    LValue, RValue
};

// Whether a function should skip actual codegen
enum SkipCodegenOption {
    kRunCodegen,
    kSkipCodegen
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
    ast::FunctionSignature signature; // TODO can this be a `FunctionType *` instead?
    std::shared_ptr<ast::FunctionDecl> funcDecl; // only nonnull if the callable is a function decl
    llvm::Value *llvmValue; // nullptr if this is a yet to be instantiated function template
    bool hasImplicitSelfArg;
    
    ResolvedCallable(ast::FunctionSignature sig, std::shared_ptr<ast::FunctionDecl> funcDecl, llvm::Value *llvmValue, bool hasImplicitSelfArg)
    : signature(sig), funcDecl(funcDecl), llvmValue(llvmValue), hasImplicitSelfArg(hasImplicitSelfArg) {}
    
    ResolvedCallable(std::shared_ptr<ast::FunctionDecl> funcDecl, llvm::Value *llvmValue, bool hasImplicitSelfArg)
    : signature(funcDecl->getSignature()), funcDecl(funcDecl), llvmValue(llvmValue), hasImplicitSelfArg(hasImplicitSelfArg) {}
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
    
    CandidateViabilityComparisonResult compare(IRGenerator &irgen, const FunctionCallTargetCandidate &other, const std::vector<std::pair<Type *, std::shared_ptr<ast::Expr>>>&) const;
};




/// a named decl is a named declaration
struct NamedDeclInfo {
    bool isRegistered = false;
    std::shared_ptr<ast::TopLevelStmt> decl;
    Type *type = nullptr;
    llvm::Value *llvmValue = nullptr; // only nonnull for functions?
    
    NamedDeclInfo(std::shared_ptr<ast::TopLevelStmt> decl) : decl(decl) {}
};




class IRGenerator {
public:
    // this is public since some of the temp-clean-scope stuff is declared outside the irgen class
    class EmptyScopeHandle;

private:
    friend class NameLookup;
    friend class EmptyScopeHandle;
    friend class MatchMaker;
    friend struct FunctionCallTargetCandidate;
    
    ast::AST &ast;
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
    
    // key: canonical name
    std::map<std::string, std::vector<NamedDeclInfo>> namedDeclInfos;
    
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
    
    IRGenerator(ast::AST&, const std::string& translationUnitPath, const driver::Options&);
    
    IRGenerator(const IRGenerator&) = delete;
    IRGenerator& operator=(const IRGenerator&) = delete;
    
    void runCodegen();
    
    std::unique_ptr<llvm::Module> getModule() {
        return std::move(module);
    }
    
    
private:
    void preflight();
    void preflightImplBlock(std::shared_ptr<ast::ImplBlock>);
    
    void registerNamedDecl(NamedDeclInfo &);
    llvm::Function* registerFunction(std::shared_ptr<ast::FunctionDecl>, NamedDeclInfo &);
    StructType* registerStructDecl(std::shared_ptr<ast::StructDecl>, NamedDeclInfo &);
    void registerTypealias(std::shared_ptr<ast::TypealiasDecl>, NamedDeclInfo &);
    VariantType* registerVariantDecl(std::shared_ptr<ast::VariantDecl>, NamedDeclInfo &);
    
    llvm::Function* addAndRegisterFunction(std::shared_ptr<ast::FunctionDecl> decl) {
        ast.push_back(decl);
        auto &info = namedDeclInfos[decl->getName()].emplace_back(decl);
        return registerFunction(decl, info);
    }
    
    StructType* addAndRegisterStructDecl(std::shared_ptr<ast::StructDecl> decl) {
        ast.push_back(decl);
        auto &info = namedDeclInfos[decl->name].emplace_back(decl);
        return registerStructDecl(decl, info);
    }
    
    /// register all names decls for a given name
    void registerNamedDecls(const std::string &name) {
        for (auto &[declName, declInfos] : namedDeclInfos) {
            if (declName != name) continue;
            for (auto &declInfo : declInfos) {
                registerNamedDecl(declInfo);
            }
        }
    }
    
    /// register all names decls for a given name, depending on a predicate
    template <typename F>
    void registerNamedDecls(const std::string &name, F &&fn) {
        for (auto &[declName, declInfos] : namedDeclInfos) {
            if (declName != name) continue;
            for (auto &declInfo : declInfos) {
                if (fn(declInfo.decl)) {
                    registerNamedDecl(declInfo);
                }
            }
        }
    }
    
    
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
    llvm::Value *codegenSubscriptExpr(std::shared_ptr<ast::SubscriptExpr>, ValueKind, SkipCodegenOption = kRunCodegen, Type ** = nullptr);
    llvm::Value *codegenMemberExpr(std::shared_ptr<ast::MemberExpr>, ValueKind, SkipCodegenOption = kRunCodegen, Type ** = nullptr);
    llvm::Value *codegenCallExpr(std::shared_ptr<ast::CallExpr>, ValueKind);
    llvm::Value *codegenLambdaExpr(std::shared_ptr<ast::LambdaExpr>, ValueKind);
    llvm::Value *codegenArrayLiteralExpr(std::shared_ptr<ast::ArrayLiteralExpr>, ValueKind);
    llvm::Value *codegenTupleExpr(std::shared_ptr<ast::TupleExpr>, ValueKind);
    llvm::Value *codegenMatchExpr(std::shared_ptr<ast::MatchExpr>, ValueKind);
    
    /// codegen the expr as a boolean comparison
    /// always returns an rvalue
    llvm::Value *codegenBoolComp(std::shared_ptr<ast::Expr>);
    
    // Intrinsics
    llvm::Value *codegen_HandleIntrinsic(std::shared_ptr<ast::FunctionDecl>, std::shared_ptr<ast::CallExpr>);
    llvm::Value *codegen_HandleArithmeticIntrinsic(ast::Operator, std::shared_ptr<ast::CallExpr>);
    llvm::Value *codegen_HandleComparisonIntrinsic(ast::Operator, std::shared_ptr<ast::CallExpr>);
    llvm::Value *codegen_HandleLogOpIntrinsic(Intrinsic, std::shared_ptr<ast::CallExpr>);
    
    
//    struct MatchExprPatternCodegenInfo {
//        Type *targetType; // type of the expression we're matching against
//        std::shared_ptr<ast::Expr> targetExpr; // the expression we're matching against
//        llvm::Value *targetLLVMValue; // Codegen result for TargetExpr
//        std::shared_ptr<ast::Expr> patternExpr;
//    };
//    // TODO? this should be the central point that handles all pattern checks and returns the correct expressions, based on the input types
//    llvm::Value *codegen_HandleMatchPatternExpr(MatchExprPatternCodegenInfo);
    
    
    // Types
    Type* resolveTypeDesc(std::shared_ptr<ast::TypeDesc>, bool setInternalResolvedType = true);
    llvm::Type* getLLVMType(Type *);
    llvm::DIType* getDIType(Type *);
    llvm::DISubroutineType* toDISubroutineType(const ast::FunctionSignature&);
    Type* resolvePrimitiveType(std::string_view name);
    
    bool isTemporary(std::shared_ptr<ast::Expr>);
    
    bool typeIsConstructible(Type *);
    bool typeIsCopyConstructible(Type *);
    bool typeIsDestructible(Type *);
    
    
    llvm::Value* constructStruct(StructType *, std::shared_ptr<ast::CallExpr> ctorCall, bool putInLocalScope, ValueKind);
    llvm::Value* constructCopyIfNecessary(Type *, std::shared_ptr<ast::Expr>, bool *didConstructCopy = nullptr);
    
    llvm::Value* constructVariant(VariantType *, const std::string &elementName);
    
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
    
    ResolvedCallable specializeTemplateFunctionDeclForCallExpr(std::shared_ptr<ast::FunctionDecl>, TemplateTypeMapping, bool hasImplicitSelfArg, SkipCodegenOption);
    
    struct CallTargetRejectionReason {
        std::string reason;
        std::shared_ptr<ast::FunctionDecl> decl;
        
        CallTargetRejectionReason(std::string reason, std::shared_ptr<ast::FunctionDecl> decl) : reason(reason), decl(decl) {}
    };
    
    enum class ResolveCallResultStatus {
        Success,
        NoCandidates,
        AmbiguousCandidates
    };
    std::optional<ResolvedCallable> resolveCall_imp(std::shared_ptr<ast::CallExpr>, SkipCodegenOption,
                                                    std::vector<FunctionCallTargetCandidate>&,
                                                    std::vector<CallTargetRejectionReason>&, ResolveCallResultStatus&, bool lookingForZeroResults);
    ResolvedCallable resolveCall(std::shared_ptr<ast::CallExpr>, SkipCodegenOption);
    
    std::optional<ResolvedCallable> resolveCall_opt(std::shared_ptr<ast::CallExpr>, SkipCodegenOption);
    
    // returns true if the call can be resolved, otherwise false.
    // does not emit IR for the call?
    bool canResolveCall(std::shared_ptr<ast::CallExpr> expr) {
        return resolveCall_opt(expr, kSkipCodegen).has_value();
    }
    
    
    TemplateTypeMapping resolveStructTemplateParametersFromExplicitTemplateArgumentList(std::shared_ptr<ast::StructDecl>, std::shared_ptr<ast::TemplateParamArgList>);
    
    std::optional<TemplateTypeMapping>
    attemptToResolveTemplateArgumentTypesForCall(const ast::FunctionSignature&, std::shared_ptr<ast::CallExpr>, const std::vector<std::pair<Type *, std::shared_ptr<ast::Expr>>>&);
    
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
    
    // basically, whether `targetTy().name(argTys...)` exists
    bool memberFunctionCallResolves(Type *targetTy, const std::string &name, const std::vector<Type *> &argTys);
    
    StructType* synth_getStructDeclStructType(const std::shared_ptr<ast::StructDecl>&);
    
    llvm::Value* synthesizeDefaultMemberwiseInitializer(std::shared_ptr<ast::StructDecl>, SkipCodegenOption);
    llvm::Value* synthesizeDefaultCopyConstructor(std::shared_ptr<ast::StructDecl>, SkipCodegenOption);
    llvm::Value* synthesizeDefaultDeallocMethod(std::shared_ptr<ast::StructDecl>, SkipCodegenOption);
    
    StructType* synthesizeLambdaExpr(std::shared_ptr<ast::LambdaExpr>);
    StructType* synthesizeUnderlyingStructTypeForTupleType(TupleType *tupleTy);
    
    void synthesizeVariantConstructor(VariantType *, const VariantType::Elements::value_type &);
};


class IRGenerator::EmptyScopeHandle {
    // TODO this also need to take debugInfo.lexicalBlocks into account !!!!!!!!!!
    IRGenerator &irgen;
    decltype(irgen.localScope) prevLocalScope;
    decltype(irgen.currentFunction) prevCurrentFunction;
    decltype(irgen.builder.GetInsertBlock()) prevInsertBlock;
    
public:
    explicit EmptyScopeHandle(IRGenerator &irgen)
    : irgen(irgen), prevLocalScope(irgen.localScope), prevCurrentFunction(irgen.currentFunction), prevInsertBlock(irgen.builder.GetInsertBlock()) {
        irgen.localScope = {};
        irgen.currentFunction = {};
    }
    
    ~EmptyScopeHandle() {
        irgen.localScope = prevLocalScope;
        irgen.currentFunction = prevCurrentFunction;
        irgen.builder.SetInsertPoint(prevInsertBlock);
    }
    
    
    /// Invokes the lambda with a clean scope, and returns whatever the lambda returns
    template <typename F>
    static std::invoke_result_t<F> invoke(IRGenerator &irgen, F &&fn) {
        EmptyScopeHandle ESH(irgen);
        return fn();
    }
};


// TODO remove this and use EmptyScopeHandle::invoke instead
template <typename F>
std::invoke_result_t<F> withCleanSlate(IRGenerator &irgen, F &&fn) {
    IRGenerator::EmptyScopeHandle ESH(irgen);
    return fn();
}

}
