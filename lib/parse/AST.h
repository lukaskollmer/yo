//
//  AST.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-03.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "lex/SourceLocation.h"
#include "TypeDesc.h"
#include "Attributes.h"
#include "util/util.h"

#include <memory>
#include <iostream>
#include <variant>
#include <string>
#include <vector>
#include <map>


namespace llvm {
class Value;
}

namespace yo::irgen {
class Type;
class StructType;
}


NS_START(yo::ast)


class TopLevelStmt;
using AST = std::vector<std::shared_ptr<TopLevelStmt>>;
using lex::SourceLocation;

std::string description(const AST&);


enum class Operator : uint8_t {
    /* +  */ Add,
    /* -  */ Sub,
    /* *  */ Mul,
    /* /  */ Div,
    /* %  */ Mod,
    /* &  */ And,
    /* |  */ Or,
    /* ^  */ Xor,
    /* << */ Shl,
    /* >> */ Shr,
    /* -  */ Neg,
    /* ~  */ BNot,  // bitwise not
    /* !  */ BNeg,  // boolean negation
    
    /* && */ LAnd,  // logical and
    /* || */ LOr,   // logical or
    
    /* == */ EQ,
    /* != */ NE,
    /* < */  LT,
    /* <= */ LE,
    /* > */  GT,
    /* >= */ GE,
    /*..< */ CR_Exc, // exclusive closed range
    /*... */ CR_Inc, // inclusive closed range
    
    /* |> */ FnPipe,
    
    /* () */ FnCall,
    /* [] */ Subscript,
    
    
    /* =  */ Assign,
};



class Expr;
class Ident;
class VarDecl;
class CompoundStmt;
class StructDecl;
class ImplBlock;



class Node {
public:
    enum class Kind {
        // Top Level Statements
        FunctionDecl, ImplBlock, StructDecl, TypealiasDecl, VariantDecl,
        
        // Local Statements
        Assignment, CompoundStmt, ExprStmt, ForLoop, IfStmt, ReturnStmt, VarDecl, WhileStmt, BreakContStmt,
        
        // Expressions
        BinOp, CallExpr, CompOp, Ident, LogicalOp, MatchExpr, MemberExpr, NumberLiteral, RawLLVMValueExpr, StaticDeclRefExpr,
        StringLiteral, SubscriptExpr, CastExpr, UnaryExpr, LambdaExpr, ArrayLiteralExpr, TupleExpr,
        
        TemplateParamDeclList, TemplateParamArgList, FunctionSignature, IfStmtBranch, MatchExprBranch, MatchExprPattern
    };
    
    Kind getKind() const { return kind; }
    bool isOfKind(Kind NK) const { return kind == NK; }
    
    // TODO add a `str` function that prints the node, as it would look as source code?
    virtual std::string description() const; // TODO remove in favor of `ast::description`
    const SourceLocation& getSourceLocation() const {
        return sourceLocation;
    }
    
    void setSourceLocation(const SourceLocation &sourceLoc) {
        this->sourceLocation = sourceLoc;
    }
    
protected:
    Node(Kind kind) : kind(kind) {}
    virtual ~Node() = default;
    
private:
    Kind kind;
    SourceLocation sourceLocation;
};


std::string nodeKindToString(Node::Kind);


class TopLevelStmt : public Node {
protected:
    TopLevelStmt(Kind kind) : Node(kind) {}
};

class LocalStmt : public Node {
protected:
    LocalStmt(Kind kind) : Node(kind) {}
};

class Expr : public Node {
protected:
    Expr(Kind kind) : Node(kind) {}

public:
    /// true if we know for a fact that this expression will always evaluate to a temporary value
    /// TODO introducing this variable is a terrible fix for this problem and it should be removed asap
    bool isKnownAsTemporary = false;
};


// for llvm
#define CLASSOF_IMP(NK) static bool classof(const Node *node) { return node->getKind() == NK; }



#pragma mark - Top Level Statements


enum class FunctionKind {
    GlobalFunction,   // A free global function
    StaticMethod,     // A static type member method
    InstanceMethod    // A type instance method
};



/// A list of template parameters, for example for a struct or function declaration
class TemplateParamDeclList : public Node {
public:
    struct Param {
        std::shared_ptr<Ident> name;
        std::shared_ptr<ast::TypeDesc> defaultType;
        
        explicit Param(std::shared_ptr<Ident> name, std::shared_ptr<ast::TypeDesc> defaultType = nullptr) : name(name), defaultType(defaultType) {}
    };
    
private:
    std::vector<Param> elements;
    
public:
    CLASSOF_IMP(Node::Kind::TemplateParamDeclList)
    TemplateParamDeclList() : Node(Node::Kind::TemplateParamDeclList) {}
    
    void addParam(Param P) {
        elements.push_back(P);
    }
    void setParams(std::vector<Param> E) {
        elements = E;
    }
    const std::vector<Param>& getParams() const {
        return elements;
    }
    bool isEmpty() const {
        return elements.empty();
    }
    size_t size() const {
        return elements.size();
    }
};



class TemplateParamArgList : public Node {
public:
    std::vector<std::shared_ptr<ast::TypeDesc>> elements;
    
    CLASSOF_IMP(Node::Kind::TemplateParamArgList)
    TemplateParamArgList() : Node(Node::Kind::TemplateParamArgList) {}
    
    size_t size() const {
        return elements.size();
    }
    
    bool isEmpty() const {
        return size() == 0;
    }
    
    std::shared_ptr<ast::TypeDesc> at(uint64_t idx) const {
        return elements.at(idx);
    }
};




// Helper class used for implementing functionality common to all declarations which can be templates
class TemplateDecl {
public:
    std::shared_ptr<TemplateParamDeclList> templateParamsDecl;
    std::vector<irgen::Type *> templateInstantiationArguments;
    
    bool isTemplateDecl() const {
        LKAssertImplication(templateParamsDecl, templateParamsDecl->size() > 0);
        return templateParamsDecl != nullptr;
    }
    
    bool isInstantiatedTemplateDecl() const {
        return !templateInstantiationArguments.empty();
    }
    
    size_t numberOfTemplateParameters() const {
        if (!templateParamsDecl) {
            return 0;
        } else {
            return templateParamsDecl->size();
        }
    }
};




class FunctionSignature : public Node, public TemplateDecl {
public:
    std::shared_ptr<TypeDesc> returnType;
    std::vector<std::shared_ptr<TypeDesc>> paramTypes;
    bool isVariadic = false;

    CLASSOF_IMP(Node::Kind::FunctionSignature)
    FunctionSignature() : Node(Node::Kind::FunctionSignature) {}
    
    size_t numberOfParameters() const {
        return paramTypes.size();
    }
};

std::ostream& operator<<(std::ostream&, const ast::FunctionSignature&);






/// A Function Declaration
class FunctionDecl : public TopLevelStmt {
public:
    FunctionSignature signature;
    std::vector<std::shared_ptr<ast::Ident>> paramNames;
    std::shared_ptr<ast::CompoundStmt> body;
    attributes::FunctionAttributes attributes;
    
    FunctionKind funcKind;
    std::string name;
    
    // TODO remove this!!!
    irgen::StructType *implType = nullptr; // Only nonnull if this is a type member function
    
    /// whether the function was declared as part of a templated impl block, and, as such, had additional parameters inserted into its template parameter list
    bool hasInsertedImplBlockTemplateParams = false;
    
    /// if this is a function declared w/in an impl block with template parameters, the start index of the impl block's template parameters
    size_t implBlockTmplParamsStartIndex = 0;

    
public:
    CLASSOF_IMP(Node::Kind::FunctionDecl)
    FunctionDecl(FunctionKind kind, std::string name, FunctionSignature sig, attributes::FunctionAttributes attr)
    : TopLevelStmt(Node::Kind::FunctionDecl), signature(sig), body(std::make_shared<CompoundStmt>()), attributes(attr), funcKind(kind), name(name) {}
    
    FunctionKind getFunctionKind() const { return funcKind; }
    void setFunctionKind(FunctionKind kind) { funcKind = kind; }
    
    const std::string& getName() const { return name; }
    
    FunctionSignature& getSignature() { return signature; }
    const FunctionSignature& getSignature() const { return signature; }
    
    irgen::StructType* getImplType() const { return implType; }
    void setImplType(irgen::StructType *ty) { implType = ty; }
    
    
    const std::vector<std::shared_ptr<Ident>>& getParamNames() const { return paramNames; }
    void setParamNames(std::vector<std::shared_ptr<Ident>> names) { paramNames = names; }
    
    attributes::FunctionAttributes& getAttributes() { return attributes; }
    const attributes::FunctionAttributes& getAttributes() const { return attributes; }
    
    const std::shared_ptr<ast::CompoundStmt>& getBody() const { return body; }
    void setBody(std::shared_ptr<ast::CompoundStmt> B) { body = B; }
    
    bool isOfFunctionKind(FunctionKind kind) const {
        return funcKind == kind;
    }
    
    bool isGlobalFunction() const {
        return funcKind == FunctionKind::GlobalFunction;
    }
    bool isInstanceMethod() const {
        return funcKind == FunctionKind::InstanceMethod;
    }
    bool isStaticMethod() const {
        return funcKind == FunctionKind::StaticMethod;
    }
    
    bool isOperatorOverload() const;
    bool isOperatorOverloadFor(Operator) const;
    
    bool isCallOperatorOverload() const {
        return isOperatorOverloadFor(Operator::FnCall);
    }
    
    bool isSubscriptOperatorOverload() const {
        return isOperatorOverloadFor(Operator::Subscript);
    }
};





class StructDecl : public TopLevelStmt, public TemplateDecl {
public:
    std::string name;
    std::vector<std::shared_ptr<VarDecl>> members;
    attributes::StructAttributes attributes;
    
    irgen::StructType *type = nullptr; // the irgen type for this struct decl
    
    CLASSOF_IMP(Node::Kind::StructDecl)
    StructDecl() : TopLevelStmt(Node::Kind::StructDecl) {}
    
    const std::string& getName() const { return name; }
};




class ImplBlock : public TopLevelStmt, public TemplateDecl {
public:
    std::shared_ptr<ast::TypeDesc> typeDesc;
    std::vector<std::shared_ptr<FunctionDecl>> methods;
    bool isNominalTemplateType = false;

    CLASSOF_IMP(Node::Kind::ImplBlock)
    ImplBlock(std::shared_ptr<ast::TypeDesc> typeDesc) : TopLevelStmt(Node::Kind::ImplBlock), typeDesc(typeDesc) {}
};





class TypealiasDecl : public TopLevelStmt {
public:
    std::string name;
    std::shared_ptr<TypeDesc> type;
    
    CLASSOF_IMP(Node::Kind::TypealiasDecl)
    TypealiasDecl(std::string name, std::shared_ptr<TypeDesc> type) : TopLevelStmt(Node::Kind::TypealiasDecl), name(name), type(type) {}
};



/// A variant type declaration
class VariantDecl : public TopLevelStmt, public TemplateDecl {
public:
    struct MemberDecl {
        std::shared_ptr<Ident> name;
        std::shared_ptr<TypeDesc> params; // Should be a tuple type (TODO!)
        
        MemberDecl(std::shared_ptr<Ident> N, std::shared_ptr<TypeDesc> P) : name(N), params(P) {}
    };
    
    std::shared_ptr<Ident> name;
//    std::shared_ptr<TemplateParamDeclList> templateParams;
    std::vector<MemberDecl> members;
    
    CLASSOF_IMP(Node::Kind::VariantDecl)
    VariantDecl(std::shared_ptr<Ident> N) : TopLevelStmt(Node::Kind::VariantDecl), name(N) {}
};



# pragma mark - Local Statements


// TOOD rename to CompoundStmt?
class CompoundStmt : public LocalStmt {
public:
    std::vector<std::shared_ptr<LocalStmt>> statements;
    
    CLASSOF_IMP(Node::Kind::CompoundStmt)
    CompoundStmt() : LocalStmt(Node::Kind::CompoundStmt) {}
    CompoundStmt(std::vector<std::shared_ptr<LocalStmt>> statements) : LocalStmt(Node::Kind::CompoundStmt), statements(statements) {}
    
    bool isEmpty() const {
        return statements.empty();
    }
};


class ReturnStmt : public LocalStmt {
public:
    std::shared_ptr<Expr> expr;
    
    CLASSOF_IMP(Node::Kind::ReturnStmt)
    explicit ReturnStmt(std::shared_ptr<Expr> expr) : LocalStmt(Node::Kind::ReturnStmt), expr(expr) {}
};



// TODO rename to VarDecl?
class VarDecl : public LocalStmt {
public:
    std::shared_ptr<Ident> ident;
    std::shared_ptr<TypeDesc> type;
    std::shared_ptr<Expr> initialValue;
    
    // eg `let &x = y;`
    // Requires `type` to be nil
    bool declaresUntypedReference = false;
    
    CLASSOF_IMP(Node::Kind::VarDecl)
    VarDecl(std::shared_ptr<Ident> ident, std::shared_ptr<TypeDesc> type, std::shared_ptr<Expr> initialValue = nullptr)
    : LocalStmt(Node::Kind::VarDecl), ident(ident), type(type), initialValue(initialValue) {}
    
    const std::string& getName() const;
};


class Assignment : public LocalStmt {
public:
    std::shared_ptr<Expr> target;
    std::shared_ptr<Expr> value;
    
    bool shouldDestructOldValue = true;
    bool overwriteReferences = false;
    
    CLASSOF_IMP(Node::Kind::Assignment)
    Assignment(std::shared_ptr<Expr> target, std::shared_ptr<Expr> value) : LocalStmt(Node::Kind::Assignment), target(target), value(value) {}
};



class IfStmt : public LocalStmt {
public:
    class Branch : public Node {
    public:
        enum class BranchKind {
            If, ElseIf, Else
        };
        
        BranchKind kind;
        std::shared_ptr<Expr> condition; // nullptr if Kind == BranchKind::Else
        std::shared_ptr<CompoundStmt> body;
        
        CLASSOF_IMP(Node::Kind::IfStmtBranch)
        Branch(BranchKind kind, std::shared_ptr<Expr> condition, std::shared_ptr<CompoundStmt> body)
        : Node(Node::Kind::IfStmtBranch), kind(kind), condition(condition), body(body) {}
    };
    
    std::vector<std::shared_ptr<Branch>> branches;
    
    CLASSOF_IMP(Node::Kind::IfStmt)
    IfStmt(std::vector<std::shared_ptr<Branch>> branches) : LocalStmt(Node::Kind::IfStmt), branches(branches) {}
};


class WhileStmt : public LocalStmt {
public:
    std::shared_ptr<ast::Expr> condition;
    std::shared_ptr<ast::CompoundStmt> body;
    
    CLASSOF_IMP(Node::Kind::WhileStmt)
    WhileStmt(std::shared_ptr<Expr> condition, std::shared_ptr<CompoundStmt> body) : LocalStmt(Node::Kind::WhileStmt), condition(condition), body(body) {}
};


class ForLoop : public LocalStmt {
public:
    bool capturesByReference = false;
    std::shared_ptr<ast::Ident> ident;
    std::shared_ptr<ast::Expr> expr;
    std::shared_ptr<ast::CompoundStmt> body;
    
    CLASSOF_IMP(Node::Kind::ForLoop)
    ForLoop(std::shared_ptr<ast::Ident> ident, std::shared_ptr<ast::Expr> expr, std::shared_ptr<ast::CompoundStmt> body)
    : LocalStmt(Node::Kind::ForLoop), ident(ident), expr(expr), body(body) {}
};



// TODO put break and continue in a single class?
// A `break` or `continue` statement
class BreakContStmt : public LocalStmt {
public:
    enum class Kind { Break, Continue };
    const Kind kind;
    
    CLASSOF_IMP(Node::Kind::BreakContStmt)
    BreakContStmt(Kind K) : LocalStmt(Node::Kind::BreakContStmt), kind(K) {}
    
    bool isBreak() const { return kind == Kind::Break; }
    bool isContinue() const { return !isBreak(); }
};



# pragma mark - Expressions


class RawLLVMValueExpr : public Expr {
public:
    llvm::Value *value;
    yo::irgen::Type *type;
    
    CLASSOF_IMP(Node::Kind::RawLLVMValueExpr)
    RawLLVMValueExpr(llvm::Value *value, yo::irgen::Type *ty) : Expr(Node::Kind::RawLLVMValueExpr), value(value), type(ty) {}
};


class Ident : public Expr {
public:
    const std::string value;
    
    CLASSOF_IMP(Node::Kind::Ident)
    explicit Ident(std::string value) : Expr(Node::Kind::Ident), value(value) {}
};



class NumberLiteral : public Expr {
public:
    enum class NumberType {
        Integer, Double, Boolean, Character
    };
    
    const uint64_t value;
    const NumberType type;
    
    CLASSOF_IMP(Node::Kind::NumberLiteral)
    explicit NumberLiteral(uint64_t value, NumberType type) : Expr(Node::Kind::NumberLiteral), value(value), type(type) {}
    
    static std::shared_ptr<NumberLiteral> integer(uint64_t value) {
        return std::make_shared<NumberLiteral>(value, NumberType::Integer);
    }
};


class StringLiteral : public Expr {
public:
    enum class StringLiteralKind {
        NormalString,   // Becomes a `String` object
        ByteString      // Becomes an `*i8` pointer
    };
    
    std::string value;
    StringLiteralKind kind;
    
    CLASSOF_IMP(Node::Kind::StringLiteral)
    explicit StringLiteral(std::string value, StringLiteralKind kind) : Expr(Node::Kind::StringLiteral), value(value), kind(kind) {}
};




class ExprStmt : public LocalStmt {
public:
    std::shared_ptr<ast::Expr> expr;
    
    CLASSOF_IMP(Node::Kind::ExprStmt)
    explicit ExprStmt(std::shared_ptr<ast::Expr> expr) : LocalStmt(Node::Kind::ExprStmt), expr(expr) {}
};



class TupleExpr : public Expr {
public:
    std::vector<std::shared_ptr<Expr>> elements;
    
    CLASSOF_IMP(Node::Kind::TupleExpr)
    TupleExpr(std::vector<std::shared_ptr<Expr>> E) : Expr(Node::Kind::TupleExpr), elements(E) {}
    
    size_t numberOfElements() const {
        return elements.size();
    }
};


// A reference to a static member of a type (for example a static method or an enum value)
class StaticDeclRefExpr : public Expr {
public:
    std::shared_ptr<TypeDesc> typeDesc;
    std::string memberName;
    
    CLASSOF_IMP(Node::Kind::StaticDeclRefExpr)
    StaticDeclRefExpr(std::shared_ptr<TypeDesc> typeDesc, const std::string &memberName)
    : Expr(Node::Kind::StaticDeclRefExpr), typeDesc(typeDesc), memberName(memberName) {}
};




// <expr>(<expr>*)
class CallExpr : public Expr {
public:
    std::shared_ptr<Expr> target;
    std::vector<std::shared_ptr<Expr>> arguments;
    std::shared_ptr<ast::TemplateParamArgList> explicitTemplateArgs;
    
    CLASSOF_IMP(Node::Kind::CallExpr)
    CallExpr(std::shared_ptr<Expr> target, std::vector<std::shared_ptr<Expr>> arguments = {})
    : Expr(Node::Kind::CallExpr), target(target), arguments(arguments) {}
    
    /// Whether the call has specifies explicit template arguments
    bool hasExplicitTemplateArgs() const {
        return explicitTemplateArgs != nullptr;
    }
    
    uint64_t numberOfExplicitTemplateArgs() const {
        if (!hasExplicitTemplateArgs()) return 0;
        else return explicitTemplateArgs->size();
    }
};



// <expr>.<ident>
// A member expression's source location should point to the first token after the target expression
class MemberExpr : public Expr {
public:
    std::shared_ptr<Expr> target;
    std::string memberName;
    
    CLASSOF_IMP(Node::Kind::MemberExpr)
    MemberExpr(std::shared_ptr<Expr> target, std::string memberName) : Expr(Node::Kind::MemberExpr), target(target), memberName(memberName) {}
};



// SourceLoc should point to the `[` character
class SubscriptExpr : public Expr {
public:
    std::shared_ptr<ast::Expr> target;
    std::vector<std::shared_ptr<ast::Expr>> args;
    
    CLASSOF_IMP(Node::Kind::SubscriptExpr)
    SubscriptExpr(std::shared_ptr<ast::Expr> target, std::vector<std::shared_ptr<ast::Expr>> args)
    : Expr(Node::Kind::SubscriptExpr), target(target), args(args) {}
};







class CastExpr : public Expr {
public:
    enum class CastKind {
        StaticCast, Bitcast
    };
    std::shared_ptr<Expr> expr;
    std::shared_ptr<TypeDesc> destType;
    CastKind kind;
    
    CLASSOF_IMP(Node::Kind::CastExpr)
    CastExpr(std::shared_ptr<Expr> expr, std::shared_ptr<TypeDesc> destType, CastKind kind)
    : Expr(Node::Kind::CastExpr), expr(expr), destType(destType), kind(kind) {
        setSourceLocation(expr->getSourceLocation());
    }
};



class MatchExprPattern : public Node {
public:
    std::shared_ptr<ast::Expr> expr;
    std::shared_ptr<ast::Expr> cond;
    
    MatchExprPattern() : Node(Kind::MatchExprPattern) {}
    
    CLASSOF_IMP(Node::Kind::MatchExprPattern)
    MatchExprPattern(std::shared_ptr<ast::Expr> PE, std::shared_ptr<ast::Expr> CE)
    : Node(Kind::MatchExprPattern), expr(PE), cond(CE) {}
    
    bool hasCondition() const {
        return cond != nullptr;
    }
};




class MatchExprBranch : public Node {
public:
    std::vector<MatchExprPattern> patterns;
    std::shared_ptr<Expr> expr;
    
    MatchExprBranch() : Node(Node::Kind::MatchExprBranch) {}
    
    CLASSOF_IMP(Node::Kind::MatchExprBranch)
    MatchExprBranch(std::vector<MatchExprPattern> patterns, std::shared_ptr<Expr> expr)
    : Node(Node::Kind::MatchExprBranch), patterns(patterns), expr(expr) {}
};




class MatchExpr : public Expr {
public:
    
    std::shared_ptr<Expr> target;
    std::vector<MatchExprBranch> branches;
    
    CLASSOF_IMP(Node::Kind::MatchExpr)
    MatchExpr(std::shared_ptr<Expr> target, std::vector<MatchExprBranch> branches) : Expr(Node::Kind::MatchExpr), target(target), branches(branches) {}
};



// A BinOp's source location refers to the location of the operator
class BinOp : public Expr {
    Operator op;
    std::shared_ptr<Expr> lhs;
    std::shared_ptr<Expr> rhs;
    bool _isInPlaceBinop = false;
    
public:
    CLASSOF_IMP(Node::Kind::BinOp)
    BinOp(Operator op, std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs)
    : Expr(Node::Kind::BinOp), op(op), lhs(lhs), rhs(rhs) {}
    
    Operator getOperator() const { return op; }
    std::shared_ptr<Expr> getLhs() const { return lhs; }
    std::shared_ptr<Expr> getRhs() const { return rhs; }
    bool isInPlaceBinop() const { return _isInPlaceBinop; }
    void setIsInPlaceBinop(bool val) { _isInPlaceBinop = val; }
};



class UnaryExpr : public Expr {
public:
    enum class Operation : uint8_t {
        Negate,
        BitwiseNot,
        LogicalNegation,
        AddressOf,
//        Dereference
    };
    
    Operation op;
    std::shared_ptr<ast::Expr> expr;
    
    CLASSOF_IMP(Node::Kind::UnaryExpr)
    UnaryExpr(Operation op, std::shared_ptr<ast::Expr> expr) : Expr(Node::Kind::UnaryExpr), op(op), expr(expr) {}
};



/// A lambda is an anonymous function
class LambdaExpr : public Expr {
public:
    /// An element in a lambda expression's capture list
    /// This can be either of the following:
    /// - `x` capture x by value
    /// - `&x` capture x by reference
    /// - `x = <expr>` captures an expression by value
    /// - `&x = <expr>` captures an expression by reference
    struct CaptureListElement {
        bool isReference = false; // TODO rename to capturesByReference or hasReferenceSemantics
        std::shared_ptr<Ident> ident;
        std::shared_ptr<Expr> expr;
    };
    
    std::vector<CaptureListElement> captureList;
    FunctionSignature signature;
    std::vector<std::shared_ptr<Ident>> paramNames;
    std::shared_ptr<CompoundStmt> body;
    
    // the struct generated for this lambda expression
    irgen::StructType *_structType = nullptr;
    
    CLASSOF_IMP(Node::Kind::LambdaExpr)
    LambdaExpr() : Expr(Node::Kind::LambdaExpr) {}
};



class ArrayLiteralExpr : public Expr {
public:
    std::vector<std::shared_ptr<Expr>> elements;
    
    CLASSOF_IMP(Node::Kind::ArrayLiteralExpr)
    explicit ArrayLiteralExpr(std::vector<std::shared_ptr<Expr>> elements)
    : Expr(Node::Kind::ArrayLiteralExpr), elements(elements) {}
};


#undef CLASSOF_IMP
NS_END
