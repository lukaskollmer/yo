//
//  AST.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-03.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"
#include "Token.h"
#include "TypeDesc.h"
#include "Attributes.h"

#include <memory>
#include <iostream>
#include <variant>
#include <string>
#include <vector>
#include <map>


namespace llvm { class Value; }
namespace yo::irgen { class Type; }


NS_START(yo::ast)


class TopLevelStmt;
using AST = std::vector<std::shared_ptr<TopLevelStmt>>;
using parser::TokenSourceLocation;

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
    
    /* |> */ FnPipe,
    
    
    /* =  */ Assign,
};



class Expr;
class Ident;
class VarDecl;
class Composite;
class StructDecl;
class ImplBlock;


class Node {
public:
    enum class NodeKind {
        // Top Level Statements
        FunctionDecl, ImplBlock, StructDecl, TypealiasDecl,
        
        // Local Statements
        Assignment, Composite, ExprStmt, ForLoop, IfStmt, ReturnStmt, VarDecl, WhileStmt, BreakContStmt,
        
        // Expressions
        BinOp, CallExpr, CompOp, Ident, LogicalOp, MatchExpr, MemberExpr, NumberLiteral, RawLLVMValueExpr, StaticDeclRefExpr,
        StringLiteral, SubscriptExpr, CastExpr, UnaryExpr,
        
        TemplateParamDeclList, TemplateParamArgList, FunctionSignature, IfStmtBranch, MatchExprBranch
    };
    
    NodeKind getNodeKind() const { return kind; }
    bool isOfKind(NodeKind NK) const { return kind == NK; }
    
    // TODO add a `str` function that prints the node, as it would look as source code?
    virtual std::string description() const; // TODO remove in favor of `ast::description`
    const TokenSourceLocation& getSourceLocation() const {
        return sourceLocation;
    }
    
    void setSourceLocation(const TokenSourceLocation &sourceLoc) {
        this->sourceLocation = sourceLoc;
    }
    
protected:
    Node(NodeKind kind) : kind(kind) {}
    virtual ~Node() = default;
    
private:
    NodeKind kind;
    TokenSourceLocation sourceLocation;
};


class TopLevelStmt : public Node {
protected:
    TopLevelStmt(NodeKind kind) : Node(kind) {}
};

class LocalStmt : public Node {
protected:
    LocalStmt(NodeKind kind) : Node(kind) {}
};

class Expr : public Node {
protected:
    Expr(NodeKind kind) : Node(kind) {}
};


// Note: some of the nested subclasses also inherit from ast::Node. one could argue that this is philosophically wrong, but it greatly simplifies ast printing



#pragma mark - Top Level Statements


enum class FunctionKind {
    GlobalFunction,   // A free global function
    StaticMethod,     // A static type member method
    InstanceMethod,   // A type instance method
    OperatorOverload
};



/// A list of template parameters, for example for a struct or function declaration
class TemplateParamDeclList : public Node {
public:
    struct Param {
        std::string name;
        std::shared_ptr<ast::TypeDesc> defaultType;
        Param(std::string name, std::shared_ptr<ast::TypeDesc> defaultType = nullptr) : name(name), defaultType(defaultType) {}
    };
    
private:
    std::vector<Param> elements;
    
public:
    TemplateParamDeclList() : Node(Node::NodeKind::TemplateParamDeclList) {}
    
    void addParam(Param P) { elements.push_back(P); }
    void setParams(std::vector<Param> E) { elements = E; }
    const std::vector<Param>& getParams() const { return elements; }
    bool isEmpty() const { return elements.empty(); }
    size_t size() const { return elements.size(); }
};



class TemplateParamArgList : public Node {
public:
    std::vector<std::shared_ptr<ast::TypeDesc>> elements;
    
    TemplateParamArgList() : Node(Node::NodeKind::TemplateParamArgList) {}
    
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



class FunctionSignature : public Node {
public:
    std::shared_ptr<TypeDesc> returnType;
    std::vector<std::shared_ptr<TypeDesc>> paramTypes;
    std::shared_ptr<TemplateParamDeclList> templateParamsDecl;
    bool isVariadic = false;

    FunctionSignature() : Node(Node::NodeKind::FunctionSignature) {}
    
    bool isTemplateDecl() const { return templateParamsDecl != nullptr; }
    uint64_t numberOfTemplateParameters() const {
        if (!templateParamsDecl) return 0;
        else return templateParamsDecl->size();
    }
};

std::ostream& operator<<(std::ostream&, const ast::FunctionSignature&);






/// A Function Declaration
class FunctionDecl : public TopLevelStmt {
    FunctionSignature signature;
    std::vector<std::shared_ptr<ast::Ident>> paramNames;
    std::shared_ptr<ast::Composite> body;
    attributes::FunctionAttributes attributes;
    
    FunctionKind funcKind;
    std::string name;
    
    // context
    irgen::StructType *implType = nullptr; // Only nonnull if this is a type member function
    /// The template arguments this function was instantiated with
    std::vector<yo::irgen::Type *> resolvedTemplateArgTypes;

    
public:
    FunctionDecl(FunctionKind kind, std::string name, FunctionSignature sig, attributes::FunctionAttributes attr)
    : TopLevelStmt(Node::NodeKind::FunctionDecl), signature(sig), body(std::make_shared<Composite>()), attributes(attr), funcKind(kind), name(name) {}
    
    FunctionKind getFunctionKind() const { return funcKind; }
    void setFunctionKind(FunctionKind kind) { funcKind = kind; }
    
    const std::string& getName() const { return name; }
    
    FunctionSignature& getSignature() { return signature; }
    const FunctionSignature& getSignature() const { return signature; }
    
    irgen::StructType* getImplType() const { return implType; }
    void setImplType(irgen::StructType *ty) { implType = ty; }
    
    const auto& getResolvedTemplateArgTypes() const { return resolvedTemplateArgTypes; }
    void setResolvedTemplateArgTypes(std::vector<yo::irgen::Type *> tys) { resolvedTemplateArgTypes = tys; }
    
    const std::vector<std::shared_ptr<Ident>>& getParamNames() const { return paramNames; }
    void setParamNames(std::vector<std::shared_ptr<Ident>> names) { paramNames = names; }
    
    attributes::FunctionAttributes& getAttributes() { return attributes; }
    const attributes::FunctionAttributes& getAttributes() const { return attributes; }
    
    const std::shared_ptr<ast::Composite>& getBody() const { return body; }
    void setBody(std::shared_ptr<ast::Composite> B) { body = B; }
    
    bool isOfFunctionKind(FunctionKind kind) const {
        return funcKind == kind;
    }
    
    /// Whether the function is a compiler-generated instantiation of a function template
    bool isTemplateInstantiation() const {
        return !resolvedTemplateArgTypes.empty();
    }
};





class StructDecl : public TopLevelStmt {
public:
    std::string name;
    std::vector<std::shared_ptr<VarDecl>> members;
    std::shared_ptr<TemplateParamDeclList> templateParamsDecl;
    attributes::StructAttributes attributes;
    
    /// The template arguments this struct was instantiated with
    std::vector<yo::irgen::Type *> resolvedTemplateArgTypes;
    
    /// The `impl` blocks belonging to this struct
    std::vector<std::shared_ptr<ImplBlock>> implBlocks;
    
    StructDecl() : TopLevelStmt(Node::NodeKind::StructDecl) {}
    
    bool isTemplateDecl() { return templateParamsDecl != nullptr; }
    
    const std::string& getName() const { return name; }
};


class ImplBlock : public TopLevelStmt {
public:
    std::string typename_; // TODO make this a TypeDesc instead?
    std::vector<std::shared_ptr<FunctionDecl>> methods;
    bool isNominalTemplateType = false;

    ImplBlock(std::string typename_) : TopLevelStmt(Node::NodeKind::ImplBlock), typename_(typename_) {}
    
    const std::string& getName() const {
        return typename_;
    }
};


class TypealiasDecl : public TopLevelStmt {
public:
    std::string typename_;
    std::shared_ptr<TypeDesc> type;
    
    TypealiasDecl(std::string typename_, std::shared_ptr<TypeDesc> type) : TopLevelStmt(Node::NodeKind::TypealiasDecl), typename_(typename_), type(type) {}
};


# pragma mark - Local Statements


class Composite : public LocalStmt {
public:
    std::vector<std::shared_ptr<LocalStmt>> statements;
    
    Composite() : LocalStmt(Node::NodeKind::Composite) {}
    Composite(std::vector<std::shared_ptr<LocalStmt>> statements) : LocalStmt(Node::NodeKind::Composite), statements(statements) {}
    
    bool isEmpty() const {
        return statements.empty();
    }
};


class ReturnStmt : public LocalStmt {
public:
    std::shared_ptr<Expr> expr;
    
    explicit ReturnStmt(std::shared_ptr<Expr> expr) : LocalStmt(Node::NodeKind::ReturnStmt), expr(expr) {}
};



// TODO rename to VarDecl?
class VarDecl : public LocalStmt {
public:
    std::string name;
    std::shared_ptr<TypeDesc> type;
    std::shared_ptr<Expr> initialValue;
    
    // eg `let &x = y;`
    // Requires `type` to be nil
    bool declaresUntypedReference = false;
    
    VarDecl(std::string name, std::shared_ptr<TypeDesc> type, std::shared_ptr<Expr> initialValue = nullptr)
    : LocalStmt(Node::NodeKind::VarDecl), name(name), type(type), initialValue(initialValue) {}
};


class Assignment : public LocalStmt {
public:
    std::shared_ptr<Expr> target;
    std::shared_ptr<Expr> value;
    
    bool shouldDestructOldValue = true;
    bool overwriteReferences = false;
    
    Assignment(std::shared_ptr<Expr> target, std::shared_ptr<Expr> value) : LocalStmt(Node::NodeKind::Assignment), target(target), value(value) {}
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
        std::shared_ptr<Composite> body;
        
        Branch(BranchKind kind, std::shared_ptr<Expr> condition, std::shared_ptr<Composite> body)
        : Node(Node::NodeKind::IfStmtBranch), kind(kind), condition(condition), body(body) {}
    };
    
    std::vector<std::shared_ptr<Branch>> branches;
    
    IfStmt(std::vector<std::shared_ptr<Branch>> branches) : LocalStmt(Node::NodeKind::IfStmt), branches(branches) {}
};


class WhileStmt : public LocalStmt {
public:
    std::shared_ptr<ast::Expr> condition;
    std::shared_ptr<ast::Composite> body;
    
    WhileStmt(std::shared_ptr<Expr> condition, std::shared_ptr<Composite> body) : LocalStmt(Node::NodeKind::WhileStmt), condition(condition), body(body) {}
};


class ForLoop : public LocalStmt {
public:
    bool capturesByReference = false;
    std::shared_ptr<ast::Ident> ident;
    std::shared_ptr<ast::Expr> expr;
    std::shared_ptr<ast::Composite> body;
    
    ForLoop(std::shared_ptr<ast::Ident> ident, std::shared_ptr<ast::Expr> expr, std::shared_ptr<ast::Composite> body)
    : LocalStmt(Node::NodeKind::ForLoop), ident(ident), expr(expr), body(body) {}
};



// TODO put break and continue in a single class?
// A `break` or `continue` statement
class BreakContStmt : public LocalStmt {
public:
    enum class Kind { Break, Continue };
    
    const Kind kind;
    BreakContStmt(Kind K) : LocalStmt(Node::NodeKind::BreakContStmt), kind(K) {}
    
    bool isBreak() const { return kind == Kind::Break; }
    bool isContinue() const { return !isBreak(); }
};



# pragma mark - Expressions


class RawLLVMValueExpr : public Expr {
public:
    llvm::Value *value;
    yo::irgen::Type *type;
    
    RawLLVMValueExpr(llvm::Value *value, yo::irgen::Type *ty) : Expr(Node::NodeKind::RawLLVMValueExpr), value(value), type(ty) {}
};


class Ident : public Expr {
public:
    const std::string value;
    
    explicit Ident(std::string value) : Expr(Node::NodeKind::Ident), value(value) {}
};



class NumberLiteral : public Expr {
public:
    enum class NumberType {
        Integer, Double, Boolean, Character
    };
    
    const uint64_t value;
    const NumberType type;
    
    explicit NumberLiteral(uint64_t value, NumberType type) : Expr(Node::NodeKind::NumberLiteral), value(value), type(type) {}
    
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
    
    explicit StringLiteral(std::string value, StringLiteralKind kind) : Expr(Node::NodeKind::StringLiteral), value(value), kind(kind) {}
};




class ExprStmt : public LocalStmt {
public:
    std::shared_ptr<ast::Expr> expr;
    
    explicit ExprStmt(std::shared_ptr<ast::Expr> expr) : LocalStmt(Node::NodeKind::ExprStmt), expr(expr) {}
};



// A reference to a static member of a type (for example a static method or an enum value)
class StaticDeclRefExpr : public Expr {
public:
    std::string typeName;
    std::string memberName;
    
    StaticDeclRefExpr(const std::string &typeName, const std::string &memberName) : Expr(Node::NodeKind::StaticDeclRefExpr), typeName(typeName), memberName(memberName) {}
};




// <expr>(<expr>*)
class CallExpr : public Expr {
public:
    std::shared_ptr<Expr> target;
    std::vector<std::shared_ptr<Expr>> arguments;
    std::shared_ptr<ast::TemplateParamArgList> explicitTemplateArgs;
    
    CallExpr(std::shared_ptr<Expr> target, std::vector<std::shared_ptr<Expr>> arguments = {})
    : Expr(Node::NodeKind::CallExpr), target(target), arguments(arguments) {}
    
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
    
    MemberExpr(std::shared_ptr<Expr> target, std::string memberName) : Expr(Node::NodeKind::MemberExpr), target(target), memberName(memberName) {}
};



// SourceLoc should point to the `[` character
class SubscriptExpr : public Expr {
public:
    std::shared_ptr<ast::Expr> target;
    std::shared_ptr<ast::Expr> offset;
    
    SubscriptExpr(std::shared_ptr<ast::Expr> target, std::shared_ptr<ast::Expr> offset) : Expr(Node::NodeKind::SubscriptExpr), target(target), offset(offset) {}
};







class CastExpr : public Expr {
public:
    enum class CastKind {
        StaticCast, Bitcast
    };
    std::shared_ptr<Expr> expr;
    std::shared_ptr<TypeDesc> destType;
    CastKind kind;
    
    CastExpr(std::shared_ptr<Expr> expr, std::shared_ptr<TypeDesc> destType, CastKind kind)
    : Expr(Node::NodeKind::CastExpr), expr(expr), destType(destType), kind(kind) {
        setSourceLocation(expr->getSourceLocation());
    }
};



class MatchExpr : public Expr {
public:
    class MatchExprBranch : public Node {
    public:
        std::vector<std::shared_ptr<Expr>> patterns;
        std::shared_ptr<Expr> expression;
        
        MatchExprBranch() : Node(Node::NodeKind::MatchExprBranch) {}
        
        MatchExprBranch(std::vector<std::shared_ptr<Expr>> patterns, std::shared_ptr<Expr> expression)
        : Node(Node::NodeKind::MatchExprBranch), patterns(patterns), expression(expression) {}
    };
    
    std::shared_ptr<Expr> target;
    std::vector<MatchExprBranch> branches;
    
    MatchExpr(std::shared_ptr<Expr> target, std::vector<MatchExprBranch> branches) : Expr(Node::NodeKind::MatchExpr), target(target), branches(branches) {}
};



// A BinOp's source location refers to the location of the operator
class BinOp : public Expr {
    Operator op;
    std::shared_ptr<Expr> lhs;
    std::shared_ptr<Expr> rhs;
    bool _isInPlaceBinop = false;
    
public:
    BinOp(Operator op, std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs)
    : Expr(Node::NodeKind::BinOp), op(op), lhs(lhs), rhs(rhs) {}
    
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
//        AddressOf
    };
    
    Operation op;
    std::shared_ptr<ast::Expr> expr;
    
    UnaryExpr(Operation op, std::shared_ptr<ast::Expr> expr) : Expr(Node::NodeKind::UnaryExpr), op(op), expr(expr) {}
};


NS_END
