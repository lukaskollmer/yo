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

namespace llvm {
    class Value;
}

NS_START(yo::ast)


class TopLevelStmt;

using AST = std::vector<std::shared_ptr<TopLevelStmt>>;


std::string description(const AST& ast);




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


class Node {
public:
    enum class NodeKind {
        // Top Level Statements
        FunctionDecl, ImplBlock, StructDecl, TypealiasDecl,
        
        // Local Statements
        Assignment, Composite, ExprStmt, ForLoop, IfStmt, ReturnStmt, VarDecl, WhileStmt,
        
        // Expressions
        BinOp, CallExpr, CompOp, Ident, LogicalOp, MatchExpr, MemberExpr, NumberLiteral, RawLLVMValueExpr, StaticDeclRefExpr,
        StringLiteral, SubscriptExpr, CastExpr, UnaryExpr,
        
        // Things that shouldn't inherit from Node, but do
        FunctionSignature, IfStmtBranch, MatchExprBranch
    };
    
    NodeKind getNodeKind() const { return kind; }
    
    virtual std::string description() const;
    const parser::TokenSourceLocation &getSourceLocation() const {
        return sourceLocation;
    }
    
    void setSourceLocation(const parser::TokenSourceLocation &sourceLoc) {
        this->sourceLocation = sourceLoc;
    }
    
protected:
    Node(NodeKind kind) : kind(kind) {}
    virtual ~Node() = default;
    
private:
    NodeKind kind;
    parser::TokenSourceLocation sourceLocation;
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


class FunctionSignature : public Node {
public:
    std::shared_ptr<TypeDesc> returnType;
    std::vector<std::shared_ptr<VarDecl>> parameters;
    std::vector<std::string> templateArgumentNames;

    FunctionSignature() : Node(Node::NodeKind::FunctionSignature) {}


    bool isTemplateFunction() const { return !templateArgumentNames.empty(); }
};

std::ostream& operator<<(std::ostream&, const ast::FunctionSignature&);








class FunctionDecl : public TopLevelStmt {
    FunctionSignature signature;
    std::vector<std::shared_ptr<LocalStmt>> funcBody;
    attributes::FunctionAttributes attributes;
    
    FunctionKind funcKind;
    std::string name;
    irgen::StructType *implType = nullptr; // Only nonnull if this is a type member function

    
public:
    explicit FunctionDecl(FunctionKind kind, std::string name, FunctionSignature sig, attributes::FunctionAttributes attr)
    : TopLevelStmt(Node::NodeKind::FunctionDecl), signature(sig), attributes(attr), funcKind(kind), name(name) {}
    
    FunctionKind getFunctionKind() const { return funcKind; }
    void setFunctionKind(FunctionKind kind) { funcKind = kind; }
    
    const std::string& getName() const { return name; }
    FunctionSignature& getSignature() { return signature; }
    const FunctionSignature& getSignature() const { return signature; }
    irgen::StructType* getImplType() const { return implType; }
    void setImplType(irgen::StructType *ty) { implType = ty; }
    
    attributes::FunctionAttributes& getAttributes() { return attributes; }
    const attributes::FunctionAttributes& getAttributes() const { return attributes; }
    
    const std::vector<std::shared_ptr<LocalStmt>>& getBody() const { return funcBody; }
    void setBody(std::vector<std::shared_ptr<LocalStmt>> body) { funcBody = body; }
    
    
    bool isOfKind(FunctionKind kind) const { return funcKind == kind; }
};






class StructDecl : public TopLevelStmt {
public:
    std::string name;
    std::vector<std::shared_ptr<VarDecl>> members;
    std::vector<std::string> templateArguments;
    attributes::StructAttributes attributes;
    
    StructDecl() : TopLevelStmt(Node::NodeKind::StructDecl) {}
    
    bool isTemplateStruct() { return !templateArguments.empty(); }
};

class ImplBlock : public TopLevelStmt {
public:
    std::string typename_;
    std::vector<std::shared_ptr<FunctionDecl>> methods;
    
    ImplBlock(std::string typename_) : TopLevelStmt(Node::NodeKind::ImplBlock), typename_(typename_) {}
    ImplBlock(std::string typename_, std::vector<std::shared_ptr<FunctionDecl>> methods) : TopLevelStmt(Node::NodeKind::ImplBlock), typename_(typename_), methods(methods) {}
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
    std::shared_ptr<Expr> expression;
    
    explicit ReturnStmt(std::shared_ptr<Expr> expression) : LocalStmt(Node::NodeKind::ReturnStmt), expression(expression) {}
};



// TODO rename to VarDecl?
class VarDecl : public LocalStmt {
public:
    std::string name;
    std::shared_ptr<TypeDesc> type;
    std::shared_ptr<Expr> initialValue;
    
    VarDecl(std::string name, std::shared_ptr<TypeDesc> type, std::shared_ptr<Expr> initialValue = nullptr)
    : LocalStmt(Node::NodeKind::VarDecl), name(name), type(type), initialValue(initialValue) {}
};


class Assignment : public LocalStmt {
public:
    std::shared_ptr<Expr> target;
    std::shared_ptr<Expr> value;
    
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
    std::shared_ptr<ast::Ident> ident;
    std::shared_ptr<ast::Expr> expr;
    std::shared_ptr<ast::Composite> body;
    
    ForLoop(std::shared_ptr<ast::Ident> ident, std::shared_ptr<ast::Expr> expr, std::shared_ptr<ast::Composite> body)
    : LocalStmt(Node::NodeKind::ForLoop), ident(ident), expr(expr), body(body) {}
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
    std::vector<std::shared_ptr<TypeDesc>> explicitTemplateArgumentTypes;
    
    CallExpr(std::shared_ptr<Expr> target,
             std::vector<std::shared_ptr<Expr>> arguments = {},
             std::vector<std::shared_ptr<TypeDesc>> explicitTemplateArgumentTypes = {})
    : Expr(Node::NodeKind::CallExpr), target(target), arguments(arguments), explicitTemplateArgumentTypes(explicitTemplateArgumentTypes) {}
};



// <expr>.<ident>
class MemberExpr : public Expr {
public:
    std::shared_ptr<Expr> target;
    std::string memberName;
    
    MemberExpr(std::shared_ptr<Expr> target, std::string memberName) : Expr(Node::NodeKind::MemberExpr), target(target), memberName(memberName) {}
};



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
    std::shared_ptr<Expr> expression;
    std::shared_ptr<TypeDesc> destType;
    CastKind kind;
    
    CastExpr(std::shared_ptr<Expr> expression, std::shared_ptr<TypeDesc> destType, CastKind kind)
    : Expr(Node::NodeKind::CastExpr), expression(expression), destType(destType), kind(kind) {}
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
    
public:
    BinOp(Operator op, std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs)
    : Expr(Node::NodeKind::BinOp), op(op), lhs(lhs), rhs(rhs) {}
    
    Operator getOperator() const { return op; }
    std::shared_ptr<Expr> getLhs() const { return lhs; }
    std::shared_ptr<Expr> getRhs() const { return rhs; }
};





class UnaryExpr : public Expr {
public:
    enum class Operation : uint8_t {
        Negate,// = Operator::Neg,
        BitwiseNot,
        LogicalNegation
    };
    
    Operation op;
    std::shared_ptr<ast::Expr> expr;
    
    UnaryExpr(Operation op, std::shared_ptr<ast::Expr> expr) : Expr(Node::NodeKind::UnaryExpr), op(op), expr(expr) {}
};

NS_END
