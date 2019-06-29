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
#include "TypeInfo.h"
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


std::string description(AST &ast);


class Expr;
class Identifier;
class VariableDecl;
class Composite;
class StructDecl;


class Node {
public:
    virtual std::string description();
    const parser::TokenSourceLocation &getSourceLocation() const {
        return sourceLocation;
    }
    
    void setSourceLocation(const parser::TokenSourceLocation &sourceLoc) {
        this->sourceLocation = sourceLoc;
    }
    
protected:
    virtual ~Node() = default;
    
    parser::TokenSourceLocation sourceLocation;
};


class TopLevelStmt : public Node {};

class LocalStmt : public Node {};

class Expr : public Node {};


// Note: some of the nested subclasses also inherit from ast::Node. one could argue that this is philosophically wrong, but it greatly simplifies ast printing



#pragma mark - Top Level Statements



class FunctionSignature : public Node {
public:
    enum class FunctionKind {
        GlobalFunction,   // A free global function
        StaticMethod,     // A static type member method
        InstanceMethod    // A type instance method
    };
    
    
    std::string name;
    FunctionKind kind;
    TypeInfo *returnType;
    std::vector<std::shared_ptr<VariableDecl>> parameters;
    std::shared_ptr<ast::StructDecl> implType; // If this is a static or instance method, the type it is a member of
    
    std::shared_ptr<attributes::FunctionAttributes> attributes;
    
    TypeInfo *variadicType;
    
    bool isTemplateFunction = false;
    std::vector<std::string> templateArgumentNames;
    
    explicit FunctionSignature() {
        attributes = std::make_shared<attributes::FunctionAttributes>();
    }
    
    bool isFullSpecialization() {
        return isTemplateFunction && templateArgumentNames.empty();
    }
    
};

std::ostream& operator<<(std::ostream&, const std::shared_ptr<ast::FunctionSignature>&);



class FunctionDecl : public TopLevelStmt {
public:
    std::shared_ptr<FunctionSignature> signature;
    std::shared_ptr<Composite> body;
    
    FunctionDecl() {}
};


class StructDecl : public TopLevelStmt {
public:
    std::shared_ptr<Identifier> name;
    std::vector<std::shared_ptr<VariableDecl>> members;
    std::vector<std::string> templateArguments;
    std::shared_ptr<attributes::StructAttributes> attributes;
    
    StructDecl() {}
    
    bool isTemplateStruct() { return !templateArguments.empty(); }
};

class ImplBlock : public TopLevelStmt {
public:
    std::string typename_;
    std::vector<std::shared_ptr<FunctionDecl>> methods;
    
    ImplBlock(std::string typename_) : typename_(typename_) {}
    ImplBlock(std::string typename_, std::vector<std::shared_ptr<FunctionDecl>> methods) : typename_(typename_), methods(methods) {}
};


class TypealiasDecl : public TopLevelStmt {
public:
    std::string typename_;
    TypeInfo *type;
    
    TypealiasDecl(std::string typename_, TypeInfo *type) : typename_(typename_), type(type) {}
};


# pragma mark - Local Statements


class Composite : public LocalStmt {
public:
    std::vector<std::shared_ptr<LocalStmt>> statements;
    
    Composite() {}
    Composite(std::vector<std::shared_ptr<LocalStmt>> statements) : statements(statements) {}
    
    bool isEmpty() const {
        return statements.empty();
    }
};


class ReturnStmt : public LocalStmt {
public:
    std::shared_ptr<Expr> expression;
    
    explicit ReturnStmt(std::shared_ptr<Expr> expression) : expression(expression) {}
};



class VariableDecl : public LocalStmt {
public:
    std::shared_ptr<Identifier> name; // TODO does something like this really warrant a pointer?
    TypeInfo *type;
    std::shared_ptr<Expr> initialValue;
    
    VariableDecl(std::shared_ptr<Identifier> name, TypeInfo *type, std::shared_ptr<Expr> initialValue = nullptr)
    : name(name), type(type), initialValue(initialValue) {}
};


class Assignment : public LocalStmt {
public:
    std::shared_ptr<Expr> target;
    std::shared_ptr<Expr> value;
    
    Assignment(std::shared_ptr<Expr> target, std::shared_ptr<Expr> value) : target(target), value(value) {}
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
        : kind(kind), condition(condition), body(body) {}
    };
    
    std::vector<std::shared_ptr<Branch>> branches;
    
    IfStmt(std::vector<std::shared_ptr<Branch>> branches) : branches(branches) {}
};


class WhileStmt : public LocalStmt {
public:
    std::shared_ptr<ast::Expr> condition;
    std::shared_ptr<ast::Composite> body;
    
    WhileStmt(std::shared_ptr<Expr> condition, std::shared_ptr<Composite> body) : condition(condition), body(body) {}
};


class ForLoop : public LocalStmt {
public:
    std::shared_ptr<ast::Identifier> ident;
    std::shared_ptr<ast::Expr> expr;
    std::shared_ptr<ast::Composite> body;
    
    ForLoop(std::shared_ptr<ast::Identifier> ident, std::shared_ptr<ast::Expr> expr, std::shared_ptr<ast::Composite> body)
    : ident(ident), expr(expr), body(body) {}
};





# pragma mark - Expressions


class RawLLVMValueExpr : public Expr {
public:
    llvm::Value *value;
    TypeInfo *type;
    
    RawLLVMValueExpr(llvm::Value *value, TypeInfo *ty) : value(value), type(ty) {}
};


class Identifier : public Expr {
public:
    const std::string value;
    
    explicit Identifier(std::string value) : value(value) {}
    
    static std::shared_ptr<Identifier> emptyIdent();
};



class NumberLiteral : public Expr {
public:
    enum class NumberType {
        Integer, Double, Boolean, Character
    };
    
    const uint64_t value;
    const NumberType type;
    
    explicit NumberLiteral(uint64_t value, NumberType type) : value(value), type(type) {}
    
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
    
    explicit StringLiteral(std::string value, StringLiteralKind kind) : value(value), kind(kind) {}
};




class ExprStmt : public LocalStmt {
public:
    std::shared_ptr<ast::Expr> expr;
    
    explicit ExprStmt(std::shared_ptr<ast::Expr> expr) : expr(expr) {}
};



// A reference to a static member of a type (for example a static method or an enum value)
class StaticDeclRefExpr : public Expr {
public:
    std::string typeName;
    std::string memberName;
    
    StaticDeclRefExpr(const std::string &typeName, const std::string &memberName) : typeName(typeName), memberName(memberName) {}
};




// <expr>(<expr>*)
class CallExpr : public Expr {
public:
    std::shared_ptr<Expr> target;
    std::vector<std::shared_ptr<Expr>> arguments;
    std::vector<TypeInfo *> explicitTemplateArgumentTypes;
    
    CallExpr(std::shared_ptr<Expr> target,
             std::vector<std::shared_ptr<Expr>> arguments = {},
             std::vector<TypeInfo *> explicitTemplateArgumentTypes = {})
    : target(target), arguments(arguments), explicitTemplateArgumentTypes(explicitTemplateArgumentTypes) {}
};



// <expr>.<ident>
class MemberExpr : public Expr {
public:
    std::shared_ptr<Expr> target;
    std::string memberName;
    
    MemberExpr(std::shared_ptr<Expr> target, std::string memberName) : target(target), memberName(memberName) {}
};



class SubscriptExpr : public Expr {
public:
    std::shared_ptr<ast::Expr> target;
    std::shared_ptr<ast::Expr> offset;
    
    SubscriptExpr(std::shared_ptr<ast::Expr> target, std::shared_ptr<ast::Expr> offset) : target(target), offset(offset) {}
};







class Typecast : public Expr {
public:
    enum class CastKind {
        StaticCast, Bitcast
    };
    std::shared_ptr<Expr> expression;
    TypeInfo *destType;
    CastKind kind;
    
    Typecast(std::shared_ptr<Expr> expression, TypeInfo *destType, CastKind kind)
    : expression(expression), destType(destType), kind(kind) {}
};




class MatchExpr : public Expr {
public:
    class MatchExprBranch : public Node {
    public:
        std::vector<std::shared_ptr<Expr>> patterns;
        std::shared_ptr<Expr> expression;
        
        MatchExprBranch(std::vector<std::shared_ptr<Expr>> patterns, std::shared_ptr<Expr> expression) : patterns(patterns), expression(expression) {}
    };
    
    std::shared_ptr<Expr> target;
    std::vector<std::shared_ptr<MatchExprBranch>> branches;
    
    MatchExpr(std::shared_ptr<Expr> target, std::vector<std::shared_ptr<MatchExprBranch>> branches) : target(target), branches(branches) {}
};


class BinaryOperation : public Expr {
public:
    enum class Operation {
        Add, Sub, Mul, Div, Mod,
        And, Or, Xor, Shl, Shr
    };
    
    Operation op;
    std::shared_ptr<Expr> lhs;
    std::shared_ptr<Expr> rhs;
    
    BinaryOperation(Operation op, std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs) : op(op), lhs(lhs), rhs(rhs) {}
};



class Comparison : public Expr {
public:
    enum class Operation {
        EQ, NE, // == / !=
        LT, LE, // > \ >=
        GT, GE  // < \ <=
    };
    
    Operation op;
    std::shared_ptr<Expr> lhs;
    std::shared_ptr<Expr> rhs;
    
    Comparison(Operation op, std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs) : op(op), lhs(lhs), rhs(rhs) {}
};


class LogicalOperation : public Expr {
public:
    enum class Operation {
        And, Or
    };
    
    Operation op;
    std::shared_ptr<Expr> lhs;
    std::shared_ptr<Expr> rhs;
    
    LogicalOperation(Operation op, std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs) : op(op), lhs(lhs), rhs(rhs) {}
};


class UnaryExpr : public Expr {
public:
    enum class Operation {
        Negate, BitwiseNot, LogicalNegation
    };
    
    Operation op;
    std::shared_ptr<ast::Expr> expr;
    
    UnaryExpr(Operation op, std::shared_ptr<ast::Expr> expr) : op(op), expr(expr) {}
};

NS_END
