//
//  AST.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-03.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <memory>
#include <iostream>

#include "util.h"
#include "TypeInfo.h"

NS_START(ast)


class TopLevelStmt;

using AST = std::vector<std::shared_ptr<TopLevelStmt>>;


std::string Description(AST &Ast);


class Expr;
class Identifier;
class VariableDecl;
class Composite;


class Node {
public:
    virtual std::string Description();
    
protected:
    Node() {}
    virtual ~Node() = default;
};



class TopLevelStmt : public Node {};

class LocalStmt : public Node {};

class Expr : public Node {};





#pragma mark - Top Level Statements



class FunctionSignature {
public:
    enum class FunctionKind {
        Global,     // A free global function
        Static,     // A static type member method
        Instance    // A type instance method
    };
    
    std::string Name; // TODO make this an identifier?
    FunctionKind Kind;
    std::string Typename; // Only relevant if Kind != Global
    std::vector<std::shared_ptr<VariableDecl>> Parameters;
    TypeInfo *ReturnType;
};



class FunctionDecl : public TopLevelStmt, public FunctionSignature {
public:
    std::shared_ptr<Composite> Body;
    
    FunctionDecl() : FunctionSignature() {}
};


class ExternFunctionDecl : public TopLevelStmt, public FunctionSignature {
public:
    ExternFunctionDecl() : FunctionSignature() {}
};


class Composite : public LocalStmt {
public:
    std::vector<std::shared_ptr<LocalStmt>> Statements;
    
    Composite() {}
};






# pragma mark - Local Statements

class ReturnStmt : public LocalStmt {
public:
    std::shared_ptr<Expr> Expression;
    
    explicit ReturnStmt(std::shared_ptr<Expr> Expression) : Expression(Expression) {}
};



class VariableDecl : public LocalStmt {
public:
    std::shared_ptr<Identifier> Name; // TODO does something like this really warrant a pointer?
    TypeInfo *Type;
    std::shared_ptr<Expr> InitialValue;
    
    VariableDecl(std::shared_ptr<Identifier> Name, TypeInfo *Type, std::shared_ptr<Expr> InitialValue = nullptr)
    : Name(Name), Type(Type), InitialValue(InitialValue) {}
};


class Assignment : public LocalStmt {
public:
    std::shared_ptr<Expr> Target;
    std::shared_ptr<Expr> Value;
    
    Assignment(std::shared_ptr<Expr> Target, std::shared_ptr<Expr> Value) : Target(Target), Value(Value) {}
};



class IfStmt : public LocalStmt {
public:
    class Branch : public Node { // Sole purpose of making this inherit from Node is simplifying ast dumping
    public:
        enum class BranchKind {
            If, ElseIf, Else
        };
        
        BranchKind Kind;
        std::shared_ptr<Expr> Condition; // nullptr if Kind == BranchKind::Else
        std::shared_ptr<Composite> Body;
        
        Branch(BranchKind Kind, std::shared_ptr<Expr> Condition, std::shared_ptr<Composite> Body)
        : Kind(Kind), Condition(Condition), Body(Body) {}
    };
    
    std::vector<std::shared_ptr<Branch>> Branches;
    
    IfStmt(std::vector<std::shared_ptr<Branch>> Branches) : Branches(Branches) {}
};








# pragma mark - Expressions


class Identifier : public Expr {
public:
    const std::string Value;
    Identifier(std::string Value) : Value(Value) {}
    
    operator std::string () { return Value; }
};



class NumberLiteral : public Expr {
public:
    uint64_t Value;
    
    explicit NumberLiteral(uint64_t Value) : Value(Value) {}
};



class FunctionCall : public Expr, public LocalStmt {
public:
    std::shared_ptr<Expr> Target;
    std::vector<std::shared_ptr<Expr>> Arguments;
    bool UnusedReturnValue;
    
    FunctionCall(std::shared_ptr<Expr> Target, std::vector<std::shared_ptr<Expr>> Arguments, bool UnusedReturnValue)
    : Target(Target), Arguments(Arguments), UnusedReturnValue(UnusedReturnValue) {}
};





class BinaryOperation : public Expr {
public:
    enum class Operation {
        Add, Sub, Mul, Div, Mod,
        And, Or, Xor, Shl, Shr
    };
    
    Operation Op;
    std::shared_ptr<Expr> LHS;
    std::shared_ptr<Expr> RHS;
    
    BinaryOperation(Operation Op, std::shared_ptr<Expr> LHS, std::shared_ptr<Expr> RHS) : Op(Op), LHS(LHS), RHS(RHS) {}
};



class Comparison : public Expr {
public:
    enum class Operation {
        EQ, NE, // == / !=
        LT, LE, // > \ >=
        GT, GE  // < \ <=
    };
    
    Operation Op;
    std::shared_ptr<Expr> LHS;
    std::shared_ptr<Expr> RHS;
    
    Comparison(Operation Op, std::shared_ptr<Expr> LHS, std::shared_ptr<Expr> RHS) : Op(Op), LHS(LHS), RHS(RHS) {}
};


class LogicalOperation : public Expr {
public:
    enum class Operation {
        And, Or
    };
    
    Operation Op;
    std::shared_ptr<Expr> LHS;
    std::shared_ptr<Expr> RHS;
    
    LogicalOperation(Operation Op, std::shared_ptr<Expr> LHS, std::shared_ptr<Expr> RHS) : Op(Op), LHS(LHS), RHS(RHS) {}
};

NS_END
