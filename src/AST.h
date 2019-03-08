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
    enum class NodeKind {
        // Categories
        Node, Expr, Stmt, TopLevelStmt, LocalStmt,
        
        // Top Level Statements
        FunctionDecl, ExternFunctionDecl,
        
        // Local Statements
        ReturnStmt, Composite, VariableDecl,
        
        // Literals
        NumberLiteral, StringLiteral, ArrayLiteral,
        
        // Expressions
        Identifier, FunctionCall, BinaryOperation,
        
    };
    
    const NodeKind _Kind;
    
    virtual std::string Description();
    
protected:
    Node(NodeKind Kind) : _Kind(Kind) {}
    virtual ~Node() = default;
};



class TopLevelStmt : public Node {
protected:
    TopLevelStmt(NodeKind Kind) : Node(Kind) {}
};

class LocalStmt : public Node {
protected:
    LocalStmt(NodeKind Kind) : Node(Kind) {}
};

class Expr : public Node {
protected:
    Expr(NodeKind Kind) : Node(Kind) {}
};





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
    
    FunctionDecl() : TopLevelStmt(NodeKind::FunctionDecl), FunctionSignature() {}
};


class ExternFunctionDecl : public TopLevelStmt, public FunctionSignature {
public:
    ExternFunctionDecl() : TopLevelStmt(NodeKind::ExternFunctionDecl), FunctionSignature() {}
};


class Composite : public LocalStmt {
public:
    std::vector<std::shared_ptr<LocalStmt>> Statements;
    
    Composite() : LocalStmt(NodeKind::Composite) {}
};






# pragma mark - Local Statements

class ReturnStmt : public LocalStmt {
public:
    std::shared_ptr<Expr> Expression;
    
    explicit ReturnStmt(std::shared_ptr<Expr> Expression) : LocalStmt(NodeKind::ReturnStmt), Expression(Expression) {}
};



class VariableDecl : public LocalStmt {
public:
    std::shared_ptr<Identifier> Name; // TODO does something like this really warrant a pointer?
    TypeInfo *Type;
    std::shared_ptr<Expr> InitialValue;
    
    VariableDecl(std::shared_ptr<Identifier> Name, TypeInfo *Type, std::shared_ptr<Expr> InitialValue = nullptr)
    : LocalStmt(NodeKind::VariableDecl), Name(Name), Type(Type), InitialValue(InitialValue) {}
};






# pragma mark - Expressions


class Identifier : public Expr {
public:
    const std::string Value;
    Identifier(std::string Value) : Expr(NodeKind::Identifier), Value(Value) {}
    
    operator std::string () { return Value; }
};



class NumberLiteral : public Expr {
public:
    uint64_t Value;
    
    explicit NumberLiteral(uint64_t Value) : Expr(NodeKind::NumberLiteral), Value(Value) {}
};



class FunctionCall : public Expr, public LocalStmt {
public:
    std::shared_ptr<Expr> Target;
    std::vector<std::shared_ptr<Expr>> Arguments;
    bool UnusedReturnValue;
    
    FunctionCall(std::shared_ptr<Expr> Target, std::vector<std::shared_ptr<Expr>> Arguments, bool UnusedReturnValue)
    : Expr(NodeKind::FunctionCall), LocalStmt(NodeKind::FunctionCall), Target(Target), Arguments(Arguments), UnusedReturnValue(UnusedReturnValue) {}
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
    
    BinaryOperation(Operation Op, std::shared_ptr<Expr> LHS, std::shared_ptr<Expr> RHS) : Expr(NodeKind::BinaryOperation), Op(Op), LHS(LHS), RHS(RHS) {}
};


NS_END
