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


std::string Description(AST &Ast);


class Expr;
class Identifier;
class VariableDecl;
class Composite;
class StructDecl;


class Node {
public:
    std::shared_ptr<parser::TokenSourceLocation> startLocation, endLocation;
    
    virtual std::string Description();
    
protected:
    virtual ~Node() = default;
};


class TopLevelStmt : virtual public Node {};

class LocalStmt : virtual public Node {};

class Expr : virtual public Node {
public:
    virtual bool isLiteral() const;
};


// Note: some of the nested subclasses also inherit from ast::Node. one could argue that this is philosophically wrong, but it greatly simplifies ast printing



#pragma mark - Top Level Statements



class FunctionSignature : public Node {
public:
    enum class FunctionKind {
        GlobalFunction,   // A free global function
        StaticMethod,     // A static type member method
        InstanceMethod    // A type instance method
    };
    
    std::string Name;
    FunctionKind Kind;
    TypeInfo *ReturnType;
    std::vector<std::shared_ptr<VariableDecl>> Parameters;
    std::shared_ptr<ast::StructDecl> ImplType; // If this is a static or instance method, the type it is a member of
    
    std::shared_ptr<attributes::FunctionAttributes> attributes;
    
    bool IsTemplateFunction = false;
    std::vector<std::string> TemplateArgumentNames;
    
    bool IsFullSpecialization() {
        return IsTemplateFunction && TemplateArgumentNames.empty();
    }
    
    explicit FunctionSignature() {
        attributes = std::make_shared<attributes::FunctionAttributes>();
    }
};

std::ostream& operator<<(std::ostream&, const std::shared_ptr<ast::FunctionSignature>&);



class FunctionDecl : public TopLevelStmt {
public:
    std::shared_ptr<FunctionSignature> Signature;
    std::shared_ptr<Composite> Body;
    
    FunctionDecl() {}
};


class ExternFunctionDecl : public TopLevelStmt {
public:
    std::shared_ptr<FunctionSignature> Signature;
    
    ExternFunctionDecl() {}
};


class StructDecl : public TopLevelStmt {
public:
    std::shared_ptr<Identifier> Name;
    std::vector<std::shared_ptr<VariableDecl>> Members;
    std::vector<std::string> TemplateArguments;
    std::shared_ptr<attributes::StructAttributes> attributes;
    
    StructDecl() {}
    
    bool IsTemplateStruct() { return !TemplateArguments.empty(); }
};

class ImplBlock : public TopLevelStmt {
public:
    std::string Typename;
    std::vector<std::shared_ptr<FunctionDecl>> Methods;
    
    ImplBlock(std::string Typename) : Typename(Typename) {}
    ImplBlock(std::string Typename, std::vector<std::shared_ptr<FunctionDecl>> Methods) : Typename(Typename), Methods(Methods) {}
};


class TypealiasDecl : public TopLevelStmt {
public:
    std::string Typename;
    TypeInfo *Type;
    
    TypealiasDecl(std::string Typename, TypeInfo *Type) : Typename(Typename), Type(Type) {}
};


# pragma mark - Local Statements


class Composite : public LocalStmt {
public:
    std::vector<std::shared_ptr<LocalStmt>> Statements;
    
    Composite() {}
    Composite(std::vector<std::shared_ptr<LocalStmt>> Statements) : Statements(Statements) {}
    
    bool isEmpty() const {
        return Statements.empty();
    }
};


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
    class Branch : public Node {
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


class WhileStmt : public LocalStmt {
public:
    std::shared_ptr<ast::Expr> Condition;
    std::shared_ptr<ast::Composite> Body;
    
    WhileStmt(std::shared_ptr<Expr> Condition, std::shared_ptr<Composite> Body) : Condition(Condition), Body(Body) {}
};


class ForLoop : public LocalStmt {
public:
    std::shared_ptr<ast::Identifier> ident;
    std::shared_ptr<ast::Expr> expr;
    std::shared_ptr<ast::Composite> body;
    
    ForLoop(std::shared_ptr<ast::Identifier> ident, std::shared_ptr<ast::Expr> expr, std::shared_ptr<ast::Composite> body) : ident(ident), expr(expr), body(body) {}
};





# pragma mark - Expressions


class RawLLVMValueExpr : public Expr {
public:
    llvm::Value *Value;
    TypeInfo *Type;
    
    RawLLVMValueExpr(llvm::Value *Value, TypeInfo *TI) : Value(Value), Type(TI) {}
};


class Identifier : public Expr {
public:
    const std::string Value;
    Identifier(std::string Value) : Value(Value) {}
    
    operator std::string () { return Value; } // TODO is this ever used?
    
    static std::shared_ptr<Identifier> emptyIdent();
};



class NumberLiteral : public Expr {
public:
    enum class NumberType {
        Integer, Double, Boolean, Character
    };
    
    const uint64_t Value;
    const NumberType Type;
    
    explicit NumberLiteral(uint64_t Value, NumberType Type) : Value(Value), Type(Type) {}
    
    static std::shared_ptr<NumberLiteral> Integer(uint64_t Value) {
        return std::make_shared<NumberLiteral>(Value, NumberType::Integer);
    }
};


class StringLiteral : public Expr {
public:
    enum class StringLiteralKind {
        NormalString,   // Becomes a `String` object
        ByteString      // Becomes an `*i8` pointer
    };
    
    std::string Value;
    StringLiteralKind Kind;
    
    explicit StringLiteral(std::string Value, StringLiteralKind Kind) : Value(Value), Kind(Kind) {}
};




class NEW_ExprStmt : public LocalStmt {
public:
    std::shared_ptr<ast::Expr> expr;
    
    explicit NEW_ExprStmt(std::shared_ptr<ast::Expr> expr) : expr(expr) {}
};



// A reference to a static member of a type (for example a static method or an enum value)
class NEW_StaticDeclRefExpr : public Expr {
public:
    std::string typeName;
    std::string memberName;
    
    NEW_StaticDeclRefExpr(const std::string &typeName, const std::string &memberName) : typeName(typeName), memberName(memberName) {}
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
    std::shared_ptr<Expr> Expression;
    TypeInfo *DestType;
    CastKind Kind;
    
    Typecast(std::shared_ptr<Expr> Expression, TypeInfo *DestType, CastKind Kind)
    : Expression(Expression), DestType(DestType), Kind(Kind) {}
};




class MatchExpr : public Expr {
public:
    class MatchExprBranch : public Node {
    public:
        std::vector<std::shared_ptr<Expr>> Patterns;
        std::shared_ptr<Expr> Expression;
        
        MatchExprBranch(std::vector<std::shared_ptr<Expr>> Patterns, std::shared_ptr<Expr> Expression) : Patterns(Patterns), Expression(Expression) {}
    };
    
    std::shared_ptr<Expr> Target;
    std::vector<std::shared_ptr<MatchExprBranch>> Branches;
    
    MatchExpr(std::shared_ptr<Expr> Target, std::vector<std::shared_ptr<MatchExprBranch>> Branches) : Target(Target), Branches(Branches) {}
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


class UnaryExpr : public Expr {
public:
    enum class Operation {
        Negate, BitwiseNot, LogicalNegation
    };
    
    Operation Op;
    std::shared_ptr<ast::Expr> Expr;
    
    UnaryExpr(Operation Op, std::shared_ptr<ast::Expr> Expr) : Op(Op), Expr(Expr) {}
};

NS_END
