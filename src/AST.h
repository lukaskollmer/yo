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
#include "Attributes.h"

namespace llvm {
    class Value;
}

NS_START(ast)


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
    virtual std::string Description();
    
    std::vector<std::string> Annotations;
    //std::vector<yo::attributes::Attribute> attributes;
    
    bool HasAnnotation(const std::string &Annotation) {
        return util::vector::contains(Annotations, Annotation);
    }
    
protected:
    Node() {}
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

class AttributeList : public Node {
public:
    const std::vector<yo::attributes::Attribute> attributes;
    
    explicit AttributeList(std::vector<yo::attributes::Attribute> attributes) : attributes(attributes) {}
};


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
    
    bool IsTemplateFunction = false;
    std::vector<std::string> TemplateArgumentNames;
    
    bool IsFullSpecialization() {
        return IsTemplateFunction && TemplateArgumentNames.empty();
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
    std::vector<std::shared_ptr<VariableDecl>> Attributes;
    std::vector<std::string> TemplateArguments;
    
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
    std::shared_ptr<ast::Identifier> Ident;
    std::shared_ptr<ast::Expr> Expr;
    std::shared_ptr<ast::Composite> Body;
    
    ForLoop(std::shared_ptr<ast::Identifier> Ident, std::shared_ptr<ast::Expr> Expr, std::shared_ptr<ast::Composite> Body) : Ident(Ident), Expr(Expr), Body(Body) {}
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
    
    operator std::string () { return Value; }
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


class FunctionCall : public Expr {
public:
    std::shared_ptr<Identifier> Target;
    std::vector<std::shared_ptr<Expr>> Arguments;
    std::vector<TypeInfo *> ExplicitTemplateArgumentTypes;
    
    FunctionCall(std::shared_ptr<Identifier> Target, std::vector<std::shared_ptr<Expr>> Arguments)
    : Target(Target), Arguments(Arguments) {}
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



// A (chained) member access
class MemberAccess : public Expr, public LocalStmt {
public:
    class Member : public Node {
    public:
        enum class MemberKind {
            Initial_Identifier,     // Value in Data.Ident
            Initial_FunctionCall,   // Value in Data.Call
            Initial_StaticCall,     // Value in Data.Call (Call.Target contains both the type, and the method name, separated by `~`. TODO come up w/ a better solution)
            
            OffsetRead,             // Value in Data.Offset
            MemberFunctionCall,     // Value in Data.Call (call target is name of the method being called)
            MemberAttributeRead     // Value in Data.Ident
        };
        
        union MemberData {
            std::shared_ptr<Identifier> Ident;
            std::shared_ptr<FunctionCall> Call;
            std::shared_ptr<Expr> Offset;
            
            // https://stackoverflow.com/a/40302092/2513803
            MemberData() : Ident{} {}
            ~MemberData() {}
        };
        
        MemberKind Kind;
        MemberData Data;
        
        Member(MemberKind Kind) : Kind(Kind) {}
        Member(MemberKind Kind, std::shared_ptr<Identifier> Ident) : Kind(Kind) {
            precondition(Kind == MemberKind::Initial_Identifier || Kind == MemberKind::MemberAttributeRead);
            Data.Ident = Ident;
        }
        Member(MemberKind Kind, std::shared_ptr<FunctionCall> Call) : Kind(Kind) {
            precondition(Kind == MemberKind::Initial_FunctionCall
                         || Kind == MemberKind::Initial_StaticCall
                         || Kind == MemberKind::MemberFunctionCall);
            Data.Call = Call;
        }
        Member(MemberKind Kind, std::shared_ptr<Expr> Offset) : Kind(Kind) {
            precondition(Kind == MemberKind::OffsetRead);
            Data.Offset = Offset;
        }
        ~Member();
    };
    
    std::vector<std::shared_ptr<Member>> Members;
    
    MemberAccess(std::vector<std::shared_ptr<Member>> Members) : Members(Members) {}
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
