//
//  AST.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-03.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "AST.h"

#include <string>
#include <iostream>
#include <ostream>
#include <sstream>
#include <strstream>
#include <map>



// TODO
// Some node kinds always fit on a single line (identifiers, numbers, maybe some other literals?). Implement that!



using namespace yo;
using namespace yo::ast;

// TODO this is stupid
bool ast::Expr::isLiteral() const {
    return dynamic_cast<const ast::NumberLiteral *>(this)
        || dynamic_cast<const ast::StringLiteral *>(this);
}



static std::shared_ptr<ast::Identifier> _emptyIdent;

std::shared_ptr<ast::Identifier> ast::Identifier::EmptyIdent() {
    if (!_emptyIdent) {
        _emptyIdent = std::make_shared<ast::Identifier>("");
    }
    return _emptyIdent;
}





#pragma mark - AST Printing

std::ostream& ast::operator<<(std::ostream &OS, const std::shared_ptr<ast::FunctionSignature> &Signature) {
    OS << "fn " << Signature->Name;
    if (Signature->IsTemplateFunction) {
        OS << "<";
        for (auto It = Signature->TemplateArgumentNames.begin(); It != Signature->TemplateArgumentNames.end(); It++) {
            OS << *It;
            if (It + 1 != Signature->TemplateArgumentNames.end()) {
                OS << ", ";
            }
        }
        OS << ">";
    }
    OS << "(";
    
    for (auto It = Signature->Parameters.begin(); It != Signature->Parameters.end(); It++) {
        OS << (*It)->Type->Str();
        if (It + 1 != Signature->Parameters.end()) {
            OS << ", ";
        }
    }
    OS << "): " << Signature->ReturnType->Str();
    return OS;
}


#define CASE(n) case E::n: return #n;

std::string FunctionKindToString(FunctionSignature::FunctionKind Kind) {
    using E = FunctionSignature::FunctionKind;
    switch (Kind) {
        CASE(GlobalFunction)
        CASE(StaticMethod)
        CASE(InstanceMethod)
    }
}

std::string BinopOperationToString(BinaryOperation::Operation Op) {
    using E = BinaryOperation::Operation;
    switch (Op) {
        CASE(Add)
        CASE(Sub)
        CASE(Mul)
        CASE(Div)
        CASE(Mod)
        CASE(And)
        CASE(Or)
        CASE(Xor)
        CASE(Shl)
        CASE(Shr)
    }
}


std::string ComparisonOpToString(Comparison::Operation Op) {
    using E = Comparison::Operation;
    switch (Op) {
        CASE(EQ)
        CASE(NE)
        CASE(LT)
        CASE(LE)
        CASE(GT)
        CASE(GE)
    }
}


std::string LogicalOperationOperatorToString(LogicalOperation::Operation Op) {
    using E = LogicalOperation::Operation;
    switch (Op) {
        CASE(And)
        CASE(Or)
    }
}


std::string IfStmtBranchKindToString(IfStmt::Branch::BranchKind Kind) {
    using E = IfStmt::Branch::BranchKind;
    switch (Kind) {
        CASE(If)
        CASE(ElseIf)
        CASE(Else)
    }
}


std::string StringLiteralKindToString(StringLiteral::StringLiteralKind Kind) {
    using E = StringLiteral::StringLiteralKind;
    switch (Kind) {
        CASE(NormalString)
        CASE(ByteString)
    }
}

std::string NumberTypeToString(NumberLiteral::NumberType Type) {
    using E = NumberLiteral::NumberType;
    switch (Type) {
        CASE(Integer)
        CASE(Double)
        CASE(Character)
        CASE(Boolean)
    }
}

std::string UnaryExprOpToString(UnaryExpr::Operation Op) {
    using E = UnaryExpr::Operation;
    switch (Op) {
        CASE(Negate)
        CASE(BitwiseNot)
        CASE(LogicalNegation)
    }
}

#undef CASE








inline constexpr unsigned INDENT_SIZE = 2;



template <typename T>
std::string ast_description(std::vector<std::shared_ptr<T>> _Nodes) {
    std::vector<std::shared_ptr<T>> Nodes(_Nodes.begin(), _Nodes.end());
    
    std::string Desc;
    Desc += "std::vector<";
    Desc += util::typeinfo::LKTypeInfo<T>::Name;
    Desc += "> [\n";
    
    for (auto It = Nodes.begin(); It != Nodes.end(); It++) {
        util::string::append_with_indentation(Desc,
                                              *It ? (*It)->Description() : "<nullptr>",
                                              INDENT_SIZE);
        
        if (It + 1 != Nodes.end()) {
            Desc += ",";
        }
        Desc += "\n";
    }
    Desc += "]";
    
    return Desc;
}


std::string ast::Description(AST &Ast) {
    return ast_description(Ast);
}



template <typename T>
std::string to_string(T arg) {
    if constexpr(std::is_same_v<T, const char *>) {
        return std::string(arg);
    
    } else if constexpr(std::is_base_of_v<std::string, T>) {
        return arg;
    
    } else if constexpr(std::is_integral_v<T>) {
        return std::to_string(arg);
    
    } else if constexpr(std::is_same_v<T, TypeInfo *>) {
        return arg->Str();
    
    } else if constexpr(std::is_same_v<T, FunctionSignature::FunctionKind>) {
        return FunctionKindToString(arg);
    
    } else if constexpr(std::is_same_v<T, BinaryOperation::Operation>) {
        return BinopOperationToString(arg);
    
    } else if constexpr(std::is_same_v<T, Comparison::Operation>) {
        return ComparisonOpToString(arg);
    
    } else if constexpr(std::is_same_v<T, LogicalOperation::Operation>) {
        return LogicalOperationOperatorToString(arg);
    
    } else if constexpr(std::is_same_v<T, IfStmt::Branch::BranchKind>) {
        return IfStmtBranchKindToString(arg);
    
    } else if constexpr(std::is_same_v<T, StringLiteral::StringLiteralKind>) {
        return StringLiteralKindToString(arg);
        
    } else if constexpr(std::is_same_v<T, NumberLiteral::NumberType>) {
        return NumberTypeToString(arg);
        
    } else if constexpr(std::is_same_v<T, UnaryExpr::Operation>) {
        return UnaryExprOpToString(arg);
    
    } else if constexpr(std::is_convertible_v<T, std::shared_ptr<Node>> || (std::is_pointer_v<T> && std::is_base_of_v<Node, typename std::remove_pointer_t<T>>)) {
        if (!arg) return "<nullptr>";
        return arg->Description();
    
    } else if constexpr(util::typeinfo::is_vector_of_convertible_v<T, std::shared_ptr<Node>>) {
        return ast_description(arg);
    
    } else {
        // this will always fail, but if it does, we get a nice compile-time error message which includes the typename of T
        static_assert(std::is_null_pointer_v<T>, "ugh");
        throw;
    }
}






struct AttributeDescription {
    const std::string Key;
    const std::string Value;
    
    template <typename T>
    AttributeDescription(const std::string Key, T Value) : Key(Key), Value(to_string(Value)) {}
};

using Mirror = std::vector<AttributeDescription>;


Mirror Reflect(FunctionSignature *Signature) {
    // TODO use the << operator instead?
    return {
        { "name", Signature->Name },
        { "kind", Signature->Kind },
        { "parameters", Signature->Parameters },
        { "returnType", Signature->ReturnType },
        { "implType", Signature->ImplType }
    };
}

Mirror Reflect(FunctionDecl *FD) {
    return {
        { "signature", FD->Signature },
        { "body", FD->Body }
    };
}

Mirror Reflect(Composite *C) {
    return {
        { "body", C->Statements }
    };
}

Mirror Reflect(ReturnStmt *Return) {
    return {
        { "expr", Return->Expression }
    };
}

Mirror Reflect(NumberLiteral *Number) {
    return {
        { "type", Number->Type },
        { "value", Number->Value }
    };
}

Mirror Reflect(Identifier *Ident) {
    return {
        { "value", Ident->Value }
    };
}

Mirror Reflect(BinaryOperation *Binop) {
    return {
        { "op", Binop->Op },
        { "lhs", Binop->LHS },
        { "rhs", Binop->RHS }
    };
}

Mirror Reflect(VariableDecl *Decl) {
    return {
        { "name", Decl->Name },
        { "type", Decl->Type },
        { "initial value", Decl->InitialValue }
    };
}

Mirror Reflect(Comparison *Comp) {
    return {
        { "op", Comp->Op },
        { "lhs", Comp->LHS },
        { "rhs", Comp->RHS }
    };
}

Mirror Reflect(LogicalOperation *LogOp) {
    return {
        { "op", LogOp->Op },
        { "lhs", LogOp->LHS },
        { "rhs", LogOp->RHS }
    };
}

Mirror Reflect(IfStmt *If) {
    return {
        { "branches", If->Branches }
    };
}

Mirror Reflect(IfStmt::Branch *Branch) {
    return {
        { "kind", Branch->Kind },
        { "condition", Branch->Condition },
        { "body", Branch->Body },
    };
}

Mirror Reflect(Assignment *Assignment) {
    return {
        { "target", Assignment->Target },
        { "value", Assignment->Value }
    };
}

Mirror Reflect(Typecast *Cast) {
    return {
        { "type", Cast->DestType },
        { "expr", Cast->Expression }
    };
}


Mirror Reflect(StructDecl *Struct) {
    return {
        { "name", Struct->Name },
        { "members", Struct->Members }
    };
}

Mirror Reflect(ImplBlock *ImplBlock) {
    return {
        { "typename", ImplBlock->Typename },
        { "methods", ImplBlock->Methods }
    };
}

Mirror Reflect(StringLiteral *String) {
    return {
        { "kind", String->Kind },
        { "value", String->Value }
    };
}

Mirror Reflect(UnaryExpr *UnaryExpr) {
    return {
        { "operation", UnaryExpr->Op },
        { "expr", UnaryExpr->Expr }
    };
}

Mirror Reflect(MatchExpr *MatchExpr) {
    return {
        { "target", MatchExpr->Target },
        { "branches", MatchExpr->Branches }
    };
}

Mirror Reflect(MatchExpr::MatchExprBranch *Branch) {
    return {
        { "patterns", Branch->Patterns },
        { "expr", Branch->Expression }
    };
}

Mirror Reflect(ast::CallExpr *callExpr) {
    std::string explicitTemplateArgumentTypes = "[ ";
    for (auto it = callExpr->explicitTemplateArgumentTypes.begin(); it != callExpr->explicitTemplateArgumentTypes.end(); it++) {
        explicitTemplateArgumentTypes.append((*it)->Str());
        if (it + 1 != callExpr->explicitTemplateArgumentTypes.end()) {
            explicitTemplateArgumentTypes.append(", ");
        }
    }
    explicitTemplateArgumentTypes.append(" ]");
    
    return {
        { "target", callExpr->target },
        { "arguments", callExpr->arguments },
        { "explicitTemplateArgumentTypes", explicitTemplateArgumentTypes }
    };
}

Mirror Reflect(ast::MemberExpr *memberExpr) {
    return {
        { "target", memberExpr->target },
        { "memberName", memberExpr->memberName }
    };
}

Mirror Reflect(ast::StaticDeclRefExpr *staticDeclRefExpr) {
    return {
        { "typeName", staticDeclRefExpr->typeName },
        { "memberName", staticDeclRefExpr->memberName }
    };
}

Mirror Reflect(ast::WhileStmt *whileStmt) {
    return {
        { "condition", whileStmt->Condition },
        { "body", whileStmt->Body },
    };
}

Mirror Reflect(ast::SubscriptExpr *subscriptExpr) {
    return {
        { "target", subscriptExpr->target },
        { "offset", subscriptExpr->offset },
    };
}

Mirror Reflect(ast::ExprStmt *exprStmt) {
    return {
        { "expr", exprStmt->expr }
    };
}




Mirror Reflect(Node *Node) {
#define HANDLE(T) if (auto X = dynamic_cast<T*>(Node)) return Reflect(X);
    
    HANDLE(FunctionDecl)
    HANDLE(Composite)
    HANDLE(ReturnStmt)
    HANDLE(NumberLiteral)
    HANDLE(Identifier)
    HANDLE(BinaryOperation)
    HANDLE(VariableDecl)
    HANDLE(Comparison)
    HANDLE(LogicalOperation)
    HANDLE(IfStmt)
    HANDLE(IfStmt::Branch)
    HANDLE(Assignment)
    HANDLE(Typecast)
    HANDLE(StructDecl)
    HANDLE(ImplBlock)
    HANDLE(StringLiteral)
    HANDLE(FunctionSignature)
    HANDLE(UnaryExpr)
    HANDLE(MatchExpr)
    HANDLE(MatchExpr::MatchExprBranch)
    HANDLE(ast::CallExpr)
    HANDLE(ast::MemberExpr)
    HANDLE(ast::StaticDeclRefExpr)
    HANDLE(ast::WhileStmt)
    HANDLE(ast::SubscriptExpr)
    HANDLE(ast::ExprStmt)
    
    std::cout << "[Reflect] Unhandled Node: " << util::typeinfo::GetTypename(*Node) << std::endl;
    throw;

#undef HANDLE
}



std::string Node::Description() {
    std::string Desc;
    
    Desc.append(util::typeinfo::GetTypename(*this)).append(" [");
    auto M = Reflect(this);
    
    if (M.empty()) {
        return Desc + "]";
    }
    
    Desc += "\n";
    
    for (auto It = M.begin(); It != M.end(); It++) {
        auto &[Key, Value] = *It;
        util::string::append_with_indentation(Desc, Key + ": " + Value, INDENT_SIZE);
        
        if (It + 1 != M.end()) {
            Desc += ",";
        }
        Desc += "\n";
    }
    Desc += "]";
    
    return Desc;
}






