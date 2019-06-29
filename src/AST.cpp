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




static std::shared_ptr<ast::Identifier> _emptyIdent;

std::shared_ptr<ast::Identifier> ast::Identifier::emptyIdent() {
    if (!_emptyIdent) {
        _emptyIdent = std::make_shared<ast::Identifier>("");
    }
    return _emptyIdent;
}





#pragma mark - AST Printing

std::ostream& ast::operator<<(std::ostream &OS, const std::shared_ptr<ast::FunctionSignature> &signature) {
    OS << "fn " << signature->name;
    if (signature->isTemplateFunction) {
        OS << "<";
        for (auto it = signature->templateArgumentNames.begin(); it != signature->templateArgumentNames.end(); it++) {
            OS << *it;
            if (it + 1 != signature->templateArgumentNames.end()) {
                OS << ", ";
            }
        }
        OS << ">";
    }
    OS << "(";
    
    for (auto it = signature->parameters.begin(); it != signature->parameters.end(); it++) {
        OS << (*it)->type->str();
        if (it + 1 != signature->parameters.end()) {
            OS << ", ";
        }
    }
    OS << "): " << signature->returnType->str();
    return OS;
}


#define CASE(n) case E::n: return #n;

std::string FunctionKindToString(FunctionSignature::FunctionKind kind) {
    using E = FunctionSignature::FunctionKind;
    switch (kind) {
        CASE(GlobalFunction)
        CASE(StaticMethod)
        CASE(InstanceMethod)
    }
}

std::string BinopOperationToString(BinaryOperation::Operation op) {
    using E = BinaryOperation::Operation;
    switch (op) {
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


std::string ComparisonOpToString(Comparison::Operation op) {
    using E = Comparison::Operation;
    switch (op) {
        CASE(EQ)
        CASE(NE)
        CASE(LT)
        CASE(LE)
        CASE(GT)
        CASE(GE)
    }
}


std::string LogicalOperationOperatorToString(LogicalOperation::Operation op) {
    using E = LogicalOperation::Operation;
    switch (op) {
        CASE(And)
        CASE(Or)
    }
}


std::string IfStmtBranchKindToString(IfStmt::Branch::BranchKind kind) {
    using E = IfStmt::Branch::BranchKind;
    switch (kind) {
        CASE(If)
        CASE(ElseIf)
        CASE(Else)
    }
}


std::string StringLiteralKindToString(StringLiteral::StringLiteralKind kind) {
    using E = StringLiteral::StringLiteralKind;
    switch (kind) {
        CASE(NormalString)
        CASE(ByteString)
    }
}

std::string NumberTypeToString(NumberLiteral::NumberType type) {
    using E = NumberLiteral::NumberType;
    switch (type) {
        CASE(Integer)
        CASE(Double)
        CASE(Character)
        CASE(Boolean)
    }
}

std::string UnaryExprOpToString(UnaryExpr::Operation op) {
    using E = UnaryExpr::Operation;
    switch (op) {
        CASE(Negate)
        CASE(BitwiseNot)
        CASE(LogicalNegation)
    }
}

#undef CASE








inline constexpr unsigned INDENT_SIZE = 2;



template <typename T>
std::string ast_description(std::vector<std::shared_ptr<T>> _nodes) {
    std::vector<std::shared_ptr<T>> nodes(_nodes.begin(), _nodes.end());
    
    std::string desc;
    desc += "std::vector<";
    desc += util::typeinfo::TypeInfo<T>::name;
    desc += "> [\n";
    
    for (auto it = nodes.begin(); it != nodes.end(); it++) {
        util::string::append_with_indentation(desc,
                                              *it ? (*it)->description() : "<nullptr>",
                                              INDENT_SIZE);
        
        if (it + 1 != nodes.end()) {
            desc += ",";
        }
        desc += "\n";
    }
    desc += "]";
    
    return desc;
}


std::string ast::description(AST &ast) {
    return ast_description(ast);
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
        return arg->str();
    
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
        return arg->description();
    
    } else if constexpr(util::typeinfo::is_vector_of_convertible_v<T, std::shared_ptr<Node>>) {
        return ast_description(arg);
    
    } else {
        // this will always fail, but if it does, we get a nice compile-time error message which includes the typename of T
        static_assert(std::is_null_pointer_v<T>, "ugh");
        throw;
    }
}






struct AttributeDescription {
    const std::string key;
    const std::string value;
    
    template <typename T>
    AttributeDescription(const std::string key, T value) : key(key), value(to_string(value)) {}
};

using Mirror = std::vector<AttributeDescription>;


Mirror Reflect(FunctionSignature *signature) {
    // TODO use the << operator instead?
    return {
        { "name", signature->name },
        { "kind", signature->kind },
        { "parameters", signature->parameters },
        { "returnType", signature->returnType },
        { "implType", signature->implType }
    };
}

Mirror Reflect(FunctionDecl *FD) {
    return {
        { "signature", FD->signature },
        { "body", FD->body }
    };
}

Mirror Reflect(Composite *C) {
    return {
        { "body", C->statements }
    };
}

Mirror Reflect(ReturnStmt *ret) {
    return {
        { "expr", ret->expression }
    };
}

Mirror Reflect(NumberLiteral *number) {
    return {
        { "type", number->type },
        { "value", number->value }
    };
}

Mirror Reflect(Identifier *ident) {
    return {
        { "value", ident->value }
    };
}

Mirror Reflect(BinaryOperation *binop) {
    return {
        { "op", binop->op },
        { "lhs", binop->lhs },
        { "rhs", binop->rhs }
    };
}

Mirror Reflect(VariableDecl *decl) {
    return {
        { "name", decl->name },
        { "type", decl->type },
        { "initial value", decl->initialValue }
    };
}

Mirror Reflect(Comparison *cmp) {
    return {
        { "op", cmp->op },
        { "lhs", cmp->lhs },
        { "rhs", cmp->rhs }
    };
}

Mirror Reflect(LogicalOperation *logOp) {
    return {
        { "op", logOp->op },
        { "lhs", logOp->lhs },
        { "rhs", logOp->rhs }
    };
}

Mirror Reflect(IfStmt *If) {
    return {
        { "branches", If->branches }
    };
}

Mirror Reflect(IfStmt::Branch *branch) {
    return {
        { "kind", branch->kind },
        { "condition", branch->condition },
        { "body", branch->body },
    };
}

Mirror Reflect(Assignment *assignment) {
    return {
        { "target", assignment->target },
        { "value", assignment->value }
    };
}

Mirror Reflect(Typecast *cast) {
    return {
        { "type", cast->destType },
        { "expr", cast->expression }
    };
}


Mirror Reflect(StructDecl *Struct) {
    return {
        { "name", Struct->name },
        { "members", Struct->members }
    };
}

Mirror Reflect(ImplBlock *implBlock) {
    return {
        { "typename", implBlock->typename_ },
        { "methods", implBlock->methods }
    };
}

Mirror Reflect(StringLiteral *SL) {
    return {
        { "kind", SL->kind },
        { "value", SL->value }
    };
}

Mirror Reflect(UnaryExpr *unaryExpr) {
    return {
        { "operation", unaryExpr->op },
        { "expr", unaryExpr->expr }
    };
}

Mirror Reflect(MatchExpr *matchExpr) {
    return {
        { "target", matchExpr->target },
        { "branches", matchExpr->branches }
    };
}

Mirror Reflect(MatchExpr::MatchExprBranch *branch) {
    return {
        { "patterns", branch->patterns },
        { "expr", branch->expression }
    };
}

Mirror Reflect(ast::CallExpr *callExpr) {
    std::string explicitTemplateArgumentTypes = "[ ";
    for (auto it = callExpr->explicitTemplateArgumentTypes.begin(); it != callExpr->explicitTemplateArgumentTypes.end(); it++) {
        explicitTemplateArgumentTypes.append((*it)->str());
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
        { "condition", whileStmt->condition },
        { "body", whileStmt->body },
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

Mirror Reflect(ast::TypealiasDecl *typealias) {
    return {
        { "name", typealias->typename_ },
        { "type", typealias->type }
    };
}




Mirror Reflect(Node *node) {
#define HANDLE(T) if (auto X = dynamic_cast<T*>(node)) return Reflect(X);
    
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
    HANDLE(ast::TypealiasDecl)
    
    std::cout << "[Reflect] Unhandled Node: " << util::typeinfo::getTypename(*node) << std::endl;
    throw;

#undef HANDLE
}



std::string Node::description() {
    std::string desc;
    
    desc.append(util::typeinfo::getTypename(*this)).append(" [");
    auto M = Reflect(this);
    
    if (M.empty()) {
        return desc + "]";
    }
    
    desc += "\n";
    
    for (auto it = M.begin(); it != M.end(); it++) {
        auto &[key, value] = *it;
        util::string::append_with_indentation(desc, key + ": " + value, INDENT_SIZE);
        
        if (it + 1 != M.end()) {
            desc += ",";
        }
        desc += "\n";
    }
    desc += "]";
    
    return desc;
}






