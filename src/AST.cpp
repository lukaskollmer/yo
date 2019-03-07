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

using namespace ast;

std::string FunctionKindToString(FunctionSignature::FunctionKind Kind) {
    switch (Kind) {
    case FunctionSignature::FunctionKind::Global:   return "global";
    case FunctionSignature::FunctionKind::Static:   return "static";
    case FunctionSignature::FunctionKind::Instance: return "instance";
    }
}


std::string NodeKindToString(Node::NodeKind Kind) {
#define CASE(x) case K::x: return "ast::"#x;
    using K = Node::NodeKind;
    switch (Kind) {
    case K::Node:
    case K::Expr:
    case K::Stmt:
    case K::TopLevelStmt:
        throw;
    CASE(LocalStmt)
    CASE(FunctionDecl)
    CASE(ReturnStmt)
    CASE(Composite)
    CASE(VariableDecl)
    CASE(NumberLiteral)
    CASE(StringLiteral)
    CASE(ArrayLiteral)
    CASE(Identifier)
    }
#undef CASE
}




inline constexpr unsigned INDENT_SIZE = 2;



template <typename T>
std::string ast::ast_description(std::vector<std::shared_ptr<T>> _Nodes) {
    std::vector<std::shared_ptr<T>> Nodes(_Nodes.begin(), _Nodes.end());
    
    std::string Desc = util::typeinfo::LKTypeInfo<T>::Name;
    Desc += " [\n";
    
    for (auto It = Nodes.begin(); It != Nodes.end(); It++) {
        util::string::append_with_indentation(Desc, (*It)->Description(), INDENT_SIZE);
        
        if (It + 1 != Nodes.end()) {
            Desc += ",";
        }
        Desc += "\n";
    }
    Desc += "]";
    
    return Desc;
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
    } else if constexpr(std::is_convertible_v<T, std::shared_ptr<Node>>) {
        return arg->Description();
    } else if constexpr(util::typeinfo::is_vector_of_convertible_v<T, std::shared_ptr<Node>>) {
        return ast_description(arg);
    } else {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-value"
        // Sole purpose of this cast is getting a compiler warning that includes the specific type of `T`
        static_cast<std::nullptr_t>(arg);
#pragma clang diagnostic push
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


Mirror Reflect(FunctionDecl *FD) {
    return {
        { "name", FD->Name },
        { "kind", FD->Kind },
        { "returnType", FD->ReturnType },
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
        { "value", Number->Value }
    };
}

Mirror Reflect(Node *Node) {
    if (auto FD = dynamic_cast<FunctionDecl *>(Node)) {
        return Reflect(FD);
    } else if (auto C = dynamic_cast<Composite *>(Node)) {
        return Reflect(C);
    } else if (auto Return = dynamic_cast<ReturnStmt *>(Node)) {
        return Reflect(Return);
    } else if (auto Number = dynamic_cast<NumberLiteral *>(Node)) {
        return Reflect(Number);
    }
    
    std::cout << "[Reflect] Unhandled Node: " << util::typeinfo::GetTypename(*Node) << std::endl;
    throw;
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






