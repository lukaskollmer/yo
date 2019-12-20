//
//  AST.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-03.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "AST.h"
#include "Mangling.h"

#include <string>
#include <iostream>
#include <ostream>
#include <sstream>
#include <strstream>
#include <map>
#include <algorithm>


// TODO make the printed ast output look better / nicer / more readable. maybe like what clang is doing?


using namespace yo;
using namespace yo::ast;



bool FunctionDecl::isOperatorOverloadFor(Operator op) const {
    return name == mangling::encodeOperator(op);
}

const std::string& VarDecl::getName() const {
    return ident->value;
}


#pragma mark - AST Printing

std::ostream& ast::operator<<(std::ostream &OS, const ast::FunctionSignature &signature) {
    if (signature.isTemplateDecl()) {
        OS << "<";
        util::vector::iterl(signature.templateParamsDecl->getParams(), [&OS](const TemplateParamDeclList::Param &P, bool isLast) {
            OS << P.name;
            if (auto defaultType = P.defaultType) {
                OS << " = " << defaultType;
            }
            if (!isLast) OS << ", ";
        });
        OS << ">";
    }
    OS << "(";

    for (auto it = signature.paramTypes.begin(); it != signature.paramTypes.end(); it++) {
        OS << (*it)->str();
        if (it + 1 != signature.paramTypes.end()) {
            OS << ", ";
        } else if (signature.isVariadic) {
            OS << "...";
        }
    }
    OS << ") -> " << signature.returnType->str();
    return OS;
}


#define CASE(n) case E::n: return #n;

std::string FunctionKindToString(FunctionKind kind) {
    using E = FunctionKind;
    switch (kind) {
        CASE(GlobalFunction)
        CASE(StaticMethod)
        CASE(InstanceMethod)
        CASE(OperatorOverload)
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
        CASE(AddressOf)
    }
}


std::string operatorToString(ast::Operator op) {
    using E = ast::Operator;
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
        CASE(Neg)
        CASE(BNot)
        CASE(BNeg)
        CASE(LAnd)
        CASE(LOr)
        CASE(EQ)
        CASE(NE)
        CASE(LT)
        CASE(LE)
        CASE(GT)
        CASE(GE)
        CASE(FnPipe)
        CASE(FnCall)
        CASE(Subscript)
        CASE(Assign)
    }
}

#undef CASE








inline constexpr unsigned INDENT_SIZE = 2;




template <typename T, typename U>
std::string ast_vec_desc(const std::vector<T> &vec, std::string(U::*desc_fn_ptr)() const) {
    std::ostringstream OS;
    std::string it_desc;
    
    OS << "std::vector<" << util::typeinfo::TypeInfo<T>::name << "> [\n";
    
    for (auto it = vec.begin(); it != vec.end(); it++) {
        if constexpr(util::typeinfo::is_nullable_v<T>) {
            if (!*it) {
                OS << "<nullptr>";
                goto fin;
            }
        }
        
        if constexpr(util::typeinfo::is_shared_ptr_v<T>) {
            it_desc = ((*it).get()->*desc_fn_ptr)();
        } else if constexpr(std::is_pointer_v<T>) {
            it_desc = ((*it)->*desc_fn_ptr)();
        } else {
            it_desc = ((*it).*desc_fn_ptr)();
        }
        util::string::append_with_indentation(OS, it_desc, INDENT_SIZE);
        
    fin:
        if (it + 1 != vec.end()) {
            OS << ",";
        }
        OS << "\n";
    }
    
    OS << "]";
    return OS.str();
}





std::string ast::description(const AST& ast) {
    return ast_vec_desc(ast, &Node::description);
}



template <typename T>
std::string to_string(T arg) {
    if constexpr(std::is_pointer_v<T> || util::typeinfo::is_shared_ptr_v<T>) {
        if (!arg) return "<nullptr>";
    }
    
    if constexpr(std::is_same_v<T, const char *>) {
        return std::string(arg);
    
    } else if constexpr(std::is_base_of_v<std::string, T>) {
        return arg;
    
    } else if constexpr(std::is_integral_v<T>) {
        return std::to_string(arg);
    
    } else if constexpr(std::is_same_v<T, TypeDesc*> || std::is_convertible_v<T, std::shared_ptr<TypeDesc>>) {
        return arg->str();
        
    } else if constexpr(std::is_base_of_v<irgen::Type, typename std::remove_pointer_t<T>>) {
        return arg->str();
    
    } else if constexpr(std::is_same_v<T, FunctionKind>) {
        return FunctionKindToString(arg);
    
    } else if constexpr(std::is_same_v<T, IfStmt::Branch::BranchKind>) {
        return IfStmtBranchKindToString(arg);
    
    } else if constexpr(std::is_same_v<T, StringLiteral::StringLiteralKind>) {
        return StringLiteralKindToString(arg);
        
    } else if constexpr(std::is_same_v<T, NumberLiteral::NumberType>) {
        return NumberTypeToString(arg);
        
    } else if constexpr(std::is_same_v<T, UnaryExpr::Operation>) {
        return UnaryExprOpToString(arg);
        
    } else if constexpr(std::is_same_v<T, ast::Operator>) {
        return operatorToString(arg);
    
    } else if constexpr(std::is_convertible_v<T, std::shared_ptr<Node>> || (std::is_pointer_v<T> && std::is_base_of_v<Node, typename std::remove_pointer_t<T>>)) {
        return arg->description();
        
    } else if constexpr(std::is_convertible_v<T, const Node&>) {
        return arg.description();
        
    } else if constexpr(util::typeinfo::is_vector_of_convertible_v<T, std::shared_ptr<Node>>) {
        return ast_vec_desc(arg, &Node::description);
    
    } else if constexpr(util::typeinfo::is_vector_of_convertible_v<T, std::shared_ptr<TypeDesc>>) {
        return ast_vec_desc(arg, &TypeDesc::str);
        
    } else if constexpr(util::typeinfo::is_vector_v<T> && std::is_base_of_v<Node, typename T::value_type>) {
        return ast_vec_desc(arg, &Node::description);
    
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


Mirror Reflect(const FunctionSignature* signature) {
    return {
        { "paramTypes", signature->paramTypes },
        { "returnType", signature->returnType },
        { "isVariadic", signature->isVariadic }
    };
}

Mirror Reflect(const FunctionDecl *FD) {
    return {
        { "funcKind", FD->getFunctionKind() },
        { "name", FD->getName() },
        { "signature", FD->getSignature() },
        //{ "attributes", FD->getAttributes() }, // TODO
        { "body", FD->getBody() },
        { "implType", FD->getImplType() }
    };
}

Mirror Reflect(const CompoundStmt *C) {
    return {
        { "body", C->statements }
    };
}

Mirror Reflect(const ReturnStmt *ret) {
    return {
        { "expr", ret->expr }
    };
}

Mirror Reflect(const NumberLiteral *number) {
    return {
        { "type", number->type },
        { "value", number->value }
    };
}

Mirror Reflect(const Ident *ident) {
    return {
        { "value", ident->value }
    };
}

Mirror Reflect(const VarDecl *decl) {
    return {
        { "ident", decl->ident },
        { "type", decl->type },
        { "initial value", decl->initialValue }
    };
}

Mirror Reflect(const IfStmt *If) {
    return {
        { "branches", If->branches },
    };
}

Mirror Reflect(const IfStmt::Branch *branch) {
    return {
        { "kind", branch->kind },
        { "condition", branch->condition },
        { "body", branch->body },
    };
}

Mirror Reflect(const Assignment *assignment) {
    return {
        { "target", assignment->target },
        { "value", assignment->value }
    };
}

Mirror Reflect(const CastExpr *cast) {
    return {
        { "type", cast->destType },
        { "expr", cast->expr }
    };
}


Mirror Reflect(const StructDecl *Struct) {
    return {
        { "name", Struct->name },
        { "members", Struct->members }
    };
}

Mirror Reflect(const ImplBlock *implBlock) {
    return {
        { "typename", implBlock->typename_ },
        { "methods", implBlock->methods }
    };
}

Mirror Reflect(const StringLiteral *SL) {
    return {
        { "kind", SL->kind },
        { "value", SL->value }
    };
}

Mirror Reflect(const UnaryExpr *unaryExpr) {
    return {
        { "operation", unaryExpr->op },
        { "expr", unaryExpr->expr }
    };
}

Mirror Reflect(const MatchExpr *matchExpr) {
    return {
        { "target", matchExpr->target },
        { "branches", matchExpr->branches }
    };
}

Mirror Reflect(const MatchExpr::MatchExprBranch *branch) {
    return {
        { "patterns", branch->patterns },
        { "expr", branch->expression }
    };
}

Mirror Reflect(const ast::CallExpr *callExpr) {
    std::ostringstream OS;
    OS << "[ ";
    if (callExpr->hasExplicitTemplateArgs()) {
        util::vector::iterl(callExpr->explicitTemplateArgs->elements, [&OS](auto &param, bool isLast) {
            OS << param->str();
            if (!isLast) OS << ", ";
        });
    }
////    util::vector::iterl(callExpr->explicitTemplateParams, <#F &&fn#>)
//    for (auto it = callExpr->explicitTemplateArgumentTypes.begin(); it != callExpr->explicitTemplateArgumentTypes.end(); it++) {
//        OS << (*it)->str();
//        if (it + 1 != callExpr->explicitTemplateArgumentTypes.end()) {
//            OS << ", ";
//        }
//    }
    OS << " ]";
    
    return {
        { "target", callExpr->target },
        { "arguments", callExpr->arguments },
        { "explicitTemplateArgumentTypes", OS.str() }
    };
}

Mirror Reflect(const ast::MemberExpr *memberExpr) {
    return {
        { "target", memberExpr->target },
        { "memberName", memberExpr->memberName }
    };
}

Mirror Reflect(const ast::StaticDeclRefExpr *staticDeclRefExpr) {
    return {
        { "typeName", staticDeclRefExpr->typeName },
        { "memberName", staticDeclRefExpr->memberName }
    };
}

Mirror Reflect(const ast::WhileStmt *whileStmt) {
    return {
        { "condition", whileStmt->condition },
        { "body", whileStmt->body },
    };
}

Mirror Reflect(const ast::SubscriptExpr *subscriptExpr) {
    return {
        { "target", subscriptExpr->target },
        { "offset", subscriptExpr->offset },
    };
}

Mirror Reflect(const ast::ExprStmt *exprStmt) {
    return {
        { "expr", exprStmt->expr }
    };
}

Mirror Reflect(const ast::TypealiasDecl *typealias) {
    return {
        { "name", typealias->typename_ },
        { "type", typealias->type }
    };
}

Mirror Reflect(const ast::BinOp *binop) {
    return {
        { "op", binop->getOperator() },
        { "lhs", binop->getLhs() },
        { "rhs", binop->getRhs() }
    };
}

Mirror Reflect(const ast::RawLLVMValueExpr *rawLLVMExpr) {
    return {
        { "type", ast::TypeDesc::makeResolved(rawLLVMExpr->type) },
        { "value", "TODO" },
    };
}


Mirror Reflect(const Node *node) {
#define CASE(ty) case NK::ty: return Reflect(static_cast<const ty *>(node));
#define CASE2(c, ty) case NK::c: return Reflect(static_cast<const ty *>(node));
    using NK = Node::NodeKind;
    
    switch (node->getNodeKind()) {
        CASE(FunctionDecl)
        CASE(CompoundStmt)
        CASE(ReturnStmt)
        CASE(NumberLiteral)
        CASE(Ident)
        CASE(VarDecl)
        CASE(IfStmt)
        CASE2(IfStmtBranch, IfStmt::Branch)
        CASE(Assignment)
        CASE(CastExpr)
        CASE(StructDecl)
        CASE(ImplBlock)
        CASE(StringLiteral)
        CASE(FunctionSignature)
        CASE(UnaryExpr)
        CASE(MatchExpr)
        CASE2(MatchExprBranch, MatchExpr::MatchExprBranch)
        CASE(CallExpr)
        CASE(MemberExpr)
        CASE(StaticDeclRefExpr)
        CASE(WhileStmt)
        CASE(SubscriptExpr)
        CASE(ExprStmt)
        CASE(TypealiasDecl)
        CASE(BinOp)
        CASE(RawLLVMValueExpr)
        default:
            std::cout << "[Reflect] Unhandled Node: " << util::typeinfo::getTypename(*node) << std::endl;
            LKFatalError("");
    }
#undef CASE2
#undef CASE
}



std::string Node::description() const {
    std::string desc;
    
    desc.append(util::typeinfo::getTypename(*this)).append(" [");
    auto M = Reflect(this);
    
    if (M.empty()) {
        return desc + "]";
    }
    
    desc += "\n";
    
    for (auto it = M.begin(); it != M.end(); it++) {
        const auto& [key, value] = *it;
        util::string::append_with_indentation(desc, key + ": " + value, INDENT_SIZE);
        
        if (it + 1 != M.end()) {
            desc += ",";
        }
        desc += "\n";
    }
    desc += "]";
    
    return desc;
}






