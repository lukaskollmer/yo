//
//  ASTVisitor.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-03-05.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "ASTVisitor.h"
#include "util/llvm_casting.h"


namespace yo {
namespace ast {


#define LOGF() util::fmt::print(__PRETTY_FUNCTION__)
#define TRY(expr) do { if (!getDerived().expr) return false; } while (0)


template <typename Derived>
class ASTVisitor {
    using NK = Node::Kind;
    
    
    Derived& getDerived() {
        return *static_cast<Derived *>(this);
    }

    
public:
    // returns false if the traversal was terminated early
    bool traverse(const AST &ast) {
        for (const auto &node : ast) {
            switch (node->getKind()) {
                case NK::FunctionDecl:
                    TRY(traverseFunctionDecl(llvm::cast<FunctionDecl>(node)));
                    break;
                
                default:
                    LKFatalError("TODO");
            }
        }
        
        return true;
    }
    
    
    bool traverse(std::shared_ptr<ast::Node> node) {
        if (auto TLS = std::dynamic_pointer_cast<TopLevelStmt>(node)) {
            return traverseTopLevelStmt(TLS);
        } else if (auto localStmt = std::dynamic_pointer_cast<LocalStmt>(node)) {
            LKFatalError("TODO");
            //return traverseLocalStmt(localStmt);
        } else if (auto expr = std::dynamic_pointer_cast<Expr>(node)) {
            return traverseExpr(expr);
        } else {
            LKFatalError("unexpected node");
        }
    }
    
    
    // MARK: TopLevelStmt traversal
    
    bool traverseTopLevelStmt(std::shared_ptr<TopLevelStmt> TLS) {
        switch (TLS->getKind()) {
            case NK::TypealiasDecl:
                return traverseTypealiasDecl(llvm::cast<TypealiasDecl>(TLS));
            case NK::FunctionDecl:
                return traverseFunctionDecl(llvm::cast<FunctionDecl>(TLS));
            case NK::StructDecl:
                return traverseStructDecl(llvm::cast<StructDecl>(TLS));
            case NK::VariantDecl:
                return traverseVariantDecl(llvm::cast<VariantDecl>(TLS));
            default:
                LKFatalError("unhandled node");
                
        }
    }
    
    
    bool traverseTypealiasDecl(std::shared_ptr<TypealiasDecl> TD) {
        TRY(visitTypealiasDecl(TD));
        TRY(traverseTypeDesc(TD->type));
        return true;
    }
    
    bool traverseFunctionDecl(std::shared_ptr<ast::FunctionDecl> FD) {
        TRY(visitFunctionDecl(FD));
        TRY(traverseFunctionSignature(FD->getSignature()));
        return true;
    }
    
    bool traverseStructDecl(std::shared_ptr<StructDecl> decl) {
        TRY(visitStructDecl(decl));
        // TODO
        return true;
    }
    
    bool traverseVariantDecl(std::shared_ptr<VariantDecl> decl) {
        TRY(visitVariantDecl(decl));
        // TODO
        return true;
    }
    
    
    bool traverseFunctionSignature(FunctionSignature &signature) {
        TRY(visitFunctionSignature(signature));
        for (auto typeDesc : signature.paramTypes) {
            TRY(traverseTypeDesc(typeDesc));
        }
        TRY(traverseTypeDesc(signature.returnType));
        return true;
    }
    
    
    bool traverseTypeDesc(std::shared_ptr<TypeDesc> typeDesc) {
        TRY(visitTypeDesc(typeDesc));
        switch (typeDesc->getKind()) {
            case TypeDesc::Kind::Nominal:
                break;
            
            
            case TypeDesc::Kind::NominalTemplated:
                for (auto arg : typeDesc->getTemplateArgs()) {
                    TRY(traverseTypeDesc(arg));
                }
                break;
            
            case TypeDesc::Kind::Pointer:
                TRY(traverseTypeDesc(typeDesc->getPointee()));
                break;
            
            case TypeDesc::Kind::Function:
                for (auto arg : typeDesc->getFunctionTypeInfo().parameterTypes) {
                    TRY(traverseTypeDesc(arg));
                }
                TRY(traverseTypeDesc(typeDesc->getFunctionTypeInfo().returnType));
                break;
            
            case TypeDesc::Kind::Reference:
                TRY(traverseTypeDesc(typeDesc->getPointee()));
                break;
            
            case TypeDesc::Kind::Decltype:
                TRY(traverseExpr(typeDesc->getDecltypeExpr()));
                break;
            
            case TypeDesc::Kind::Tuple:
                for (auto ty : typeDesc->getTupleMembers()) {
                    TRY(traverseTypeDesc(ty));
                }
                break;
            
            case TypeDesc::Kind::Resolved:
                LKFatalError("TODO"); // how should this be handled?
                break;
        }
        return true;
    }
    
    
    bool traverseExpr(std::shared_ptr<Expr> expr) {
        TRY(visitExpr(expr));
        // TODO
        return true;
    }
    
    
    
    
#define DEF_VISIT_FN(T) \
bool visit##T(std::shared_ptr<T>) { return true; }
    
#define DEF_VISIT_FN_N(N, T) \
bool visit##N(std::shared_ptr<T>) { return true; }

#define DEF_VISIT_FN_T(N, T) \
bool visit##N(T) { return true; }
    
    DEF_VISIT_FN(Node)
    DEF_VISIT_FN(TypeDesc)
    
    DEF_VISIT_FN(TopLevelStmt)
    DEF_VISIT_FN(LocalStmt)
    DEF_VISIT_FN(Expr)
    
    DEF_VISIT_FN(FunctionDecl)
    DEF_VISIT_FN_N(FunctionBody, CompoundStmt)
    
    DEF_VISIT_FN(StructDecl)
    DEF_VISIT_FN(TypealiasDecl)
    DEF_VISIT_FN(VariantDecl)
    
    DEF_VISIT_FN_T(FunctionSignature, FunctionSignature&)
    
    
};






class ASTPrinter : public ASTVisitor<ASTPrinter> {
public:
    // TODO
};



void print_ast(const ast::AST &ast) {
    ASTPrinter().traverse(ast);
    LKFatalError("");
}




} // namespace ast
} // namespace yo
