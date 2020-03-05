//
//  ASTVisitor.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-03-05.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "ASTVisitor.h"
#include "util_llvm.h"


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
    
    
    bool traverseFunctionDecl(std::shared_ptr<ast::FunctionDecl> FD) {
        TRY(visitFunctionDecl(FD));
        TRY(traverseFunctionSignature(FD->getSignature()));
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
    
    
    bool visitFunctionDecl(std::shared_ptr<ast::FunctionDecl>) {
        return true;
    }
    bool visitFunctionSignature(FunctionSignature&) {
        return true;
    }
    bool visitFunctionBody(std::shared_ptr<CompoundStmt>) {
        return true;
    }
    bool visitExpr(std::shared_ptr<Expr>) {
        return true;
    }
    bool visitTypeDesc(std::shared_ptr<TypeDesc>) {
        return true;
    }
    
};





class Visitor1 : public ASTVisitor<Visitor1> {
    std::shared_ptr<StructDecl> decl;

public:
    explicit Visitor1(std::shared_ptr<StructDecl> decl) : decl(decl) {}
    
    bool visitTypeDesc(std::shared_ptr<TypeDesc> typeDesc) {
        if (decl->isTemplateDecl()) {
            if (typeDesc->isNominalTemplated() && typeDesc->getName() == decl->name) {
                return false;
            }
        } else {
            if (typeDesc->isNominal() && typeDesc->getName() == decl->name) {
                return false;
            }
        }
        return true;
    }
};


bool check_sig_ty_dep(FunctionSignature &signature, std::shared_ptr<StructDecl> decl) {
    return !Visitor1(decl).traverseFunctionSignature(signature);
    
}




} // namespace ast
} // namespace yo
