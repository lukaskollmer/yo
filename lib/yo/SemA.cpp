//
//  SemA.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-03-06.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "SemA.h"
#include "ASTVisitor.h"
#include "util_llvm.h"

#include <string>

using namespace yo;
using namespace yo::sema;
using NK = ast::Node::Kind;


std::string node_summary(std::shared_ptr<ast::Node> node) {
    switch (node->getKind()) {
        case NK::FunctionDecl: {
            auto FD = llvm::cast<ast::FunctionDecl>(node);
            return util::fmt::format("FunctionDecl: {}", FD->getName());
        }
        
        case NK::StructDecl:{
            auto SD = llvm::cast<ast::StructDecl>(node);
            std::ostringstream OS;
            OS << SD->name;
            if (SD->isTemplateDecl()) {
                std::ostringstream OS;
                OS << "<";
                util::vector::iterl(SD->templateParamsDecl->getParams(), [&OS](auto &P, bool isLast) {
                    OS << P.name->value;
                    if (!isLast) {
                        OS << ", ";
                    }
                });
                OS << ">";
            }
            return util::fmt::format("StructDecl: {}", OS.str());
        }
        
        case NK::TypealiasDecl: {
            auto TD = llvm::cast<ast::TypealiasDecl>(node);
            return util::fmt::format("Typealias {} = {}", TD->typename_, TD->type);
        }
        
        default:
            LKFatalError("wtf");
    }
}


void dbg_ast(const ast::AST &ast) {
    for (auto &node : ast) {
        util::fmt::print("{}", node_summary(node));
    }
}




// whether ast nodes depend on each other
// (eg, a function F depends on a type declaration T if A's signature contains T)


bool isDependentOn(const std::shared_ptr<ast::TopLevelStmt> &lhs, const std::shared_ptr<ast::TopLevelStmt> &rhs) {
    auto lhsSum = node_summary(lhs);
    auto rhsSum = node_summary(rhs);
    util::string::pad_right(lhsSum, 35, ' ');
    util::string::pad_right(rhsSum, 35, ' ');
    util::fmt::print("{}   <--->   {}", lhsSum, rhsSum);
    if (lhs->isOfKind(NK::FunctionDecl) && rhs->isOfKind(NK::StructDecl)) {
        // Function dependent on struct type?
        auto FD = llvm::cast<ast::FunctionDecl>(lhs);
        auto SD = llvm::cast<ast::StructDecl>(rhs);
        
        if (auto typeDesc = FD->implTypeDesc; typeDesc && typeDesc->isNominal() && typeDesc->getName() == SD->name) {
            // member function depends the type it is a member of
            return true;
        }
        
        return ast::check_sig_ty_dep(FD->getSignature(), SD);
    }
    
    if (lhs->isOfKind(NK::FunctionDecl) && rhs->isOfKind(NK::TypealiasDecl)) {
        auto FD = llvm::cast<ast::FunctionDecl>(lhs);
        auto TD = llvm::cast<ast::TypealiasDecl>(rhs);
        return ast::check_sig_tyname_dep(FD->getSignature(), TD->typename_);
    }
    
    
    return false;
    LKFatalError("TODO");
}




bool sema::resolveTopLevelDependencies(ast::AST &ast) {
    util::fmt::print("Input AST:");
    dbg_ast(ast);
    LKFatalError("TODO");
}


void sema::run(const ast::AST&) {}




//FunctionDecl <line:1:1, line:3:1> line:1:5 square 'int (int)'
//|-ParmVarDecl <col:12, col:16> col:16 used n 'int'
//`-CompoundStmt <col:19, line:3:1>
//  `-ReturnStmt <line:2:5, col:16>
//    `-BinaryOperator <col:12, col:16> 'int' '*'
//      |-ImplicitCastExpr <col:12> 'int' <LValueToRValue>
//      | `-DeclRefExpr <col:12> 'int' lvalue ParmVar 0x55d6838aadf8 'n' 'int'
//      `-ImplicitCastExpr <col:16> 'int' <LValueToRValue>
//        `-DeclRefExpr <col:16> 'int' lvalue ParmVar 0x55d6838aadf8 'n' 'int'
