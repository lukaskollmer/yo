//
//  ASTRewriter.h
//  yo
//
//  Created by Lukas Kollmer on 2019-04-20.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "parse/AST.h"


#include <memory>
#include <map>
#include <string>
#include <optional>
#include <initializer_list>



namespace yo::irgen {


/// Basically just a bunch of functions for producing a copy of a part of the AST, with the option to substitute some type descs
class ASTRewriter {
public:
    using TmplParamMapping = std::map<std::string, std::shared_ptr<ast::TypeDesc>>;
    //NominalTypeMappingT
    const TmplParamMapping templateArgumentMapping; // TODO rename
    
    
    ASTRewriter() {}
    explicit ASTRewriter(const TmplParamMapping &M) : templateArgumentMapping(M) {}
    
#define DEF_FN(T) std::shared_ptr<ast::T> handle##T(std::shared_ptr<ast::T>);
    DEF_FN(TopLevelStmt)
    DEF_FN(LocalStmt)
    DEF_FN(Expr)
    
    DEF_FN(TypeDesc)
    DEF_FN(FunctionDecl)
    DEF_FN(StructDecl)
    
    DEF_FN(VarDecl)
    DEF_FN(Assignment)
    DEF_FN(ReturnStmt)
    DEF_FN(WhileStmt)
    DEF_FN(IfStmt)
    DEF_FN(ForLoop)
    DEF_FN(CompoundStmt)
    DEF_FN(ExprStmt)
    
    DEF_FN(MatchExpr)
    DEF_FN(CallExpr)
    DEF_FN(SubscriptExpr)
    DEF_FN(MemberExpr)
    DEF_FN(BinOp)
    DEF_FN(Ident)
    DEF_FN(UnaryExpr)
    DEF_FN(LambdaExpr)
    DEF_FN(StaticDeclRefExpr)
    DEF_FN(TemplateParamDeclList)
    DEF_FN(TemplateParamArgList)
    
    ast::FunctionSignature handleFunctionSignature(const ast::FunctionSignature&);
#undef DEF_FN
};

}
