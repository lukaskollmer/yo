//
//  ASTRewriter.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-04-20.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "ASTRewriter.h"
#include "Mangling.h"
#include "parse/Attributes.h"
#include "util/MapUtils.h"
#include "util/VectorUtils.h"
#include "util/llvm_casting.h"

#include "llvm/Support/Casting.h"

#include <memory>



using namespace yo;
using namespace irgen;

using NK = ast::Node::Kind;





#define CASE(T) case NK::T: return handle##T(llvm::cast<ast::T>(node));

#define unhandled_node(node) \
LKFatalError("Unhandled Node: %s", ast::nodeKindToString(node->getKind()).c_str());


std::shared_ptr<ast::LocalStmt> ASTRewriter::handleLocalStmt(std::shared_ptr<ast::LocalStmt> node) {
    switch (node->getKind()) {
        CASE(ReturnStmt)
        CASE(Assignment)
        CASE(VarDecl)
        CASE(WhileStmt)
        CASE(IfStmt)
        CASE(ExprStmt)
        CASE(ForLoop)
        default:
            unhandled_node(node)
    }
}

std::shared_ptr<ast::Expr> ASTRewriter::handleExpr(std::shared_ptr<ast::Expr> node) {
    if (!node) {
        return nullptr;
    }
    
    switch (node->getKind()) {
        // ignored nodes
        case NK::NumberLiteral:
        case NK::StringLiteral:
            return node;
        
        CASE(MatchExpr)
        CASE(CallExpr)
        CASE(SubscriptExpr)
        CASE(MemberExpr)
        CASE(BinOp)
        CASE(Ident)
        CASE(UnaryExpr)
        CASE(LambdaExpr)
        CASE(StaticDeclRefExpr)
        
        default:
            unhandled_node(node)
    }
}

#undef CASE
#undef unhandled_node







std::shared_ptr<ast::TypeDesc> ASTRewriter::handleTypeDesc(std::shared_ptr<ast::TypeDesc> typeDesc) {
    using TDK = ast::TypeDesc::Kind;
    
    if (!typeDesc) {
        return nullptr;
    }
    
    const auto &loc = typeDesc->getSourceLocation();
    
    switch (typeDesc->getKind()) {
        case TDK::Resolved:
            return ast::TypeDesc::makeResolved(typeDesc->getResolvedType(), loc);
        
        case TDK::Pointer:
            return ast::TypeDesc::makePointer(handleTypeDesc(typeDesc->getPointee()), loc);
        
        case TDK::Reference:
            return ast::TypeDesc::makeReference(handleTypeDesc(typeDesc->getPointee()), loc);
        
        case TDK::Nominal: {
            if (auto ty = util::map::get_opt(templateArgumentMapping, typeDesc->getName())) {
                return std::make_shared<ast::TypeDesc>(**ty);
            }
            return std::make_shared<ast::TypeDesc>(*typeDesc);
        }
        
        case TDK::Decltype:
            return ast::TypeDesc::makeDecltype(handleExpr(typeDesc->getDecltypeExpr()), loc);
        
        case TDK::NominalTemplated: {
            auto resolvedTemplateArgs = util::vector::map(typeDesc->getTemplateArgs(), [this](auto &ty) {
                return handleTypeDesc(ty);
            });
            return ast::TypeDesc::makeNominalTemplated(typeDesc->getName(), resolvedTemplateArgs, loc);
        }
        
        
        case TDK::Tuple: {
            auto resolvedTypes = util::vector::map(typeDesc->getTupleMembers(), [this](auto &type) {
                return handleTypeDesc(type);
            });
            return ast::TypeDesc::makeTuple(resolvedTypes, loc);
        }
        
        case TDK::Function:
            LKFatalError("TODO");
        
        
    }
}


ast::FunctionSignature ASTRewriter::handleFunctionSignature(const ast::FunctionSignature &signature) {
    ast::FunctionSignature specSig;
    
    specSig.setSourceLocation(signature.getSourceLocation());
    specSig.paramTypes.reserve(signature.numberOfParameters());
    for (auto &paramTypeDesc : signature.paramTypes) {
        specSig.paramTypes.push_back(handleTypeDesc(paramTypeDesc));
    }
    specSig.returnType = handleTypeDesc(signature.returnType);
    
    if (signature.numberOfTemplateParameters() > 0) {
        specSig.templateParamsDecl = std::make_shared<ast::TemplateParamDeclList>();
        specSig.templateParamsDecl->setSourceLocation(signature.templateParamsDecl->getSourceLocation());
        
        for (const auto &param : signature.templateParamsDecl->getParams()) {
            if (auto argTy = util::map::get_opt(templateArgumentMapping, param.name->value)) {
                specSig.templateInstantiationArguments.push_back((*argTy)->getResolvedType());
            } else {
                specSig.templateParamsDecl->addParam(ast::TemplateParamDeclList::Param(param.name, handleTypeDesc(param.defaultType)));
            }
        }
    }
    
    if (specSig.templateParamsDecl && specSig.templateParamsDecl->size() == 0) {
        specSig.templateParamsDecl = nullptr;
    }
    
    return specSig;
}


std::shared_ptr<ast::FunctionDecl> ASTRewriter::handleFunctionDecl(std::shared_ptr<ast::FunctionDecl> decl) {
    auto specializedFuncDecl = std::make_shared<ast::FunctionDecl>(decl->getFunctionKind(),
                                                                   decl->getName(),
                                                                   handleFunctionSignature(decl->getSignature()),
                                                                   decl->getAttributes());
    specializedFuncDecl->setSourceLocation(decl->getSourceLocation());
    specializedFuncDecl->setParamNames(decl->getParamNames()); // TODO make copies here!
    
    if (decl->getAttributes().intrinsic) {
        LKAssert(decl->getBody()->isEmpty());
        return specializedFuncDecl;
    }
    
    specializedFuncDecl->setBody(handleCompoundStmt(decl->getBody()));
    return specializedFuncDecl;
}





std::shared_ptr<ast::StructDecl> ASTRewriter::handleStructDecl(std::shared_ptr<ast::StructDecl> decl) {
    auto spec = std::make_shared<ast::StructDecl>(*decl);
    spec->members = util::vector::map(decl->members, [this](const auto &member) {
        return handleVarDecl(member);
    });
    return spec;
}


std::shared_ptr<ast::VariantDecl> ASTRewriter::handleVariantDecl(std::shared_ptr<ast::VariantDecl> decl) {
    auto spec = std::make_shared<ast::VariantDecl>(*decl);
    spec->name = handleIdent(decl->name);
    spec->members = util::vector::map(decl->members, [this](const ast::VariantDecl::MemberDecl &member) {
        ast::VariantDecl::MemberDecl spec(member);
        spec.name = handleIdent(member.name);
        spec.params = handleTypeDesc(member.params);
        return spec;
    });
    return spec;
}



#pragma mark - Local Statements


std::shared_ptr<ast::CompoundStmt> ASTRewriter::handleCompoundStmt(std::shared_ptr<ast::CompoundStmt> stmt) {
    auto spec = std::make_shared<ast::CompoundStmt>(*stmt);
    spec->statements = util::vector::map(stmt->statements, [&](const auto &stmt) {
        return handleLocalStmt(stmt);
    });
    return spec;
}


std::shared_ptr<ast::VarDecl> ASTRewriter::handleVarDecl(std::shared_ptr<ast::VarDecl> decl) {
    auto spec = std::make_shared<ast::VarDecl>(*decl);
    spec->type = handleTypeDesc(decl->type);
    spec->initialValue = handleExpr(decl->initialValue);
    return spec;
}


std::shared_ptr<ast::ReturnStmt> ASTRewriter::handleReturnStmt(std::shared_ptr<ast::ReturnStmt> stmt) {
    auto spec = std::make_shared<ast::ReturnStmt>(*stmt);
    spec->expr = handleExpr(stmt->expr);
    return spec;
}

std::shared_ptr<ast::Assignment> ASTRewriter::handleAssignment(std::shared_ptr<ast::Assignment> stmt) {
    auto spec = std::make_shared<ast::Assignment>(*stmt);
    spec->target = handleExpr(stmt->target);
    spec->value = handleExpr(stmt->value);
    return spec;
}



std::shared_ptr<ast::WhileStmt> ASTRewriter::handleWhileStmt(std::shared_ptr<ast::WhileStmt> stmt) {
    auto spec = std::make_shared<ast::WhileStmt>(*stmt);
    spec->condition = handleExpr(stmt->condition);
    spec->body = handleCompoundStmt(stmt->body);
    return spec;
}

std::shared_ptr<ast::IfStmt> ASTRewriter::handleIfStmt(std::shared_ptr<ast::IfStmt> stmt) {
    auto spec = std::make_shared<ast::IfStmt>(*stmt);
    spec->branches = util::vector::map(stmt->branches, [&](const auto &branch) {
        auto spec = std::make_shared<ast::IfStmt::Branch>(*branch);
        spec->condition = handleExpr(branch->condition);
        spec->body = handleCompoundStmt(branch->body);
        return spec;
    });
    return spec;
}


std::shared_ptr<ast::ForLoop> ASTRewriter::handleForLoop(std::shared_ptr<ast::ForLoop> stmt) {
    auto spec = std::make_shared<ast::ForLoop>(*stmt);
    spec->ident = handleIdent(stmt->ident);
    spec->expr = handleExpr(stmt->expr);
    spec->body = handleCompoundStmt(stmt->body);
    return spec;
}


std::shared_ptr<ast::ExprStmt> ASTRewriter::handleExprStmt(std::shared_ptr<ast::ExprStmt> stmt) {
    auto spec = std::make_shared<ast::ExprStmt>(*stmt);
    spec->expr = handleExpr(stmt->expr);
    return spec;
}



#pragma mark - Expressions

std::shared_ptr<ast::CallExpr> ASTRewriter::handleCallExpr(std::shared_ptr<ast::CallExpr> call) {
    auto spec = std::make_shared<ast::CallExpr>(*call);
    spec->target = handleExpr(call->target);
    spec->arguments = util::vector::map(call->arguments, [this](const auto &expr) {
        return handleExpr(expr);
    });
    spec->explicitTemplateArgs = handleTemplateParamArgList(call->explicitTemplateArgs);
    return spec;
}


std::shared_ptr<ast::SubscriptExpr> ASTRewriter::handleSubscriptExpr(std::shared_ptr<ast::SubscriptExpr> expr) {
    auto spec = std::make_shared<ast::SubscriptExpr>(*expr);
    spec->target = handleExpr(expr->target);
    spec->args = util::vector::map(expr->args, [this](const auto &expr) {
        return handleExpr(expr);
    });
    return spec;
}


std::shared_ptr<ast::MemberExpr> ASTRewriter::handleMemberExpr(std::shared_ptr<ast::MemberExpr> expr) {
    auto spec = std::make_shared<ast::MemberExpr>(*expr);
    spec->target = handleExpr(expr->target);
    return spec;
}


std::shared_ptr<ast::Ident> ASTRewriter::handleIdent(std::shared_ptr<ast::Ident> ident) {
    return std::make_shared<ast::Ident>(*ident);
}


std::shared_ptr<ast::StaticDeclRefExpr> ASTRewriter::handleStaticDeclRefExpr(std::shared_ptr<ast::StaticDeclRefExpr> expr) {
    auto spec = std::make_shared<ast::StaticDeclRefExpr>(*expr);
    spec->typeDesc = handleTypeDesc(expr->typeDesc);
    return spec;
}





std::shared_ptr<ast::MatchExpr> ASTRewriter::handleMatchExpr(std::shared_ptr<ast::MatchExpr> expr) {
    auto spec = std::make_shared<ast::MatchExpr>(*expr);
    spec->target = handleExpr(expr->target);
    spec->branches = util::vector::map(expr->branches, [this](const ast::MatchExprBranch &branch) {
        ast::MatchExprBranch spec(branch);
        spec.expr = handleExpr(branch.expr);
        spec.patterns = util::vector::map(branch.patterns, [this](const ast::MatchExprPattern &pattern) {
            ast::MatchExprPattern spec(pattern);
            spec.expr = handleExpr(pattern.expr);
            spec.cond = handleExpr(pattern.cond);
            return spec;
        });
        return spec;
    });
    return spec;
}



std::shared_ptr<ast::BinOp> ASTRewriter::handleBinOp(std::shared_ptr<ast::BinOp> expr) {
    auto spec = std::make_shared<ast::BinOp>(*expr);
    spec->lhs = handleExpr(expr->lhs);
    spec->rhs = handleExpr(expr->rhs);
    return spec;
}


std::shared_ptr<ast::UnaryExpr> ASTRewriter::handleUnaryExpr(std::shared_ptr<ast::UnaryExpr> expr) {
    auto spec = std::make_shared<ast::UnaryExpr>(*expr);
    spec->expr = handleExpr(expr->expr);
    return spec;
}



std::shared_ptr<ast::LambdaExpr> ASTRewriter::handleLambdaExpr(std::shared_ptr<ast::LambdaExpr> lambdaExpr) {
    LKFatalError("TODO: implement!");
}





std::shared_ptr<ast::TemplateParamDeclList> ASTRewriter::handleTemplateParamDeclList(std::shared_ptr<ast::TemplateParamDeclList> decl) {
    if (!decl) {
        return nullptr;
    }
    
    auto spec = std::make_shared<ast::TemplateParamDeclList>(*decl);
    spec->setParams(util::vector::map(decl->getParams(), [this](const ast::TemplateParamDeclList::Param &param) {
        ast::TemplateParamDeclList::Param spec(param);
        spec.name = handleIdent(param.name);
        spec.defaultType = handleTypeDesc(param.defaultType);
        return spec;
    }));
    return spec;
}



std::shared_ptr<ast::TemplateParamArgList> ASTRewriter::handleTemplateParamArgList(std::shared_ptr<ast::TemplateParamArgList> tmplArgs) {
    if (!tmplArgs) {
        return nullptr;
    }
    
    auto spec = std::make_shared<ast::TemplateParamArgList>(*tmplArgs);
    spec->elements = util::vector::map(tmplArgs->elements, [this](const auto &typeDesc) {
        return handleTypeDesc(typeDesc);
    });
    return spec;
}
