//
//  IRGen.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "IRGen.h"

#include "Mangling.h"

using namespace irgen;
using namespace llvm;



std::string MangleFunctionName(std::string Name) {
    return Name == "main" ? Name : mangling::MangleFunction(Name);
}




LLVMContext IRGenerator::C;


IRGenerator::IRGenerator(const std::string ModuleName) : Builder(C) {
    Module = llvm::make_unique<llvm::Module>(ModuleName, C);
    M = Module.get();
    
    
    i8  = llvm::Type::getInt8Ty(C);
    i16 = llvm::Type::getInt16Ty(C);
    i32 = llvm::Type::getInt32Ty(C);
    i64 = llvm::Type::getInt64Ty(C);
    
    Void = llvm::Type::getVoidTy(C);
    Bool = llvm::Type::getInt1Ty(C);
    Double = llvm::Type::getDoubleTy(C);
}




void IRGenerator::Codegen(ast::AST &Ast) {
    Preflight(Ast);
    
    for (auto &Node : Ast) {
        Codegen(Node);
    }
}



void IRGenerator::Preflight(ast::AST &Ast) {
    for (auto &Node : Ast) {
        if (auto FunctionDecl = std::dynamic_pointer_cast<ast::FunctionDecl>(Node)) {
            RegisterFunctionSignature(FunctionDecl);
        } else if (auto ExternFunctionDecl = std::dynamic_pointer_cast<ast::ExternFunctionDecl>(Node)) {
            RegisterFunctionSignature(ExternFunctionDecl, false);
        }
    }
}


void IRGenerator::RegisterFunctionSignature(std::shared_ptr<ast::FunctionSignature> Signature, bool MangleName) {
    std::vector<llvm::Type *> ParameterTypes;
    
    for (auto &P : Signature->Parameters) {
        ParameterTypes.push_back(GetLLVMType(P->Type));
    }
    
    auto Name = MangleName ? MangleFunctionName(Signature->Name) : Signature->Name;
    
    auto FT = llvm::FunctionType::get(GetLLVMType(Signature->ReturnType), ParameterTypes, false);
    auto F = llvm::Function::Create(FT, llvm::Function::LinkageTypes::ExternalLinkage, Name, M);
    
    unsigned Idx = 0;
    for (auto &Arg : F->args()) {
        Arg.setName(Signature->Parameters[Idx++]->Name->Value);
    }
}



# pragma mark - Codegen


#define HANDLE(node, T) \
if (auto X = std::dynamic_pointer_cast<ast::T>(node)) return Codegen(X);

#define IGNORE(node, T) \
if (std::dynamic_pointer_cast<ast::T>(node)) return nullptr;

#define unhandled_node(node) \
{ std::cout << "[IRGenerator::Codegen] Unhandled Node: " << util::typeinfo::GetTypename(*(node)) << std::endl; \
throw; }

llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Node> Node) {
    HANDLE(Node, Expr)
    HANDLE(Node, LocalStmt)
    HANDLE(Node, TopLevelStmt)
    
    unhandled_node(Node)
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::TopLevelStmt> TLS) {
    HANDLE(TLS, FunctionDecl)
    IGNORE(TLS, ExternFunctionDecl)
    
    unhandled_node(TLS)
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::LocalStmt> LocalStmt) {
    HANDLE(LocalStmt, Composite)
    HANDLE(LocalStmt, FunctionCall)
    
    unhandled_node(LocalStmt);
}

llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Expr> Expr) {
    HANDLE(Expr, NumberLiteral)
    
    unhandled_node(Expr)
}

#undef HANDLE
#undef IGNORE
#undef unhandled_node


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::FunctionDecl> FunctionDecl, bool MangleName) {
    assert(Scope.IsEmpty());
    
    std::string Name = FunctionDecl->Name;
    if (MangleName) Name = MangleFunctionName(Name);
    
    auto F = M->getFunction(Name);
    if (!F) {
        LKFatalError("Unable to find function '%s'", Name.c_str());
    }
    
    for (auto &Arg : F->args()) {
        auto Binding = ValueBinding([&]() {
            return &Arg;
        }, [&](llvm::Value *Value) {
            LKFatalError("Function arguments are read-only (%s in %s)", Arg.getName().str().c_str(), Name.c_str());
        });
        
        Scope.Insert(Arg.getName(), Arg.getType(), std::move(Binding));
    }
    
    auto BB = llvm::BasicBlock::Create(C, "entry", F);
    Builder.SetInsertPoint(BB);
    
    Codegen(FunctionDecl->Body);
    return F;
}



llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Composite> Composite) {
    auto M = Scope.GetMarker();
    
    for (auto &Stmt : Composite->Statements) {
        if (auto ReturnStmt = std::dynamic_pointer_cast<ast::ReturnStmt>(Stmt)) {
            // TODO special handling?
            Codegen(ReturnStmt);
            Scope.Clear();
        } else {
            Codegen(Stmt);
        }
    }
    
    auto Entries = Scope.GetEntriesSinceMarker(M);
    for (auto &E : Entries) {
        Scope.Remove(std::get<0>(E));
    }
    
    // if statements are implemented as phi nodes, which means that we need every branch to return _something_
    return llvm::ConstantInt::get(i8, 0);
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::ReturnStmt> ReturnStmt) {
    return Builder.CreateRet(Codegen(ReturnStmt->Expression));
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::NumberLiteral> Number) {
    return ConstantInt::get(i64, Number->Value);
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::FunctionCall> Call) {
    llvm::Function *F;
    
    if (auto Ident = std::dynamic_pointer_cast<ast::Identifier>(Call->Target)) {
        F = M->getFunction(MangleFunctionName(Ident->Value));
        if (!F) {
            F = M->getFunction(Ident->Value);
        }
        if (!F) {
            LKFatalError("Unable to find function named '%s'", Ident->Value.c_str());
        }
    } else {
        throw;
    }
    
    auto FT = F->getFunctionType();
    
    std::vector<llvm::Value *> Args;
    
    for (auto I = 0; I < Call->Arguments.size(); I++) {
        auto ExpectedType = FT->getParamType(I);
        auto V = Codegen(Call->Arguments[I]);
        if (!TypecheckAndApplyTrivialCastIfPossible(&V, ExpectedType)) {
            outs() << "Type mismatch: Cannot pass expression of type " << V->getType() << " to function '" << F->getName() << "' expecting " << ExpectedType << "\n";
            throw;
        }
        Args.push_back(V);
    }
    
    return Builder.CreateCall(F, Args);
}


#pragma mark - Types


llvm::Type *IRGenerator::GetLLVMType(TypeInfo *TI) {
#define HANDLE(name) if (TI->Equals(TypeInfo::name)) { return name; }
    
    switch (TI->Kind) {
        case TypeInfo::Kind::Pointer:
            HANDLE(i8_ptr)
            break;
        case TypeInfo::Kind::Primitive: {
            HANDLE(i8)
            HANDLE(i16)
            HANDLE(i32)
            HANDLE(i64)
            HANDLE(Bool)
            HANDLE(Double)
            HANDLE(Void)
            break;
        }
        default: break;
    }
    
    if (TI->Kind == TypeInfo::Kind::Complex) {
        //TypeCache.Get(TI->Data.Name)->LLVMType->print(outs());
        //return TypeCache.Get(TI->Data.Name)->LLVMType;
    }
    
    throw;
#undef HANDLE
}




bool IRGenerator::TypecheckAndApplyTrivialCastIfPossible(llvm::Value **V, llvm::Type *DestType) {
    auto SrcTy = (*V)->getType();
    
    if (SrcTy == DestType) return true;
    
    if (DestType->isIntegerTy() && (*V)->getValueID() == llvm::Value::ValueTy::ConstantIntVal) {
        *V = Builder.CreateIntCast(*V, DestType, true); // TODO add support for signed/unsigned types
        return true;
    }
    return false;
}
