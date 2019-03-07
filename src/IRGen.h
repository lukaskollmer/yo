//
//  IRGen.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//
#if 0
#pragma once

#include <string>
#include <map>
#include <memory>
#include <utility>
#include <functional>


#include "ast/AST.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

#include "util.h"
#include "Type.h"


NS_START(irgen)


struct ValueBinding {
    using ReadTy  = std::function<llvm::Value*(void)>;
    using WriteTy = std::function<void(llvm::Value*)>;
    
    const ReadTy  Read;
    const WriteTy Write;
    
    ValueBinding(ReadTy Read, WriteTy Write) : Read(Read), Write(Write) {}
};



class Scope {
private:
    using Entry = std::tuple<ast::IdentifierExprAST*, llvm::Type*, std::shared_ptr<ValueBinding>>;
    using V = std::vector<Entry>;
    V Symbols;
    
public:
    using Marker = uint64_t;
    
    void Insert(ast::IdentifierExprAST *Ident, llvm::Type *Type, ValueBinding Binding);
    void Insert(ast::IdentifierExprAST *Ident, ValueBinding Binding);
    
    std::pair<llvm::Type*, ValueBinding> Remove(ast::IdentifierExprAST *Ident);
    
    bool Contains(ast::IdentifierExprAST *Ident);
    bool IsEmpty() { return Symbols.empty(); }
    void Clear() { Symbols.clear(); } // TODO instead make this return all symbols?
    
    llvm::Type*GetType(ast::IdentifierExprAST *Ident);
    void SetType(ast::IdentifierExprAST *Ident, llvm::Type*Type);
    
    ValueBinding *GetBinding(ast::IdentifierExprAST *Ident);
    
    Scope::Entry *_GetEntry(ast::IdentifierExprAST *Ident, V::const_iterator *Pos = nullptr);
    
    // Iterate the Scope in reverse insertion order
    void Iterate(std::function<void(ast::IdentifierExprAST*, llvm::Type*, ValueBinding*, bool*)> Fn);
    
    
    // Markers, etc
    Marker GetMarker();
    V GetEntriesSinceMarker(Marker M);
    
    
    // Debugging
    void Dump();
};





class TypeCache {
public:
    struct StructDescriptor {
        struct Attribute {
            std::string Name;
            TypeInfo *Type;
            
            Attribute(std::string Name, TypeInfo *TI) : Name(Name), Type(TI) {}
        };
        
        std::string Name;
        std::vector<Attribute> Attributes;
        llvm::Type *LLVMType;
        
        StructDescriptor(std::string Name, std::vector<Attribute> Attributes, llvm::Type *LLVMType) : Name(Name), Attributes(Attributes), LLVMType(LLVMType) {}
    };
    
    TypeCache() {}
    
    void Register(ast::StructDeclAST *StructDecl, llvm::Type *LLVMType);
    StructDescriptor *Get(std::string Name);
    
    
private:
    std::map<std::string, StructDescriptor> Structs;
};






class IRGenerator {
private:
    llvm::LLVMContext &C;
    std::unique_ptr<llvm::Module> Module;
    llvm::Module *M;
    llvm::IRBuilder<> Builder;
    
    Scope Scope;
    TypeCache TypeCache;
    
    llvm::Type *i8, *i16, *i32, *i64;
    llvm::Type *i8_ptr;
    llvm::Type *Void, *Bool, *Double;

public:
    IRGenerator(llvm::LLVMContext &C, const std::string &ModuleName);

    std::unique_ptr<llvm::Module> GetModule() {
        return std::move(Module);
    }

    void Codegen(ast::AST &Ast);
    
private:
    void Preflight(ast::AST &Ast);
    void RegisterFunctionSignature(ast::FunctionSignature &FunctionSignature, bool MangleName = true);
    void RegisterStruct(ast::StructDeclAST *StructDecl);
    
    llvm::Value *Codegen(ast::Node *Node);
    
    llvm::Value *Codegen(ast::NumberExprAST *Number);
    llvm::Value *Codegen(ast::FunctionCallAST *Call);
    
    llvm::Value *Codegen(ast::FunctionDeclAST *FunctionDecl, bool MangleName = true);
    llvm::Value *Codegen(ast::Composite *Composite);
    llvm::Value *Codegen(ast::VariableDeclAST *VariableDecl);
    llvm::Value *Codegen(ast::ReturnStmtAST *ReturnStmt);
    llvm::Value *Codegen(ast::BinaryOperationExprAST *BinaryOperation);
    llvm::Value *Codegen(ast::IdentifierExprAST *Identifier);
    llvm::Value *Codegen(ast::Assignment *Assignment);
    llvm::Value *Codegen(ast::IfStmt *If);
    
    llvm::Value *Codegen(ast::Comparison *Comparison);
    
    // Types
    llvm::Type *GetLLVMType(TypeInfo *TI);
    uint64_t Sizeof(llvm::Type *Ty);
    uint64_t Sizeof(TypeInfo *TI) { return Sizeof(GetLLVMType(TI)); }
    bool IsSignedType(llvm::Type *Type);
    
    bool Binop_AttemptToResolvePotentialIntTypeMismatchesByCastingNumberLiteralsIfPossible(llvm::Value **LHS, llvm::Value **RHS);
    
    
    // Return value indicates whether the types (after casting, if necessary) are compatible
    // If V is an int literal and DestTy is an integer type of a different size than V, V gets rewritten to a cast to DestTy
    bool TypecheckAndApplyTrivialIntCastIfPossible(llvm::Value **V, llvm::Type *DestTy);
    
    
    
    // Code synthesis
    // guaranteed to return llvm::Function*
    llvm::Value *GenerateInitializerForStruct(ast::StructDeclAST *);
};
    

NS_END
#endif
