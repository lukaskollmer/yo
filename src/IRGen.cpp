//
//  IRGen.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//
#if 0
#include "IRGen.h"

#include <iostream>

#include "llvm/Support/ErrorHandling.h"
#include "Mangling.h"


using namespace llvm;
using namespace irgen;
using namespace ast;



#define NOT_YET_IMPLEMENTED \
std::cout << "[" << __PRETTY_FUNCTION__ << "]: Not yet implemented\n"; \
exit(EXIT_FAILURE);



//
// SCOPE
//

void Scope::Insert(ast::IdentifierExprAST *Ident, llvm::Type *Type, ValueBinding Binding) {
    Symbols.push_back(std::make_tuple(Ident, Type, std::make_shared<ValueBinding>(std::move(Binding))));
}

void Scope::Insert(ast::IdentifierExprAST *Ident, ValueBinding Binding) {
    Insert(Ident, nullptr, Binding);
}


std::pair<llvm::Type*, ValueBinding> Scope::Remove(ast::IdentifierExprAST *Ident) {
    V::const_iterator Pos;
    auto E = _GetEntry(Ident, &Pos);
    std::pair<llvm::Type *, ValueBinding> Retval(std::get<1>(*E), *std::get<2>(*E));
    Symbols.erase(Pos);
    return Retval;
}



Scope::Entry *Scope::_GetEntry(ast::IdentifierExprAST *Ident, V::const_iterator *Pos) {
    for (V::iterator It = Symbols.end(); It-- != Symbols.begin();) {
        if (std::get<0>(*It)->getValue() == Ident->getValue()) {
            if (Pos) *Pos = It;
            return &*It;
        }
    }
    
    //for (V::reverse_iterator It = Symbols.rbegin(); It != Symbols.rend(); It++) {
    //    if (std::get<0>(*It)->getValue() == Ident->getValue()) {
    //        if (Pos) *Pos = std::
    //        return &*It;
    //    }
    //}
    return nullptr;
}



bool Scope::Contains(ast::IdentifierExprAST *Ident) {
    return GetBinding(Ident) != nullptr;
}


llvm::Type*Scope::GetType(ast::IdentifierExprAST *Ident) {
    return std::get<1>(*_GetEntry(Ident));
}

void Scope::SetType(ast::IdentifierExprAST *Ident, llvm::Type *Type) {
    throw 0;
    auto E = _GetEntry(Ident);
    std::get<1>(*E) = Type;
}


ValueBinding *Scope::GetBinding(ast::IdentifierExprAST *Ident) {
    return std::get<2>(*_GetEntry(Ident)).get();
    //return &std::get<2>(*_GetEntry(Ident));
}


void Scope::Iterate(std::function<void (ast::IdentifierExprAST*, llvm::Type*, ValueBinding*, bool*)> Fn) {
    bool Stop = false;
    for (V::reverse_iterator It = Symbols.rbegin(); It != Symbols.rend(); It++) {
        Fn(std::get<0>(*It), std::get<1>(*It), std::get<2>(*It).get(), &Stop);
        if (Stop) break;
    }
}


// The marker is the index of the next element that would be inserted
Scope::Marker Scope::GetMarker() {
    return Symbols.size();
}

Scope::V Scope::GetEntriesSinceMarker(Marker M) {
    if (M >= Symbols.size()) return {};
    return Scope::V(Symbols.begin() + M, Symbols.end());
}


void Scope::Dump() {
    std::cout << "Scope:\n";
    for (auto &Entry : Symbols) {
        std::cout << "- " << std::get<0>(Entry)->getValue() << ": ";
        std::get<1>(Entry)->print(outs()); std::cout << "\n";
    }
}








void TypeCache::Register(ast::StructDeclAST *StructDecl, llvm::Type *LLVMType) {
    std::vector<StructDescriptor::Attribute> Attributes;
    for (auto &VarDecl : StructDecl->getAttributes()) {
        Attributes.push_back(StructDescriptor::Attribute(VarDecl->Name->getValue(), VarDecl->Type));
    }
    
    auto Name = StructDecl->getIdentifier()->getValue();
    Structs.insert({Name, StructDescriptor(Name, Attributes, LLVMType)});
    //Structs[Name] = StructDescriptor(Name, Attributes);
    // TODO: whatthefuck why does the insert line compile, but the assignment one below not???
}


TypeCache::StructDescriptor *TypeCache::Get(std::string Name) {
    return &Structs.at(Name);
}

//
// IRGEN
//


IRGenerator::IRGenerator(llvm::LLVMContext &C, const std::string &ModuleName) : C(C), Builder(C) {
    Module = llvm::make_unique<llvm::Module>(ModuleName, C);
    M = Module.get();
    
    i8  = Type::getInt8Ty(C);
    i16 = Type::getInt16Ty(C);
    i32 = Type::getInt32Ty(C);
    i64 = Type::getInt64Ty(C);
    
    Void = Type::getVoidTy(C);
    Bool = Type::getInt1Ty(C);
    Double = Type::getDoubleTy(C);
    i8_ptr = i8->getPointerTo();
}


void IRGenerator::Preflight(AST &Ast) {
    for (auto &Node : Ast) {
        if (auto FunctionDecl = Node->As<FunctionDeclAST>()) {
            RegisterFunctionSignature(FunctionDecl->getSignature());
        } else if (auto ExternFunctionDecl = Node->As<ExternFunctionDeclAST>()) {
            RegisterFunctionSignature(ExternFunctionDecl->getSignature(), false);
        } else if (auto StructDecl = Node->As<StructDeclAST>()) {
            RegisterStruct(StructDecl);
        } else if (auto EnumDecl = Node->As<ast::EnumDecl>()) {
            LKLog("TODO: Handle EnumDecl");
        } else {
            report_fatal_error("TODO");
        }
    }
}



std::string MangleFunction(std::string Name) {
    if (Name == "main") return Name;
    return mangling::MangleFunction(Name);
}

void IRGenerator::RegisterFunctionSignature(ast::FunctionSignature &FunctionSignature, bool MangleName) {
    std::vector<llvm::Type *> ParameterTypes;
    
    for (auto &P : FunctionSignature.getParameters()) {
        ParameterTypes.push_back(GetLLVMType(P->Type));
    }
    
    auto Name = MangleName ? MangleFunction(FunctionSignature.getName()) : FunctionSignature.getName();
    
    auto FT = FunctionType::get(GetLLVMType(FunctionSignature.getReturnType()), ParameterTypes, false);
    auto F = Function::Create(FT, Function::LinkageTypes::ExternalLinkage, Name, M);
    
    unsigned Idx = 0;
    for (auto &Arg : F->args()) {
        Arg.setName(FunctionSignature.getParameters()[Idx++]->Name->getValue());
    }
}


void IRGenerator::RegisterStruct(ast::StructDeclAST *StructDecl) {
    auto S = llvm::StructType::create(C, StructDecl->getIdentifier()->getValue());
    
    std::vector<Type *> Members;
    for (auto Attribute : StructDecl->getAttributes()) {
        Members.push_back(GetLLVMType(Attribute->Type));
    }
    S->setBody(Members);
    
    
    TypeCache.Register(StructDecl, S);
    
    GenerateInitializerForStruct(StructDecl);
}







void IRGenerator::Codegen(AST &Ast) {
    Preflight(Ast);
    
    for (auto &Node : Ast) {
        Codegen(Node.get());
    }
}




Value *IRGenerator::Codegen(Node *Node) {
#define HANDLE_TYPE(T) if (auto N = Node->As<T>()) return Codegen(N);
#define IGNORE_TYPE(T) if (Node->GetNodeType() == Node::NodeType::T) return nullptr;
    
    // TODO put these in the order in which they're most likely to occur ??
    HANDLE_TYPE(FunctionDeclAST)
    HANDLE_TYPE(NumberExprAST)
    HANDLE_TYPE(FunctionCallAST)
    HANDLE_TYPE(ReturnStmtAST)
    HANDLE_TYPE(BinaryOperationExprAST)
    HANDLE_TYPE(IdentifierExprAST)
    HANDLE_TYPE(VariableDeclAST)
    HANDLE_TYPE(Assignment)
    HANDLE_TYPE(IfStmt)
    HANDLE_TYPE(Comparison)
    
    IGNORE_TYPE(StructDecl)
    IGNORE_TYPE(EnumDecl)
    IGNORE_TYPE(ExternFunctionDecl)
    
    
    std::cout << "Unhandled Node: " << Node->GetNodeTypeName() << "\n";
    report_fatal_error("dafuq");
#undef HANDLE_TYPE
#undef IGNORE_TYPE
}





Value *IRGenerator::Codegen(FunctionDeclAST *FunctionDecl, bool MangleName) {
    assert(Scope.IsEmpty());
    
    std::string Name = FunctionDecl->getSignature().getName();
    if (MangleName) Name = MangleFunction(Name);
    auto F = M->getFunction(Name);
    if (!F) {
        LKFatalError("Unable to find function '%s'", Name.c_str());
    }
    
    // TODO make function arguments immutable?
    for (auto &Arg : F->args()) {
        auto Binding = ValueBinding([&]() {
            return &Arg;
        }, [/*this, */&Arg, &Name](llvm::Value *Value) {
            LKFatalError("Function arguments are immutable (%s in %s)", Arg.getName().str().c_str(), Name.c_str());
        });
        // TODO the identifier we're creating here will never be freed
        Scope.Insert(new ast::IdentifierExprAST(Arg.getName()), Arg.getType(), std::move(Binding));
        
    }
    
    auto BB = BasicBlock::Create(C, "entry", F);
    Builder.SetInsertPoint(BB);

    Codegen(FunctionDecl->getBody());
    return F;
}


Value *IRGenerator::Codegen(VariableDeclAST *VariableDecl) {
    llvm::Type *Type = nullptr;
    llvm::Value *InitialValue = nullptr;
    
    if (auto T = VariableDecl->Type) {
        Type = GetLLVMType(T);
    } else {
        // If no type is specified, there _has_ to be an initial value
        assert(VariableDecl->InitialValue);
    }
    
    
    if (auto InitialValueExpr = VariableDecl->InitialValue) {
        InitialValue = Codegen(InitialValueExpr);
        if (!Type) {
            Type = InitialValue->getType();
        }
    }
    
    assert(Type);
    
    auto Alloca = Builder.CreateAlloca(Type);
    Alloca->setName(VariableDecl->Name->getValue());
    
    outs() << VariableDecl->Name->getValue() << ": "; Type->print(outs()); outs() << "\n\n";
    
    auto Binding = ValueBinding([this, Alloca] () {
        return Builder.CreateLoad(Alloca);
    }, [=] (llvm::Value *V) {
        outs() << "Type: "; Type->print(outs()); outs() << "\n";
        if (!TypecheckAndApplyTrivialIntCastIfPossible(&V, Type)) {
            throw;
            //outs() << "Type mismatch: Cannot assign value of type " << V->getType() << " to variable of type " << Type << "\n";
        }
        Builder.CreateStore(V, Alloca);
    });
    
    Scope.Insert(VariableDecl->Name, Type, Binding);
    
    if (InitialValue) {
        Binding.Write(InitialValue);
    }
    
    return Alloca;
}





Value *IRGenerator::Codegen(ast::Composite *Composite) {
    auto M = Scope.GetMarker();
    
    for (auto &Stmt : Composite->getStatements()) {
        if (auto ReturnStmt = Stmt->As<ReturnStmtAST>()) {
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
    
    // If statements are implemented as phi nodes, which means we need every branch to return something
    return ConstantInt::get(i8, 0);
}



Value *IRGenerator::Codegen(ReturnStmtAST *ReturnStmt) {
    return Builder.CreateRet(Codegen(ReturnStmt->getExpression()));
}




Value *IRGenerator::Codegen(NumberExprAST *Number) {
    return ConstantInt::get(i64, Number->getValue());
}


Value *IRGenerator::Codegen(FunctionCallAST *Call) {
    auto F = M->getFunction(mangling::MangleFunction(Call->getTarget()));
    if (!F) {
        F = M->getFunction(Call->getTarget());
    }
    if (!F) {
        LKFatalError("Unable to find function %s", Call->getTarget().c_str());
    }
    
    auto FT = F->getFunctionType();
    
    std::vector<Value *> Args;
    
    for (auto I = 0; I < Call->getArguments().size(); I++) {
        auto ExpectedType = FT->getParamType(I);
        auto V = Codegen(Call->getArguments()[I]);
        if (!TypecheckAndApplyTrivialIntCastIfPossible(&V, ExpectedType)) {
            outs() << "Type mismatch: Cannot pass expression of type " << V->getType() << " to function '" << F->getName() << "' expecting " << ExpectedType << "\n";
            throw;
        }
        Args.push_back(V);
    }
    
    
    return Builder.CreateCall(F, Args);
}




llvm::Instruction::BinaryOps GetLLVMBinaryOpInstruction_Int(ast::BinaryOperationExprAST::Operation Op, bool IsSigned) {
    using Operation = ast::BinaryOperationExprAST::Operation;
    using BinaryOps = llvm::Instruction::BinaryOps;
    
    switch (Op) {
        case Operation::Add: return BinaryOps::Add;
        case Operation::Sub: return BinaryOps::Sub;
        case Operation::Mul: return BinaryOps::Mul;
        case Operation::Div: return IsSigned ? BinaryOps::SDiv : BinaryOps::UDiv;
        case Operation::Mod: return IsSigned ? BinaryOps::SRem : BinaryOps::URem;
        case Operation::And: return BinaryOps::And;
        case Operation::Or:  return BinaryOps::Or;
        case Operation::Xor: return BinaryOps::And;
        case Operation::Shl: return BinaryOps::Shl;
        case Operation::Shr: return BinaryOps::LShr; // TODO (important) arithmetic or logical right shift?
    }
    
    llvm_unreachable("should never reach here");
}




llvm::Instruction::BinaryOps GetLLVMBinaryOpInstruction_Double(ast::BinaryOperationExprAST::Operation Op) {
    using Operation = ast::BinaryOperationExprAST::Operation;
    using BinaryOps = llvm::Instruction::BinaryOps;
    
    switch (Op) {
        case Operation::Add: return BinaryOps::FAdd;
        case Operation::Sub: return BinaryOps::FSub;
        case Operation::Mul: return BinaryOps::FMul;
        case Operation::Div: return BinaryOps::FDiv;
        case Operation::Mod: return BinaryOps::FRem;
        default: llvm_unreachable("nope");
    }
}



Value *IRGenerator::Codegen(BinaryOperationExprAST *BinaryOperation) {
    auto LHS = Codegen(BinaryOperation->getLhs());
    auto RHS = Codegen(BinaryOperation->getRhs());
    
    if (!Binop_AttemptToResolvePotentialIntTypeMismatchesByCastingNumberLiteralsIfPossible(&LHS, &RHS)) {
        report_fatal_error("Unable to resolve type mismatch between comparison operands");
    }
    
    Instruction::BinaryOps Op;
    auto T = LHS->getType(); // same as RHS->getType()
    if (T->isIntegerTy()) {
        Op = GetLLVMBinaryOpInstruction_Int(BinaryOperation->getOp(), IsSignedType(T));
    } else if (T->isDoubleTy()) {
        Op = GetLLVMBinaryOpInstruction_Double(BinaryOperation->getOp());
    } else {
        printf("Unhandled Type in binop:\n");
        T->print(outs());
        throw;
    }
    
    return Builder.CreateBinOp(Op, LHS, RHS);
}



Value *IRGenerator::Codegen(IdentifierExprAST *Identifier) {
    // Check local variables
    if (auto *Binding = Scope.GetBinding(Identifier)) {
        return Binding->Read();
    }
    
    std::cout << "Name: " << Identifier->getValue() << std::endl;
    report_fatal_error("ugh fuck no");
}





Value *IRGenerator::Codegen(Assignment *Assignment) {
    if (auto Ident = Assignment->getTarget()->As<IdentifierExprAST>()) {
        auto Binding = Scope.GetBinding(Ident);
        if (!Binding) {
            throw "unable to find identifier";
        }
        
        // Typechecks and potential trivial casts are performed in `VarBinding.Write`
        auto V = Codegen(Assignment->getValue());
        Binding->Write(V);
        return V;
    }
    throw 0;
}











//
// CONTROL FLOW
//

Value *IRGenerator::Codegen(IfStmt *If) {
    auto F = Builder.GetInsertBlock()->getParent();
    
    
    std::vector<BasicBlock *> BranchBodyBlocks;
    for (auto I = 0; I < If->getBranches().size(); I++) {
        auto Name = Twine(F->getName()).concat("_if_body").concat(Twine(I));
        BranchBodyBlocks.push_back(BasicBlock::Create(C, Name));
    }
    
    
    
    // The entry points to each branch's condition
    // Note that if the last branch is a conditionless else branch, this points directly to the branch body
    std::vector<BasicBlock *> BranchConditionBlocks;
    
    for (auto I = 0; I < If->getBranches().size(); I++) {
        if (If->getBranches()[I]->Kind == IfStmt::Branch::BranchKind::Else) break;
        auto Name = Twine(F->getName()).concat("_if_cond_").concat(Twine(I));
        BranchConditionBlocks.push_back(BasicBlock::Create(C, Name));
    }
    
    BranchConditionBlocks.push_back(BranchBodyBlocks.back());
    
    for (auto I = 0; I < If->getBranches().size(); I++) {
        if (If->getBranches()[I]->Kind == IfStmt::Branch::BranchKind::Else) break;
        if (I > 0) {
            auto BB = BranchConditionBlocks[I];
            F->getBasicBlockList().push_back(BB);
            Builder.SetInsertPoint(BB);
        }
        
        auto Cond = If->getBranches()[I]->Condition;
        auto CondV = Codegen(Cond);
        Builder.CreateCondBr(CondV, BranchBodyBlocks[I], BranchConditionBlocks[I + 1]);
    }
    
    std::vector<Value *> BranchValues;
    auto MergeBB = BasicBlock::Create(C, "merge");
    
    for (auto I = 0; I < If->getBranches().size(); I++) {
        auto BB = BranchBodyBlocks[I];
        F->getBasicBlockList().push_back(BB);
        Builder.SetInsertPoint(BB);
        
        BranchValues.push_back(Codegen(If->getBranches()[I]->Body));
        Builder.CreateBr(MergeBB);
        BranchBodyBlocks[I] = Builder.GetInsertBlock();
    }
    
    F->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    
    // the result of the phi instruction is currently unused, might be useful though in the future
    auto PHI = Builder.CreatePHI(i8, If->getBranches().size());
    
    for (auto I = 0; I < If->getBranches().size(); I++) {
        PHI->addIncoming(BranchValues[I], BranchBodyBlocks[I]);
    }
    return PHI;
}





llvm::CmpInst::Predicate GetMatchingLLVMPredicateForComparison(ast::Comparison *Comparison) {
    using Operation = ast::Comparison::Operation;
    using Predicate = llvm::CmpInst::Predicate;
    
    switch (Comparison->getOp()) {
        case Operation::EQ: return Predicate::ICMP_EQ;
        case Operation::NE: return Predicate::ICMP_NE;
        case Operation::LT: return Predicate::ICMP_SLT; // TODO differentiate between signed and unsigned
        case Operation::GT: return Predicate::ICMP_SGT;
        case Operation::LE: return Predicate::ICMP_SLE;
        case Operation::GE: return Predicate::ICMP_SGE;
    }
}



Value *IRGenerator::Codegen(ast::Comparison *Comparison) {
    auto LHS = Codegen(Comparison->getLhs());
    auto RHS = Codegen(Comparison->getRhs());
    
    // TODO signed/unsigned types
    //assert_implication(IsSignedType(LHS->getType()), IsSignedType(RHS->getType()));
    
    auto Pred = GetMatchingLLVMPredicateForComparison(Comparison);
    
    if (!Binop_AttemptToResolvePotentialIntTypeMismatchesByCastingNumberLiteralsIfPossible(&LHS, &RHS)) {
        report_fatal_error("Unable to resolve type mismatch between comparison operands");
    }
    
    return Builder.CreateICmp(Pred, LHS, RHS, "cond");
}










//
// TYPES
//



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
    }
    
    if (TI->Kind == TypeInfo::Kind::Complex) {
        //TypeCache.Get(TI->Data.Name)->LLVMType->print(outs());
        return TypeCache.Get(TI->Data.Name)->LLVMType;
    }
    
    throw;
#undef HANDLE
}



// All int types are treated as signed for now, so this isn't really necessary
bool IRGenerator::IsSignedType(llvm::Type *T) {
    return T == i8 || T == i16 || T == i32 || T == i64;
}



uint64_t IRGenerator::Sizeof(llvm::Type *Ty) {
    return M->getDataLayout().getTypeAllocSize(Ty);
}


// Inserts casts if operand types mismatch
// For example, the expression `x` + 5` wouldn't compile if `x` was i8 (since the literal is inferred to be i64)
// But literals can trivially be cast to the expected type
// Returns false if there is a type mismatch, but we were unable to resolve it
bool IRGenerator::Binop_AttemptToResolvePotentialIntTypeMismatchesByCastingNumberLiteralsIfPossible(llvm::Value **LHS, llvm::Value **RHS) {
    
    auto T_LHS = (*LHS)->getType();
    auto T_RHS = (*RHS)->getType();
    
    if (T_LHS == T_RHS) return true;
    
    // There's no need to check for doubles here, since there's only one double type
    if (!(T_LHS->isIntegerTy() && T_RHS->isIntegerTy())) {
        outs() << "Unable to compare incompatible types " << T_LHS << " and " << T_RHS << "\n" ;
        throw;
    }
    
    if ((*LHS)->getValueID() == llvm::Value::ValueTy::ConstantIntVal) {
        // lhs is literal, cast to type of rhs
        *LHS = Builder.CreateIntCast(*LHS, T_RHS, IsSignedType(T_RHS));
    } else if ((*RHS)->getValueID() == llvm::Value::ValueTy::ConstantIntVal) {
        // rhs is literal, cast to type of lhs
        *RHS = Builder.CreateIntCast(*RHS, T_LHS, IsSignedType(T_LHS));
    } else {
        return false;
    }
    return true;
}



bool IRGenerator::TypecheckAndApplyTrivialIntCastIfPossible(llvm::Value **V, llvm::Type *DestTy) {
    auto SrcTy = (*V)->getType();
    
//    llvm::outs() << "SrcTy: "; SrcTy->print(outs());  outs() << "\n";
//    llvm::outs() << "DstTy: "; DestTy->print(outs()); outs() << "\n";
//    llvm::outs() << "V:     "; (*V)->print(outs()); outs() << "\n";
//
//    if (DestTy->isVoidTy()) {
//        throw;
//    }
    
    if (SrcTy == DestTy) return true;
    
    if (DestTy->isIntegerTy() && (*V)->getValueID() == Value::ValueTy::ConstantIntVal) {
        *V = Builder.CreateIntCast(*V, DestTy, IsSignedType(DestTy));
        return true;
    }
    
    return false;
}
















//
// CODE SYNTHESIS
//





llvm::Value *IRGenerator::GenerateInitializerForStruct(ast::StructDeclAST *StructDecl) {
    auto Struct = TypeCache.Get(StructDecl->getIdentifier()->getValue());
    
    auto Signature = FunctionSignature(nullptr,
                                       mangling::MangleMethod(Struct->Name, "init", mangling::MethodKind::Static),
                                       StructDecl->getAttributes(),
                                       TypeInfo::MakeComplex(Struct->Name));
    
    RegisterFunctionSignature(Signature, false);
    
    // TODO pretty much all pointers we create below never get freed
    auto self = new IdentifierExprAST("self");
    
    std::vector<ast::LocalStmtAST *> Body;
    
    // Declare & allocate self
    Body.push_back(new VariableDeclAST(self, TypeInfo::i8_ptr, nullptr));
    Body.push_back(new Assignment(self, new FunctionCallAST("malloc", { new NumberExprAST(Sizeof(Struct->LLVMType)) }, false)));
    
    for (auto &Attr : Struct->Attributes) {
        
    }
    
    Body.push_back(new ReturnStmtAST(self));
    
    
    auto C = new Composite(Body);
    auto F = new FunctionDeclAST(Signature, C);
    return Codegen(F, false);
}







#endif
