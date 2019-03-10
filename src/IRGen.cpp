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
    
    i8_ptr = i8->getPointerTo();
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
    HANDLE(LocalStmt, VariableDecl)
    HANDLE(LocalStmt, IfStmt)
    HANDLE(LocalStmt, Assignment)
    
    unhandled_node(LocalStmt);
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Expr> Expr) {
    HANDLE(Expr, NumberLiteral)
    HANDLE(Expr, BinaryOperation)
    HANDLE(Expr, Identifier)
    HANDLE(Expr, FunctionCall)
    HANDLE(Expr, Comparison)
    HANDLE(Expr, LogicalOperation)
    HANDLE(Expr, Typecast)
    
    unhandled_node(Expr)
}

#undef HANDLE
#undef IGNORE
#undef unhandled_node


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::FunctionDecl> FunctionDecl, bool MangleName) {
    precondition(Scope.IsEmpty());
    
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






#pragma mark - Local Statements


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::VariableDecl> Decl) {
    llvm::Type *Type = nullptr;
    llvm::Value *InitialValue = nullptr;
    
    if (Decl->Type) {
        Type = GetLLVMType(Decl->Type);
    } else {
        // If no type is specified, there _has_ to be an initial value
        precondition(Decl->InitialValue);
    }
    
    if (auto InitialValueExpr = Decl->InitialValue) {
        InitialValue = Codegen(InitialValueExpr);
        if (!Type) {
            Type = InitialValue->getType();
        }
    }
    
    precondition(Type);
    
    auto Alloca = Builder.CreateAlloca(Type);
    Alloca->setName(Decl->Name->Value);
    
    auto Binding = ValueBinding([this, Alloca] () {
        return Builder.CreateLoad(Alloca);
    }, [=] (llvm::Value *V) {
        if (!TypecheckAndApplyTrivialCastIfPossible(&V, Type)) {
            llvm::outs() << "Type mismatch: cannot assign value of type " << V->getType() << " to variable of type " << Type << "\n";
            throw;
        }
        Builder.CreateStore(V, Alloca);
    });
    
    Scope.Insert(Decl->Name->Value, Type, Binding);
    
    if (InitialValue) {
        Binding.Write(InitialValue);
    }
    
    return Alloca;
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


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Assignment> Assignment) {
    if (auto Ident = std::dynamic_pointer_cast<ast::Identifier>(Assignment->Target)) {
        auto Binding = Scope.GetBinding(Ident->Value);
        if (!Binding) throw;
        
        Binding->Write(Codegen(Assignment->Value));
    }
    
    
    // TODO should assignments return something?
    return nullptr;
}








#pragma mark - Expressions


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::NumberLiteral> Number) {
    return ConstantInt::get(i64, Number->Value);
}



llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Identifier> Ident) {
    if (auto Binding = Scope.GetBinding(Ident->Value)) {
        return Binding->Read();
    }
    
    std::cout << "Unable to find identifier " << Ident->Value << std::endl;
    throw;
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Typecast> Cast) {
    auto V = Codegen(Cast->Expression);
    
    auto SrcType = V->getType();
    auto DestType = GetLLVMType(Cast->DestType);
    
    if (SrcType == DestType) return V;
    
    llvm::Instruction::CastOps CastOp;
    
    if (SrcType->isIntegerTy() && DestType->isIntegerTy()) {
        if (SrcType->getIntegerBitWidth() > DestType->getIntegerBitWidth()) {
            // casting to a smaller type
            CastOp = llvm::Instruction::CastOps::Trunc;
        } else {
            // casting to a larger type
            if (IsSignedType(SrcType)) {
                CastOp = llvm::Instruction::CastOps::SExt;
            } else {
                CastOp = llvm::Instruction::CastOps::ZExt;
            }
        }
    } else {
        throw;
    }
    
    return Builder.CreateCast(CastOp, V, DestType);
}



llvm::Instruction::BinaryOps GetLLVMBinaryOpInstruction_Int(ast::BinaryOperation::Operation Op, bool IsSigned) {
    using Operation = ast::BinaryOperation::Operation;
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




llvm::Instruction::BinaryOps GetLLVMBinaryOpInstruction_Double(ast::BinaryOperation::Operation Op) {
    using Operation = ast::BinaryOperation::Operation;
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




llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::BinaryOperation> Binop) {
    auto LHS = Codegen(Binop->LHS);
    auto RHS = Codegen(Binop->RHS);
    
    if (!Binop_AttemptToResolvePotentialIntTypeMismatchesByCastingNumberLiteralsIfPossible(&LHS, &RHS)) {
        llvm::outs() << "Unable to resolve type mismatch between comparison operands " << LHS->getType() << " and " << RHS->getType() << "\n";
        throw;
    }
    
    llvm::Instruction::BinaryOps Op;
    auto T = LHS->getType(); // same as RHS->getType()
    if (T->isIntegerTy()) {
        Op = GetLLVMBinaryOpInstruction_Int(Binop->Op, IsSignedType(T));
    } else if (T->isDoubleTy()) {
        Op = GetLLVMBinaryOpInstruction_Double(Binop->Op);
    } else {
        llvm::outs() << "Unhandled tyoe in binop: " << T << "\n";
        throw;
    }
    
    return Builder.CreateBinOp(Op, LHS, RHS);
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





#pragma mark - Conditions


llvm::CmpInst::Predicate GetMatchingLLVMCmpInstPredicateForComparisonOperator(ast::Comparison::Operation Op) {
    using Operation = ast::Comparison::Operation;
    using Predicate = llvm::CmpInst::Predicate;
    
    switch (Op) {
        case ast::Comparison::Operation::EQ: return Predicate::ICMP_EQ;
        case ast::Comparison::Operation::NE: return Predicate::ICMP_NE;
        case ast::Comparison::Operation::LT: return Predicate::ICMP_SLT; // TODO differentiate between signed and unsigned
        case ast::Comparison::Operation::LE: return Predicate::ICMP_SLE;
        case ast::Comparison::Operation::GT: return Predicate::ICMP_SGT;
        case ast::Comparison::Operation::GE: return Predicate::ICMP_SGE;
    }
}



llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Comparison> Comparison) {
    auto LHS = Codegen(Comparison->LHS);
    auto RHS = Codegen(Comparison->RHS);
    
    precondition(LHS->getType()->isIntegerTy());
    
    // TODO signed/unsigned types
    //assert_implication(IsSignedType(LHS->getType()), IsSignedType(RHS->getType()));
    
    if (!Binop_AttemptToResolvePotentialIntTypeMismatchesByCastingNumberLiteralsIfPossible(&LHS, &RHS)) {
        outs() << "Type mismatch: Unable to compare incompatible types '" << LHS->getType() << "' and '" << RHS->getType() << "'\n";
        throw;
    }
    
    auto Pred = GetMatchingLLVMCmpInstPredicateForComparisonOperator(Comparison->Op);
    return Builder.CreateICmp(Pred, LHS, RHS);
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::LogicalOperation> LogOp) {
    auto LHS = Codegen(LogOp->LHS);
    auto RHS = Codegen(LogOp->RHS);
    
    precondition(LHS->getType() == Bool && RHS->getType() == Bool);
    
    auto Op = LogOp->Op == ast::LogicalOperation::Operation::And
        ? llvm::Instruction::BinaryOps::And
        : llvm::Instruction::BinaryOps::Or;
    
    return Builder.CreateBinOp(Op, LHS, RHS);
}







llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::IfStmt> If) {
    auto F = Builder.GetInsertBlock()->getParent();
    
    
    std::vector<BasicBlock *> BranchBodyBlocks;
    for (auto I = 0; I < If->Branches.size(); I++) {
        auto Name = Twine(F->getName()).concat("_if_body").concat(Twine(I));
        BranchBodyBlocks.push_back(BasicBlock::Create(C, Name));
    }
    
    
    
    // The entry points to each branch's condition
    // Note that if the last branch is a conditionless else branch, this points directly to the branch body
    std::vector<BasicBlock *> BranchConditionBlocks;
    
    for (auto I = 0; I < If->Branches.size(); I++) {
        if (If->Branches[I]->Kind == ast::IfStmt::Branch::BranchKind::Else) break;
        auto Name = Twine(F->getName()).concat("_if_cond_").concat(Twine(I));
        BranchConditionBlocks.push_back(BasicBlock::Create(C, Name));
    }
    
    BranchConditionBlocks.push_back(BranchBodyBlocks.back());
    
    for (auto I = 0; I < If->Branches.size(); I++) {
        if (If->Branches[I]->Kind == ast::IfStmt::Branch::BranchKind::Else) break;
        if (I > 0) {
            auto BB = BranchConditionBlocks[I];
            F->getBasicBlockList().push_back(BB);
            Builder.SetInsertPoint(BB);
        }
        
        auto Cond = If->Branches[I]->Condition;
        auto CondV = Codegen(Cond);
        Builder.CreateCondBr(CondV, BranchBodyBlocks[I], BranchConditionBlocks[I + 1]);
    }
    
    std::vector<Value *> BranchValues;
    auto MergeBB = BasicBlock::Create(C, "merge");
    
    for (auto I = 0; I < If->Branches.size(); I++) {
        auto BB = BranchBodyBlocks[I];
        F->getBasicBlockList().push_back(BB);
        Builder.SetInsertPoint(BB);
        
        BranchValues.push_back(Codegen(If->Branches[I]->Body));
        Builder.CreateBr(MergeBB);
        BranchBodyBlocks[I] = Builder.GetInsertBlock();
    }
    
    F->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    
    // the result of the phi instruction is currently unused, might be useful though in the future
    auto PHI = Builder.CreatePHI(i8, If->Branches.size());
    
    for (auto I = 0; I < If->Branches.size(); I++) {
        PHI->addIncoming(BranchValues[I], BranchBodyBlocks[I]);
    }
    return PHI;
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



bool IRGenerator::TypecheckAndApplyTrivialCastIfPossible(llvm::Value **V, llvm::Type *DestType) {
    auto SrcTy = (*V)->getType();
    
    if (SrcTy == DestType) return true;
    
    if (DestType->isIntegerTy() && (*V)->getValueID() == llvm::Value::ValueTy::ConstantIntVal) {
        *V = Builder.CreateIntCast(*V, DestType, IsSignedType(DestType)); // TODO add support for signed/unsigned types
        return true;
    }
    return false;
}
