//
//  IRGen.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-02-24.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "IRGen.h"

#include <optional>

#include "Mangling.h"

using namespace irgen;



// Utils


llvm::raw_ostream& operator<<(llvm::raw_ostream &OS, llvm::Type *T) {
    T->print(OS);
    return OS;
}

llvm::raw_ostream& operator<<(llvm::raw_ostream &OS, llvm::Value *V) {
    V->print(OS);
    return OS;
}



std::string MangleFunctionName(std::string Name) {
    return Name == "main" ? Name : mangling::MangleFunction(Name);
}

std::string MangleFunctionName(std::shared_ptr<ast::FunctionSignature> Signature, std::optional<std::string> Typename) {
    using FK = ast::FunctionDecl::FunctionKind;
    
    switch (Signature->Kind) {
        case FK::GlobalFunction:
            return MangleFunctionName(Signature->Name);
        case FK::InstanceMethod:
            precondition(Typename);
            return mangling::MangleMethod(Typename.value(), Signature->Name, mangling::MethodKind::Instance);
        case FK::StaticMethod:
            precondition(Typename);
            return mangling::MangleMethod(Typename.value(), Signature->Name, mangling::MethodKind::Static);
    }
}




llvm::LLVMContext IRGenerator::C;


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
        
        } else if (auto StructDecl = std::dynamic_pointer_cast<ast::StructDecl>(Node)) {
            RegisterStructDecl(StructDecl);
        
        } else if (auto ImplBlock = std::dynamic_pointer_cast<ast::ImplBlock>(Node)) {
            RegisterImplBlock(ImplBlock);
        }
    }
}


void IRGenerator::RegisterFunctionSignature(std::shared_ptr<ast::FunctionSignature> Signature, bool MangleName, std::optional<std::string> Typename) {
    std::vector<llvm::Type *> ParameterTypes;
    
    for (auto &P : Signature->Parameters) {
        ParameterTypes.push_back(GetLLVMType(P->Type));
    }
    
    std::string Name;
    
    if (!MangleName) {
        precondition(Typename == std::nullopt);
        Name = Signature->Name;
    } else {
        Name = MangleFunctionName(Signature, Typename);
    }
    
    auto FT = llvm::FunctionType::get(GetLLVMType(Signature->ReturnType), ParameterTypes, false);
    auto F = llvm::Function::Create(FT, llvm::Function::LinkageTypes::ExternalLinkage, Name, M);
    
    unsigned Idx = 0;
    for (auto &Arg : F->args()) {
        Arg.setName(Signature->Parameters[Idx++]->Name->Value);
    }
}



void IRGenerator::RegisterStructDecl(std::shared_ptr<ast::StructDecl> Struct) {
    TypeCache.Insert(Struct->Name->Value, Struct);
}



void IRGenerator::RegisterImplBlock(std::shared_ptr<ast::ImplBlock> ImplBlock) {
    auto Typename = ImplBlock->Typename;
    precondition(TypeCache.Contains(Typename));
    
    auto T = TypeInfo::MakeComplex(Typename);
    
    for (auto &M : ImplBlock->Methods) {
        auto Kind = ast::FunctionSignature::FunctionKind::StaticMethod;
        if (!M->Parameters.empty()) {
            auto First = M->Parameters[0];
            if (First->Name->Value == "self" && First->Type->Equals(T)) {
                Kind = ast::FunctionSignature::FunctionKind::InstanceMethod;
            }
        }
        M->Kind = Kind;
        
        RegisterFunctionSignature(M, true, Typename);
    }
}



# pragma mark - Codegen


#define HANDLE(node, T, ...) \
if (auto X = std::dynamic_pointer_cast<ast::T>(node)) return Codegen(X, ## __VA_ARGS__);

#define IGNORE(node, T) \
if (std::dynamic_pointer_cast<ast::T>(node)) return nullptr;

#define unhandled_node(node) \
{ std::cout << "[IRGenerator::Codegen] Unhandled Node: " << util::typeinfo::GetTypename(*(node)) << std::endl; \
throw; }


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::TopLevelStmt> TLS) {
    HANDLE(TLS, FunctionDecl)
    IGNORE(TLS, ExternFunctionDecl)
    HANDLE(TLS, StructDecl)
    HANDLE(TLS, ImplBlock)
    
    unhandled_node(TLS)
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::LocalStmt> LocalStmt) {
    HANDLE(LocalStmt, Composite)
    HANDLE(LocalStmt, FunctionCall)
    HANDLE(LocalStmt, VariableDecl)
    HANDLE(LocalStmt, IfStmt)
    HANDLE(LocalStmt, Assignment)
    HANDLE(LocalStmt, MemberAccess, CodegenReturnValueKind::Value);
    
    unhandled_node(LocalStmt);
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Expr> Expr, CodegenReturnValueKind ReturnValueKind) {
    HANDLE(Expr, NumberLiteral)
    HANDLE(Expr, BinaryOperation)
    HANDLE(Expr, Identifier, ReturnValueKind)
    HANDLE(Expr, FunctionCall)
    HANDLE(Expr, Comparison)
    HANDLE(Expr, LogicalOperation)
    HANDLE(Expr, Typecast)
    HANDLE(Expr, MemberAccess, ReturnValueKind)
    HANDLE(Expr, StringLiteral)
    HANDLE(Expr, CharLiteral)
    
    unhandled_node(Expr)
}

#undef HANDLE
#undef IGNORE
#undef unhandled_node




#pragma mark - Top Level Statements

llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::FunctionDecl> FunctionDecl, bool MangleName, std::optional<std::string> Typename) {
    precondition(Scope.IsEmpty());
    
    auto Name = !MangleName ? FunctionDecl->Name : MangleFunctionName(FunctionDecl, Typename);
    
    auto F = M->getFunction(Name);
    if (!F) {
        LKFatalError("Unable to find function '%s'", Name.c_str());
    }
    
    for (auto &Arg : F->args()) {
        auto Binding = ValueBinding(&Arg, [&]() {
            return &Arg;
        }, [&](llvm::Value *Value) {
            LKFatalError("Function arguments are read-only (%s in %s)", Arg.getName().str().c_str(), Name.c_str());
        });
        
        Scope.Insert(Arg.getName(), Arg.getType(), std::move(Binding));
    }
    
    auto BB = llvm::BasicBlock::Create(C, "entry", F);
    Builder.SetInsertPoint(BB);
    
    Codegen(FunctionDecl->Body, true);
    return F;
}




llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::StructDecl> Struct) {
    GenerateStructInitializer(Struct);
    return nullptr;
}



llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::ImplBlock> ImplBlock) {
    auto Typename = ImplBlock->Typename;
    for (auto &M : ImplBlock->Methods) {
        Codegen(M, true, Typename);
    }
    return nullptr;
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
    
    auto Binding = ValueBinding(Alloca, [this, Alloca] () {
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







llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Composite> Composite, bool IsFunctionBody) {
    auto M = Scope.GetMarker();
    
    bool DidReturn = false;
    
    for (auto It = Composite->Statements.begin(); It != Composite->Statements.end(); It++) {
        auto &Stmt = *It;
        if (auto ReturnStmt = std::dynamic_pointer_cast<ast::ReturnStmt>(Stmt)) {
            Codegen(ReturnStmt);
            DidReturn = true;
            break;
        } else {
            Codegen(Stmt);
            if (IsFunctionBody && It + 1 == Composite->Statements.end()) {
                // Reached the end of the composite w/out a return statement
                auto F = Builder.GetInsertBlock()->getParent();
                precondition(F->getReturnType() == Void);
                Builder.CreateRetVoid();
                DidReturn = true;
            }
        }
    }
    
    if (DidReturn) {
        Scope.Clear();
    } else {
        auto Entries = Scope.GetEntriesSinceMarker(M);
        for (auto &E : Entries) {
            Scope.Remove(std::get<0>(E));
        }
    }
    
    // if statements are implemented as phi nodes, which means that we need every branch to return _something_
    return llvm::ConstantInt::get(i8, 0);
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::ReturnStmt> ReturnStmt) {
    auto F = Builder.GetInsertBlock()->getParent();
    
    if (auto Expr = ReturnStmt->Expression) {
        auto V = Codegen(Expr);
        if (!TypecheckAndApplyTrivialCastIfPossible(&V, F->getReturnType())) {
            llvm::outs() << "Error: Can't return value of type '" << V->getType() << "' from function returning '" << F->getReturnType() << "'\n";
            throw;
        }
        return Builder.CreateRet(V);
    } else {
        precondition(F->getReturnType() == Void);
        return Builder.CreateRetVoid();
    }
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Assignment> Assignment) {
    // TODO should assignments return something?
    
    if (auto Ident = std::dynamic_pointer_cast<ast::Identifier>(Assignment->Target)) {
        auto Binding = Scope.GetBinding(Ident->Value);
        if (!Binding) throw;
        
        Binding->Write(Codegen(Assignment->Value));
        return nullptr;
    }
    
    if (auto MemberAccess = std::dynamic_pointer_cast<ast::MemberAccess>(Assignment->Target)) {
        // In this case, the member access needs to be an lvalue, and should return an address, instead of a dereferenced
        auto Dest = Codegen(MemberAccess, CodegenReturnValueKind::Address);
        auto Value = Codegen(Assignment->Value);
        
        precondition(Dest->getType()->isPointerTy());
        
        if (!TypecheckAndApplyTrivialCastIfPossible(&Value, Dest->getType()->getPointerElementType())) {
            llvm::outs() << "Unable to store value of type " << Value->getType() << " into address of type " << Dest->getType() << "\n";
            throw;
        }
        
        Builder.CreateStore(Value, Dest);
        return nullptr;
    }
    
    throw;
}








#pragma mark - Expressions


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::NumberLiteral> Number) {
    return llvm::ConstantInt::get(i64, Number->Value);
}



llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::StringLiteral> StringLiteral) {
    return Builder.CreateGlobalStringPtr(StringLiteral->Value);
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::CharLiteral> CharLiteral) {
    return llvm::ConstantInt::get(i8, CharLiteral->Value);
}


// If TakeAddress is true, this returns a pointer to the identifier, instead of the value stored
llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Identifier> Ident, CodegenReturnValueKind ReturnValueKind) {
    if (auto Binding = Scope.GetBinding(Ident->Value)) {
        switch (ReturnValueKind) {
            case CodegenReturnValueKind::Value:
                return Binding->Read();
            case CodegenReturnValueKind::Address:
                return const_cast<llvm::Value *>(Binding->Value);
        }
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
    
    if (Cast->ForceBitcast) {
        precondition(M->getDataLayout().getTypeSizeInBits(SrcType) == M->getDataLayout().getTypeSizeInBits(DestType));
        CastOp = llvm::Instruction::CastOps::BitCast;
    
    } else if (SrcType->isIntegerTy() && DestType->isIntegerTy()) {
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
    } else if (SrcType->isPointerTy() && DestType->isPointerTy()) {
        CastOp = llvm::Instruction::CastOps::BitCast;
    } else {
        llvm::outs() << "Unable to cast " << SrcType << " to " << DestType << "\n";
        throw;
    }
    
    return Builder.CreateCast(CastOp, V, DestType);
}




llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::MemberAccess> MemberAccess, CodegenReturnValueKind ReturnValueKind) {
    using MK = ast::MemberAccess::Member::MemberKind;
    
    llvm::Value *V = nullptr;
    std::vector<llvm::Value *> GEPIndices;
    llvm::Type *CurrentType = nullptr;
    
    auto HandleGEPIfNecessary = [&]() {
        precondition(V);
        if (!GEPIndices.empty()) {
            V = Builder.CreateGEP(V, GEPIndices);
        }
    };
    
    
    
    for (auto It = MemberAccess->Members.begin(); It != MemberAccess->Members.end(); It++) {
        auto &Member = *It;
        
        CodegenReturnValueKind RetvalKind;
        if (It + 1 != MemberAccess->Members.end()) {
            auto K = (*(It + 1))->Kind;
            if (K == MK::OffsetRead) {
                RetvalKind = CodegenReturnValueKind::Value;
            } else {
                RetvalKind = ReturnValueKind;
            }
        }
        
        switch (Member->Kind) {
            case MK::Initial_Identifier: {
                auto Ident = Member->Data.Ident;
                V = Codegen(Ident, RetvalKind);
                
                CurrentType = Scope.GetType(Ident->Value);
                break;
            }
            case MK::Initial_FunctionCall:
                V = Codegen(Member->Data.Call); // requested ReturnValueKind ignored here !?
                // TODO somehow find out here whether the return value is unused? if this isn't the last member, we know for a fact that it isn't
                CurrentType = V->getType();
                precondition(CurrentType);
                break;
            
            case MK::OffsetRead: {
                // TODO special handling if V isn't a pointer
                precondition(V && V->getType()->isPointerTy());
                
                auto Offset = Codegen(Member->Data.Offset);
                GEPIndices.push_back(Offset);
                
                CurrentType = V->getType()->getPointerElementType();
                break;
            }
                
            case MK::MemberAttributeRead: {
                precondition(CurrentType && CurrentType->isPointerTy() && CurrentType->getPointerElementType()->isStructTy());
                auto MemberName = Member->Data.Ident->Value;
                auto StructName = CurrentType->getPointerElementType()->getStructName();
                auto StructType = TypeCache.Get(StructName);
                
                auto Offset = -1;
                for (auto &Attr : StructType->Attributes) {
                    Offset += 1;
                    if (Attr->Name->Value == MemberName) {
                        CurrentType = GetLLVMType(Attr->Type);
                        break;
                    }
                }
                
                if (Offset == -1) {
                    std::cout << "Unable to find member '" << MemberName << "' in Struct '" << StructName.str() << "'\n";
                    throw;
                }
                
                GEPIndices.push_back(llvm::ConstantInt::get(i64, 0)); // Dereference the pointer
                GEPIndices.push_back(llvm::ConstantInt::get(i32, Offset));
                break;
            }
                
            case MK::MemberFunctionCall: {
                precondition(CurrentType && CurrentType->isPointerTy() && CurrentType->getPointerElementType()->isStructTy());
                auto Typename = CurrentType->getPointerElementType()->getStructName().str();
                auto Call = Member->Data.Call;
                auto MethodName = std::dynamic_pointer_cast<ast::Identifier>(Call->Target)->Value;
                auto MangledName = mangling::MangleMethod(Typename, MethodName, mangling::MethodKind::Instance);
                
                auto F = M->getFunction(MangledName);
                if (!F) {
                    std::cout << "Error: Unable to find method -[" << Typename << " " << MethodName << "] (mangled: " << MangledName << ")" << std::endl;
                    throw;
                }
                
                CurrentType = F->getReturnType();
                
                HandleGEPIfNecessary();
                
                // Call the method, inserting the additional first parameter
                Call->Target = std::make_shared<ast::Identifier>(MangledName);
                auto CallInst = llvm::dyn_cast<llvm::CallInst>(Codegen(Call, 1));
                CallInst->setArgOperand(0, V);
                V = CallInst;
                break;
            }
        }
    }
    
    HandleGEPIfNecessary();
    
    switch (ReturnValueKind) {
        case CodegenReturnValueKind::Value:
            if (V && llvm::isa<llvm::GetElementPtrInst>(V)) {
                return Builder.CreateLoad(V);
            } else {
                return V;
            }
        case CodegenReturnValueKind::Address:
            return V;
    }
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



// ArgumentOffset: indicates the number of skipped arguments at the start of the argumens list
llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::FunctionCall> Call, unsigned ArgumentOffset) {
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
    
    std::vector<llvm::Value *> Args(ArgumentOffset, nullptr);
    
    precondition(Call->Arguments.size() == FT->getNumParams() - ArgumentOffset);
    
    for (auto I = ArgumentOffset; I < FT->getNumParams(); I++) { // NOTE: getNumParams only returns *fixed* parameters
        auto ExpectedType = FT->getParamType(I);
        auto V = Codegen(Call->Arguments[I]);
        if (!TypecheckAndApplyTrivialCastIfPossible(&V, ExpectedType)) {
            llvm::outs() << "Type mismatch: Cannot pass expression of type " << V->getType() << " to function '" << F->getName() << "' expecting " << ExpectedType << "\n";
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
        case Operation::EQ: return Predicate::ICMP_EQ;
        case Operation::NE: return Predicate::ICMP_NE;
        case Operation::LT: return Predicate::ICMP_SLT; // TODO differentiate between signed and unsigned
        case Operation::LE: return Predicate::ICMP_SLE;
        case Operation::GT: return Predicate::ICMP_SGT;
        case Operation::GE: return Predicate::ICMP_SGE;
    }
}



llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Comparison> Comparison) {
    auto LHS = Codegen(Comparison->LHS);
    auto RHS = Codegen(Comparison->RHS);
    
    precondition(LHS->getType()->isIntegerTy());
    
    // TODO signed/unsigned types
    //assert_implication(IsSignedType(LHS->getType()), IsSignedType(RHS->getType()));
    
    if (!Binop_AttemptToResolvePotentialIntTypeMismatchesByCastingNumberLiteralsIfPossible(&LHS, &RHS)) {
        llvm::outs() << "Type mismatch: Unable to compare incompatible types '" << LHS->getType() << "' and '" << RHS->getType() << "'\n";
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
    
    
    std::vector<llvm::BasicBlock *> BranchBodyBlocks;
    for (auto I = 0; I < If->Branches.size(); I++) {
        auto Name = llvm::Twine(F->getName()).concat("_if_body").concat(llvm::Twine(I));
        BranchBodyBlocks.push_back(llvm::BasicBlock::Create(C, Name));
    }
    
    
    
    // The entry points to each branch's condition
    // Note that if the last branch is a conditionless else branch, this points directly to the branch body
    std::vector<llvm::BasicBlock *> BranchConditionBlocks;
    
    for (auto I = 0; I < If->Branches.size(); I++) {
        if (If->Branches[I]->Kind == ast::IfStmt::Branch::BranchKind::Else) break;
        auto Name = llvm::Twine(F->getName()).concat("_if_cond_").concat(llvm::Twine(I));
        BranchConditionBlocks.push_back(llvm::BasicBlock::Create(C, Name));
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
    
    std::vector<llvm::Value *> BranchValues;
    auto MergeBB = llvm::BasicBlock::Create(C, "merge");
    
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
    
    static std::map<std::string, llvm::Type *> StructTypeMappings;
    
    switch (TI->Kind) {
        case TypeInfo::Kind::Primitive: {
            HANDLE(i8)
            HANDLE(i16)
            HANDLE(i32)
            HANDLE(i64)
            HANDLE(Bool)
            HANDLE(Double)
            HANDLE(Void)
            throw;
        }
        
        case TypeInfo::Kind::Pointer: {
            auto num_indirections = 0;
            while (TI->Kind == TypeInfo::Kind::Pointer) {
                num_indirections += 1;
                TI = TI->Pointee();
            }
            auto Type = GetLLVMType(TI);
            
            while (num_indirections--) {
                Type = Type->getPointerTo();
            }
            return Type;
        }
        
        case TypeInfo::Kind::Complex: {
            auto Name = TI->Data.Name;
            if (auto Type = StructTypeMappings[Name]) {
                return Type;
            }
            
            auto LLVMType = llvm::StructType::create(C, Name);
            
            std::vector<llvm::Type *> Types;
            for (auto &Attr : TypeCache.Get(Name)->Attributes) {
                Types.push_back(GetLLVMType(Attr->Type));
            }
            
            LLVMType->setBody(Types);
            StructTypeMappings[Name] = LLVMType->getPointerTo();
            return LLVMType->getPointerTo();
            
        }
        
        case TypeInfo::Kind::Function:
            throw;
    }
    
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
        llvm::outs() << "Unable to compare incompatible types " << T_LHS << " and " << T_RHS << "\n" ;
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



namespace astgen {
    using namespace ast;
    
    std::shared_ptr<Identifier> Ident(std::string Value) {
        return std::make_shared<Identifier>(Value);
    }
    
    std::vector<std::shared_ptr<Expr>> ExprVec(std::initializer_list<std::shared_ptr<Expr>> E) {
        return E;
    }
    
    std::shared_ptr<NumberLiteral> Number(uint64_t Value) {
        return std::make_shared<NumberLiteral>(Value);
    }
    
    std::shared_ptr<FunctionCall> Call(std::shared_ptr<Identifier> Target, std::vector<std::shared_ptr<Expr>> Args, bool UnusedReturnValue) {
        return std::make_shared<FunctionCall>(Target, Args, UnusedReturnValue);
    }
    
    std::shared_ptr<Assignment> Assign(std::shared_ptr<Expr> Target, std::shared_ptr<Expr> Value) {
        return std::make_shared<Assignment>(Target, Value);
    }
    
    std::shared_ptr<Typecast> Cast(std::shared_ptr<Expr> Expr, TypeInfo *T) {
        return std::make_shared<Typecast>(Expr, T);
    }
}



#pragma mark - Synthesized Functions

llvm::Value *IRGenerator::GenerateStructInitializer(std::shared_ptr<ast::StructDecl> Struct) {
    auto Typename = Struct->Name->Value;
    
    auto T = TypeInfo::MakeComplex(Typename);
    auto F = std::make_shared<ast::FunctionDecl>();
    F->Name = "init";
    F->Kind = ast::FunctionSignature::FunctionKind::StaticMethod;
    F->Parameters = Struct->Attributes;
    F->ReturnType = T;
    F->Body = std::make_shared<ast::Composite>();
    
    auto self = astgen::Ident("self");
    
    {
        // Allocate Self
        F->Body->Statements.push_back(std::make_shared<ast::VariableDecl>(self, T));
        
        auto Malloc = astgen::Call(astgen::Ident("malloc"),
                                   astgen::ExprVec({astgen::Number(M->getDataLayout().getTypeAllocSize(GetLLVMType(T)))}),
                                   false);
        
        auto X = std::make_shared<ast::Typecast>(Malloc, T);
        
        F->Body->Statements.push_back(astgen::Assign(self, X));
        
    }
    
    
    // Set Attributes
    auto Idx = 0;
    for (auto &Attr : Struct->Attributes) {
        auto M1 = std::make_shared<ast::MemberAccess::Member>(ast::MemberAccess::Member::MemberKind::Initial_Identifier);
        M1->Data.Ident = self;
        
        auto M2 = std::make_shared<ast::MemberAccess::Member>(ast::MemberAccess::Member::MemberKind::OffsetRead);
        M2->Data.Offset = astgen::Number(0);
        
        auto M3 = std::make_shared<ast::MemberAccess::Member>(ast::MemberAccess::Member::MemberKind::OffsetRead);
        M3->Data.Offset = astgen::Cast(astgen::Number(Idx++), TypeInfo::i32);
        
        auto MemberAccess = std::make_shared<ast::MemberAccess>(std::vector<std::shared_ptr<ast::MemberAccess::Member>>({M1, M2, M3}));
        F->Body->Statements.push_back(astgen::Assign(MemberAccess, Attr->Name));
    }
    
    
    // Return
    F->Body->Statements.push_back(std::make_shared<ast::ReturnStmt>(self));
    
    RegisterFunctionSignature(F, true, Typename);
    return Codegen(F, true, Typename);
}
