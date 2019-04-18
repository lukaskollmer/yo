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
#include "util_llvm.h"

using namespace irgen;
using namespace util_llvm;


inline constexpr unsigned kInstanceMethodCallArgumentOffset = 1;


// ast utils

std::ostream& operator<<(std::ostream &OS, const std::shared_ptr<ast::FunctionSignature> &Signature) {
    OS << "fn " << Signature->Name << "(";
    for (auto It = Signature->Parameters.begin(); It != Signature->Parameters.end(); It++) {
        OS << (*It)->Type->Str();
        if (It + 1 != Signature->Parameters.end()) {
            OS << ", ";
        }
    }
    OS << "): " << Signature->ReturnType->Str();
    return OS;
}



// IRGen


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
    
    VerifyFunctionDeclarations();
    
    for (auto &Node : Ast) {
        Codegen(Node);
    }
}



bool is_variadic(std::shared_ptr<ast::Node> Node) {
    return Node->HasAnnotation("variadic");
}


template <typename T>
std::string MangleFullyResolved(T Function) {
    if (Function->HasAnnotation("no_mangle")) {
        return Function->Signature->Name;
    }
    return mangling::MangleFullyResolvedNameForSignature(Function->Signature);
}

void IRGenerator::Preflight(ast::AST &Ast) {
    for (auto &Node : Ast) {
        if (auto FunctionDecl = std::dynamic_pointer_cast<ast::FunctionDecl>(Node)) {
            RegisterFunction(FunctionDecl);
        
        } else if (auto EFD = std::dynamic_pointer_cast<ast::ExternFunctionDecl>(Node)) {
            // No potential conflict w/ other non-external already resolved functions, since their resolved name will be mangled (unlike the external function's name)
            if (auto EFS2Sig = GetResolvedFunctionWithName(EFD->Signature->Name)) {
                precondition(EFD->Signature->ReturnType->Equals(EFS2Sig->ReturnType)
                             && EFD->Signature->Parameters.size() == EFS2Sig->Parameters.size()
                             && std::equal(EFD->Signature->Parameters.begin(), EFD->Signature->Parameters.end(), EFS2Sig->Parameters.begin(),
                                           [] (auto Arg1, auto Arg2) -> bool {
                                               return Arg1->Type->Equals(Arg2->Type);
                                           })
                             && "Multiple external function declarations w/ same name, but different signatures"
                             );
                continue;
            }
            RegisterFunction(EFD);
        
        } else if (auto StructDecl = std::dynamic_pointer_cast<ast::StructDecl>(Node)) {
            RegisterStructDecl(StructDecl);
        
        } else if (auto ImplBlock = std::dynamic_pointer_cast<ast::ImplBlock>(Node)) {
            RegisterImplBlock(ImplBlock);
        }
    }
}


void IRGenerator::RegisterFunction(std::shared_ptr<ast::FunctionDecl> Function) {
    precondition(!Function->Signature->IsTemplateFunction);
    
    auto Signature = Function->Signature;
    std::vector<llvm::Type *> ParameterTypes;
    
    for (auto &P : Signature->Parameters) {
        ParameterTypes.push_back(GetLLVMType(P->Type));
    }
    
    std::string CanonicalName = mangling::MangleCanonicalNameForSignature(Signature);
    std::string ResolvedName = MangleFullyResolved(Function);
    
    
    precondition(M->getFunction(ResolvedName) == nullptr);
    
    auto FT = llvm::FunctionType::get(GetLLVMType(Signature->ReturnType), ParameterTypes, is_variadic(Function));
    auto F = llvm::Function::Create(FT, llvm::Function::LinkageTypes::ExternalLinkage, ResolvedName, M);
    
    unsigned Idx = 0;
    for (auto &Arg : F->args()) {
        Arg.setName(Signature->Parameters[Idx++]->Name->Value);
    }
    
    ResolvedFunctions[ResolvedName] = Function->Signature;
    Functions[CanonicalName].push_back(FunctionResolutionInfo(Function, F));
}


void IRGenerator::RegisterFunction(std::shared_ptr<ast::ExternFunctionDecl> Function) {
    auto Signature = Function->Signature;
    std::vector<llvm::Type *> ParameterTypes;
    
    for (auto &P : Signature->Parameters) {
        ParameterTypes.push_back(GetLLVMType(P->Type));
    }
    
    precondition(M->getFunction(Signature->Name) == nullptr);
    auto FT = llvm::FunctionType::get(GetLLVMType(Signature->ReturnType), ParameterTypes, is_variadic(Function));
    auto F = llvm::Function::Create(FT, llvm::Function::LinkageTypes::ExternalLinkage, Signature->Name, M);
    
    // we can't really append this to the `Functions` vector since that stores an ast::FunctionDecl, but external functions don't have a body :/
    // TODO: come up w/ a solution!
    //ExternalFunctions[Signature->Name] = Signature;
    ResolvedFunctions[Signature->Name] = Signature;
    
    auto Decl = std::make_shared<ast::FunctionDecl>();
    Decl->Signature = Function->Signature;
    Functions[Signature->Name].push_back(FunctionResolutionInfo(Decl, F));
}



std::shared_ptr<ast::FunctionSignature> IRGenerator::GetResolvedFunctionWithName(std::string &Name) {
    if (auto It = ResolvedFunctions.find(Name); It != ResolvedFunctions.end()) {
        return It->second;
    }
    return nullptr;
}


void IRGenerator::RegisterStructDecl(std::shared_ptr<ast::StructDecl> Struct) {
    TypeCache.Insert(Struct->Name->Value, Struct);
}



void IRGenerator::RegisterImplBlock(std::shared_ptr<ast::ImplBlock> ImplBlock) {
    using FK = ast::FunctionSignature::FunctionKind;
    
    auto Typename = ImplBlock->Typename;
    precondition(TypeCache.Contains(Typename));
    
    auto T = TypeInfo::MakeComplex(Typename);
    
    for (auto &F : ImplBlock->Methods) {
        auto Kind = FK::StaticMethod;
        if (!F->Signature->Parameters.empty()) {
            auto First = F->Signature->Parameters[0];
            if (First->Name->Value == "self" && First->Type->Equals(T)) {
                Kind = FK::InstanceMethod;
            }
        }
        F->Signature->Kind = Kind;
        F->Signature->ImplType = TypeCache.Get(Typename);
        
        RegisterFunction(F);
    }
}





#pragma mark - FunctionDecl Verification

// Make sure there's no ambiguity
void IRGenerator::VerifyFunctionDeclarations() {
    // TODO
    // make sure all function signatures are unique
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
    HANDLE(LocalStmt, WhileStmt)
    
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

llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::FunctionDecl> FunctionDecl) {
    precondition(Scope.IsEmpty());
    
    auto ResolvedName = MangleFullyResolved(FunctionDecl);
    
    //auto Name = !Options.ShouldMangleName ? FunctionDecl->Name : MangleFunctionName(FunctionDecl, Options.Typename);
    
    auto F = M->getFunction(ResolvedName);
    if (!F) {
        LKFatalError("Unable to find function '%s'", ResolvedName.c_str());
    }
    
    for (auto I = 0; I < FunctionDecl->Signature->Parameters.size(); I++) {
        auto &Arg = F->arg_begin()[I];
        
        auto Binding = ValueBinding(&Arg, [&Arg]() {
            return &Arg;
        }, [=, &Arg](llvm::Value *V) {
            LKFatalError("Function arguments are read-only (%s in %s)", Arg.getName().str().c_str(), ResolvedName.c_str());
        });
        
        auto Param = FunctionDecl->Signature->Parameters[I];
        Scope.Insert(Param->Name->Value, Param->Type, Binding);
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
        Codegen(M);
    }
    return nullptr;
}




#pragma mark - Local Statements


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::VariableDecl> Decl) {
    TypeInfo *Type = TypeInfo::Unresolved;
    bool HasInferredType = false;
    
    if ((Type = Decl->Type) == TypeInfo::Unresolved) {
        // If no type is specified, there _has_ to be an initial value
        precondition(Decl->InitialValue);
        Type = GuessType(Decl->InitialValue);
        HasInferredType = true;
    }
    
    precondition(Type);
    auto Alloca = Builder.CreateAlloca(GetLLVMType(Type));
    Alloca->setName(Decl->Name->Value);
    
    auto Binding = ValueBinding(Alloca, [=] () {
        return Builder.CreateLoad(Alloca);
    }, [=] (llvm::Value *V) {
        precondition(V->getType() == Alloca->getType()->getPointerElementType());
        Builder.CreateStore(V, Alloca);
    });
    
    Scope.Insert(Decl->Name->Value, Type, Binding);
    
    if (auto Expr = Decl->InitialValue) {
        if (!HasInferredType) {
            precondition(GuessType(Expr)->Equals(Type));
        }
        Binding.Write(Codegen(Expr));
    }
    
    return Alloca;
}







llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::Composite> Composite, bool IsFunctionBody) {
    auto M = Scope.GetMarker();
    
    bool DidReturn = false;
    
    if (IsFunctionBody && Composite->Statements.empty()) {
        // Empty body -> no return statement
        auto F = Builder.GetInsertBlock()->getParent();
        if (F->getReturnType() == Void) {
            Builder.CreateRetVoid();
            DidReturn = true;
        } else {
            LKFatalError("Missing return statement in function '%s' w/ return type '%s'", F->getName().str().c_str(), util_llvm::to_string(F->getReturnType()).c_str());
        }
    }
    
    for (auto It = Composite->Statements.begin(); !DidReturn && It != Composite->Statements.end(); It++) {
        auto &Stmt = *It;
        if (auto ReturnStmt = std::dynamic_pointer_cast<ast::ReturnStmt>(Stmt)) {
            Codegen(ReturnStmt);
            DidReturn = true;
        } else {
            Codegen(Stmt);
            if (IsFunctionBody && It + 1 == Composite->Statements.end()) {
                // TODO this doesn't seem to work?
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
    auto FName = Builder.GetInsertBlock()->getParent()->getName().str();
    auto F = ResolvedFunctions[FName];
    
    if (auto Expr = ReturnStmt->Expression) {
        TypeInfo *T;
        if (!TypecheckAndApplyTrivialCastIfPossible(&Expr, F->ReturnType, &T)) {
            LKFatalError("Error: Can't return value of type '%s' from function '%s' returning '%s'", T->Str().c_str(), FName.c_str(), F->ReturnType->Str().c_str());
        }
        
        return Builder.CreateRet(Codegen(Expr));
    }
    
    precondition(F->ReturnType->Equals(TypeInfo::Void));
    return Builder.CreateRetVoid();
}



bool IntegerLiteralFitsInType(uint64_t Value, TypeInfo *Type) {
    assert_implication(static_cast<int64_t>(Value) < 0, Type->IsSigned());
    
    uint64_t MaxValue;
    
    if (!Type->IsSigned()) { // unsigned
        if (Type->Equals(TypeInfo::u8)) MaxValue = UINT8_MAX;
        else if (Type->Equals(TypeInfo::u16)) MaxValue = UINT16_MAX;
        else if (Type->Equals(TypeInfo::u32)) MaxValue = UINT32_MAX;
        else if (Type->Equals(TypeInfo::u64)) MaxValue = UINT64_MAX;
        else LKFatalError("should never reach here");
    } else { // signed
        if (Type->Equals(TypeInfo::i8)) MaxValue = INT8_MAX;
        else if (Type->Equals(TypeInfo::i16)) MaxValue = INT16_MAX;
        else if (Type->Equals(TypeInfo::i32)) MaxValue = INT32_MAX;
        else if (Type->Equals(TypeInfo::i64)) MaxValue = INT64_MAX;
        else LKFatalError("should never reach here");
    }
    
    return Value <= MaxValue;
}


bool IRGenerator::TypecheckAndApplyTrivialCastIfPossible(std::shared_ptr<ast::Expr> *Expr, TypeInfo *ExpectedType, TypeInfo **InitialTypeOfExpr) {
    auto Type = GuessType(*Expr);
    if (InitialTypeOfExpr) *InitialTypeOfExpr = Type;
    
    if (Type->Equals(ExpectedType)) return true;
    
    // at this point, both are integers
    if (auto NumberLiteral = std::dynamic_pointer_cast<ast::NumberLiteral>(*Expr)) {
        precondition(ExpectedType->IsIntegerType());
        precondition(IntegerLiteralFitsInType(NumberLiteral->Value, ExpectedType));
        
        *Expr = std::make_shared<ast::Typecast>(*Expr, ExpectedType, ast::Typecast::CastKind::StaticCast);
        return true;
    }
    
    std::cout << "input: " << Type->Str() << ", expected: " << ExpectedType->Str() << std::endl;
    throw;
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
        precondition(Dest->getType()->isPointerTy());
        auto DestTy = GuessType(Assignment->Target);
        
        auto Expr = Assignment->Value;
        TypeInfo *T;
        if (!TypecheckAndApplyTrivialCastIfPossible(&Expr, DestTy, &T)) {
            LKFatalError("unable to store value of type '%s' into '%s'", T->Str().c_str(), DestTy->Str().c_str());
        }
        
        Builder.CreateStore(Codegen(Expr), Dest);
        return nullptr;
    }
    
    throw;
}








#pragma mark - Expressions


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::NumberLiteral> Number) {
    return llvm::ConstantInt::get(i64, Number->Value);
}



llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::StringLiteral> StringLiteral) {
    using SLK = ast::StringLiteral::StringLiteralKind;
    
    switch (StringLiteral->Kind) {
        case SLK::ByteString:
            return Builder.CreateGlobalStringPtr(StringLiteral->Value);
        case SLK::NormalString: {
            precondition(TypeCache.Contains("String"));
            StringLiteral->Kind = SLK::ByteString;
            auto Target = std::make_shared<ast::Identifier>(mangling::MangleCanonicalName("String", "new", ast::FunctionSignature::FunctionKind::StaticMethod));
            auto Call = std::make_shared<ast::FunctionCall>(Target, std::vector<std::shared_ptr<ast::Expr>>(1, StringLiteral), false);
            return Codegen(Call);
        }
    }
}


llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::CharLiteral> CharLiteral) {
    precondition(IntegerLiteralFitsInType(CharLiteral->Value, TypeInfo::i8));
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
    auto SrcTy = GuessType(Cast->Expression);
    auto DestTy = Cast->DestType;
    
    if (SrcTy->Equals(DestTy)) {
        return Codegen(Cast->Expression);
    }
    
    
    llvm::Instruction::CastOps Op;
    
    switch (Cast->Kind) {
        case ast::Typecast::CastKind::Bitcast: {
            precondition(M->getDataLayout().getTypeSizeInBits(GetLLVMType(SrcTy)) == M->getDataLayout().getTypeSizeInBits(GetLLVMType(DestTy)));
            if (SrcTy->IsPointer() && DestTy->IsIntegerType()) {
                Op = llvm::Instruction::CastOps::PtrToInt;
            } else if (SrcTy->IsIntegerType() && DestTy->IsPointer()) {
                Op = llvm::Instruction::CastOps::IntToPtr;
            } else {
                Op = llvm::Instruction::CastOps::BitCast;
            }
            break;
        }
        
        case ast::Typecast::CastKind::StaticCast: {
            if (SrcTy->IsIntegerType() && DestTy->IsIntegerType()) {
                auto SrcIntWidth  = GetLLVMType(SrcTy)->getIntegerBitWidth();
                auto DestIntWidth = GetLLVMType(DestTy)->getIntegerBitWidth();
                
                if (SrcIntWidth > DestIntWidth) {
                    // casting to a smaller type
                    Op = llvm::Instruction::CastOps::Trunc;
                } else {
                    // casting to a larger type
                    if (SrcTy->IsSigned()) {
                        Op = llvm::Instruction::CastOps::SExt;
                    } else {
                        Op = llvm::Instruction::CastOps::ZExt;
                    }
                }
                break;
            }
            throw;
        }
    }
    
    return Builder.CreateCast(Op, Codegen(Cast->Expression), GetLLVMType(DestTy));
}




llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::MemberAccess> MemberAccess, CodegenReturnValueKind ReturnValueKind) {
    using MK = ast::MemberAccess::Member::MemberKind;
    
    // TODO: This is almost definitely wrong
    
    llvm::Value *V = nullptr;
    TypeInfo *CurrentType = nullptr;
    
    for (auto &Member : MemberAccess->Members) {
        switch (Member->Kind) {
            case MK::Initial_Identifier: {
                auto Ident = Member->Data.Ident;
                V = Codegen(Ident);
                CurrentType = Scope.GetType(Ident->Value);
                break;
            }
            
            case MK::Initial_StaticCall: {
                std::shared_ptr<ast::FunctionSignature> SelectedOverload;
                V = Codegen(Member->Data.Call, 0, &SelectedOverload);
                CurrentType = SelectedOverload->ReturnType;
                
                break;
            }
            
            case MK::Initial_FunctionCall: {
                std::shared_ptr<ast::FunctionSignature> Signature;
                V = Codegen(Member->Data.Call, 0, &Signature);
                CurrentType = Signature->ReturnType;
                break;
            }
            
            case MK::OffsetRead: {
                precondition(CurrentType->IsPointer());
                
                if (llvm::isa<llvm::GetElementPtrInst>(V)) {
                    V = Builder.CreateLoad(V);
                }
                
                // We need V to be a pointer
                precondition(V->getType()->isPointerTy());
                V = Builder.CreateGEP(V, Codegen(Member->Data.Offset));
                CurrentType = CurrentType->Pointee();
                break;
            }
            
            case MK::MemberFunctionCall: {
                precondition(CurrentType->IsComplex());

                auto Call = Member->Data.Call;
                precondition(Call->Target->Value[0] == '-'); // The name should already be mangled
                
                std::shared_ptr<ast::FunctionSignature> SelectedOverload;
                
                // Call the function, inserting the implicit first argument
                auto CallInst = llvm::dyn_cast<llvm::CallInst>(Codegen(Call, kInstanceMethodCallArgumentOffset, &SelectedOverload));
                CallInst->setOperand(0, V);
                V = CallInst;
                
                CurrentType = SelectedOverload->ReturnType;
                break;
            }
            
            case MK::MemberAttributeRead: {
                precondition(CurrentType->IsComplex() && TypeCache.Contains(CurrentType->Data.Name));
                auto MemberName = Member->Data.Ident->Value;
                auto StructName = CurrentType->Data.Name;
                auto StructType = TypeCache.Get(StructName);
                
                uint32_t Offset = 0;
                auto DidFindMember = false;
                for (auto &Attr : StructType->Attributes) {
                    if (Attr->Name->Value == MemberName) {
                        DidFindMember = true;
                        break;
                    }
                    Offset++;
                }
                
                if (!DidFindMember) {
                    std::cout << "Unable to find member '" << MemberName << "' in struct '" << StructName << std::endl;
                    throw;
                }
                CurrentType = StructType->Attributes[Offset]->Type;
                
                llvm::Value *Offsets[] = {
                    llvm::ConstantInt::get(i64, 0),
                    llvm::ConstantInt::get(i32, Offset)
                };
                
                if (llvm::isa<llvm::GetElementPtrInst>(V)) {
                    V = Builder.CreateLoad(V);
                }
        
                V = Builder.CreateGEP(V, Offsets);
                break;
            }
        }
    }
    
    
    switch (ReturnValueKind) {
        case CodegenReturnValueKind::Address:
            return V;
        case CodegenReturnValueKind::Value:
            if (V && llvm::isa<llvm::GetElementPtrInst>(V)) {
                return Builder.CreateLoad(V);
            } else {
                return V;
            }
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


// Returns the function most closely matching the call
FunctionResolutionInfo IRGenerator::ResolveCall(std::shared_ptr<ast::FunctionCall> Call, unsigned ArgumentOffset) {
    auto &PossibleTargets = Functions[Call->Target->Value];
    precondition(!PossibleTargets.empty());
    
    if (PossibleTargets.size() == 1) {
        return PossibleTargets[0];
    }
    
    // Skip function resolution if the call has no parameters
    
    if (Call->Arguments.empty()) {
        for (auto &Target : PossibleTargets) {
            if (Target.Decl->Signature->Parameters.empty()) {
                throw;
            }
        }
        LKFatalError("unable to resolve function call");
    }
    
    
    std::vector<std::pair<uint32_t, FunctionResolutionInfo>> Matches;
    
    for (auto &Target : PossibleTargets) {
        auto Signature = Target.Decl->Signature;
        
        if (Signature->Parameters.size() != Call->Arguments.size()) {
            Matches.push_back({0, Target});
            continue;
        }
        
        uint32_t Score = 0;
        
        for (auto I = 0; I < Call->Arguments.size(); I++) {
            auto Arg = Call->Arguments[I];
            auto Type_Passed = GuessType(Arg);
            auto Type_Expected = Target.Decl->Signature->Parameters[I]->Type;
            
            if (Type_Passed->Equals(Type_Expected)) {
                Score += 10;
            } else if (auto NumberLiteral = std::dynamic_pointer_cast<ast::NumberLiteral>(Arg)) {
                precondition(NumberLiteral->Type == ast::NumberLiteral::NumberType::Integer);
                if (ValueIsTriviallyConvertibleTo(NumberLiteral, Type_Expected)) {
                    Score += 5;
                }
            }
        }
        
        Matches.push_back({Score, Target});
    }
    
    std::sort(Matches.begin(), Matches.end(), [](auto Arg0, auto Arg1) { return Arg0.first > Arg1.first; });
    
    std::cout << "Matching overloads:\n";
    for (auto &[Score, Info] : Matches) {
        std::cout << "- " << Score << ": " << Info.Decl->Signature << std::endl;
    }
    
    auto BestMatch = Matches.front().second;
    precondition(!BestMatch.Decl->Signature->IsTemplateFunction);
    return BestMatch;
}







// ArgumentOffset: indicates the number of skipped arguments at the start of the argumens list
llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::FunctionCall> Call, unsigned ArgumentOffset, std::shared_ptr<ast::FunctionSignature> *SelectedOverload) {
    llvm::Function *F;
    
    auto TargetName = Call->Target->Value;
    std::cout << "CallTarget: " << TargetName << std::endl;
    
    auto ResolvedCall = ResolveCall(Call, ArgumentOffset);
    F = ResolvedCall.LLVMFunction;
    if (SelectedOverload) {
        *SelectedOverload = ResolvedCall.Decl->Signature;
    }
    //F = ResolveCall(Call, ArgumentOffset);
    
    if (!F) {
        F = M->getFunction(TargetName);
    }
    if (!F) {
        LKFatalError("Unable to find function named '%s'", TargetName.c_str());
    }
    
    auto FT = F->getFunctionType();
    auto IsVariadic = FT->isVarArg();
    
    precondition(Call->Arguments.size() >= FT->getNumParams() - ArgumentOffset - IsVariadic);
    
    
    std::vector<llvm::Value *> Args(ArgumentOffset, nullptr);
    
    auto NumFixedArgs = FT->getNumParams() - ArgumentOffset;

    
    for (auto I = ArgumentOffset; I < NumFixedArgs; I++) {
        auto ExpectedType = ResolvedCall.Decl->Signature->Parameters[I]->Type;
        
        auto Expr = Call->Arguments[I];
        TypeInfo *T;
        if (!TypecheckAndApplyTrivialCastIfPossible(&Expr, ExpectedType, &T)) {
            LKFatalError("Type mismatch in call to '%s'. Arg #%i: expected '%s', got '%s'", Call->Target->Value.c_str(), I, ExpectedType->Str().c_str(), T->Str().c_str());
        }
        
        Args.push_back(Codegen(Expr));
    }
    
    if (IsVariadic && GetResolvedFunctionWithName(TargetName)) { // TargetName is unmangled
        for (auto It = Call->Arguments.begin() + NumFixedArgs; It != Call->Arguments.end(); It++) {
            Args.push_back(Codegen(*It));
        }
    } else if (IsVariadic) {
        throw; // TODO implement
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
        auto Name = llvm::Twine(F->getName()).concat("_if_body_").concat(llvm::Twine(I));
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





llvm::Value *IRGenerator::Codegen(std::shared_ptr<ast::WhileStmt> WhileStmt) {
    auto F = Builder.GetInsertBlock()->getParent();
    
    // TODO add unique ids to the branch names!, add the current function name?
    auto CondBB = llvm::BasicBlock::Create(C, "while_cond");
    auto BodyBB = llvm::BasicBlock::Create(C, "while_body");
    auto MergeBB = llvm::BasicBlock::Create(C, "while_merge");
    
    F->getBasicBlockList().push_back(CondBB);
    Builder.CreateBr(CondBB);
    Builder.SetInsertPoint(CondBB);
    
    Builder.CreateCondBr(Codegen(WhileStmt->Condition), BodyBB, MergeBB);
    
    F->getBasicBlockList().push_back(BodyBB);
    Builder.SetInsertPoint(BodyBB);
    Codegen(WhileStmt->Body);
    Builder.CreateBr(CondBB);
    
    F->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    
    return nullptr;
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
        
        case TypeInfo::Kind::Unresolved:
            LKFatalError("unresolved type");
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


bool IRGenerator::IsIntegerType(TypeInfo *TI) {
    return TI == TypeInfo::i8 || TI == TypeInfo::i16 || TI == TypeInfo::i32 || TI == TypeInfo::i64;
}

bool IRGenerator::IsTriviallyConvertible(TypeInfo *SrcType, TypeInfo *DestType) {
    if (SrcType->Equals(DestType)) return true;
    
    if (IsIntegerType(SrcType) && IsIntegerType(DestType)) {
        throw;
    }
    
    return false;
}



bool IRGenerator::ValueIsTriviallyConvertibleTo(std::shared_ptr<ast::NumberLiteral> Number, TypeInfo *TI) {
    precondition(Number->Type == ast::NumberLiteral::NumberType::Integer && IsIntegerType(TI));
    precondition(!Number->IsSigned);
    
    auto Value = Number->Value;
    uint8_t BitCount = 0;
    while (Value != 0) { ++BitCount; Value >>= 1; }
    
    return BitCount <= TI->Size;
}

TypeInfo *IRGenerator::GuessType(std::shared_ptr<ast::Expr> Expr) {
#define IF(name, T) if (auto name = std::dynamic_pointer_cast<T>(Expr))
    
    IF(_, ast::NumberLiteral) {
        return TypeInfo::i64;
    }
    
    IF(MemberAccess, ast::MemberAccess) {
        return GuessType(MemberAccess);
    }
    
    IF(BinaryExpr, ast::BinaryOperation) {
        return GuessType(BinaryExpr->LHS); // todo should this check whether lhs and rhs have the same type?
    }
    
    IF(Ident, ast::Identifier) {
        return Scope.GetType(Ident->Value);
    }
    
    IF(Call, ast::FunctionCall) {
        return ResolveCall(Call, 0).Decl->Signature->ReturnType;
    }
    
    IF(Cast, ast::Typecast) {
        return Cast->DestType;
    }
    
    LKFatalError("Unhandled node %s", util::typeinfo::GetTypename(*Expr).c_str());
    
#undef IF
}



TypeInfo *IRGenerator::GuessType(std::shared_ptr<ast::MemberAccess> MemberAccess) {
    using MK = ast::MemberAccess::Member::MemberKind;
    TypeInfo *Type = TypeInfo::Unresolved;
    
    for (auto &Member : MemberAccess->Members) {
        switch (Member->Kind) {
            case MK::Initial_Identifier:
                Type = Scope.GetType(Member->Data.Ident->Value);
                break;
            
            case MK::Initial_FunctionCall:{
                auto R = ResolveCall(Member->Data.Call, 0);
                Type = ResolveCall(Member->Data.Call, 0).Decl->Signature->ReturnType;
                break;}
            
            case MK::Initial_StaticCall:
                Type = ResolveCall(Member->Data.Call, 0).Decl->Signature->ReturnType;
                break;
            
            case MK::OffsetRead:
                precondition(Type->IsPointer());
                Type = Type->Pointee();
                break;
            
            case MK::MemberFunctionCall: {
                std::cout << "XX:" << Member->Data.Call << std::endl;
                precondition(Type->IsComplex() && TypeCache.Contains(Type->Data.Name));
                auto Call = Member->Data.Call;
                precondition(Call->Target->Value[0] != '-'); // make sure the name is still unmangled
                Call->Target = std::make_shared<ast::Identifier>(mangling::MangleCanonicalName(Type->Data.Name, Call->Target->Value, ast::FunctionSignature::FunctionKind::InstanceMethod));
                Type = ResolveCall(Call, kInstanceMethodCallArgumentOffset).Decl->Signature->ReturnType;
                break;
            }
            
            case MK::MemberAttributeRead:
                precondition(Type->IsComplex());
                auto DidFind = false;
                for (auto &Attr : TypeCache.Get(Type->Data.Name)->Attributes) {
                    if (Attr->Name->Value == Member->Data.Ident->Value) {
                        Type = Attr->Type;
                        DidFind = true;
                        break;
                    }
                }
                precondition(DidFind);
                break;
        }
    }
    
    return Type;
}




#pragma mark - Synthesized Functions


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
        return std::make_shared<Typecast>(Expr, T, Typecast::CastKind::StaticCast);
    }
}


llvm::Value *IRGenerator::GenerateStructInitializer(std::shared_ptr<ast::StructDecl> Struct) {
    auto Typename = Struct->Name->Value;
    
    auto T = TypeInfo::MakeComplex(Typename);
    auto F = std::make_shared<ast::FunctionDecl>();
    F->Signature = std::make_shared<ast::FunctionSignature>();
    F->Signature->Name = "init";
    F->Signature->Kind = ast::FunctionSignature::FunctionKind::StaticMethod;
    F->Signature->Parameters = Struct->Attributes;
    F->Signature->ReturnType = T;
    F->Signature->ImplType = Struct;
    F->Body = std::make_shared<ast::Composite>();
    
    auto self = astgen::Ident("self");
    
    {
        // Allocate Self
        F->Body->Statements.push_back(std::make_shared<ast::VariableDecl>(self, T));
        
        auto Malloc = astgen::Call(astgen::Ident("malloc"),
                                   astgen::ExprVec({astgen::Number(M->getDataLayout().getTypeAllocSize(GetLLVMType(T)))}),
                                   false);
        
        auto X = std::make_shared<ast::Typecast>(Malloc, T, ast::Typecast::CastKind::Bitcast);
        F->Body->Statements.push_back(astgen::Assign(self, X));
        
    }
    
    // Set Attributes
    for (auto &Attr : Struct->Attributes) {
        auto M1 = std::make_shared<ast::MemberAccess::Member>(ast::MemberAccess::Member::MemberKind::Initial_Identifier, self);
        auto M2 = std::make_shared<ast::MemberAccess::Member>(ast::MemberAccess::Member::MemberKind::MemberAttributeRead, Attr->Name);
        
        auto M = std::make_shared<ast::MemberAccess>(std::vector<std::shared_ptr<ast::MemberAccess::Member>>({M1, M2}));
        F->Body->Statements.push_back(std::make_shared<ast::Assignment>(M, Attr->Name));
        
    }
    
    // Return
    F->Body->Statements.push_back(std::make_shared<ast::ReturnStmt>(self));
    
    RegisterFunction(F);
    return Codegen(F);
}
