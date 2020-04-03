//
//  NameLookup.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-03-26.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#include "NameLookup.h"
#include "IRGen.h"
#include "Type.h"
#include "Mangling.h"

#include "util/util.h"
#include "util/Format.h"
#include "util/MapUtils.h"
#include "util/llvm_casting.h"

#include <string>
#include <map>


using namespace yo;
using namespace yo::irgen;
using NK = ast::Node::Kind;


std::ostream& irgen::operator<<(std::ostream &OS, const ValueInfo &VI) {
    switch (VI.kind) {
        case ValueInfo::Kind::TypeRef:
            return OS << "TypeRef[" << VI.getTypeRef() << "]";
        
        case ValueInfo::Kind::TypeRefTmpl:
            return OS << "TypeRefTmpl[...]";
        
        case ValueInfo::Kind::Property: {
            auto &info = VI.getPropertyInfo();
            OS << "property[" << info.parentType;
            if (VI.isStaticMember) {
                OS << "::";
            } else {
                OS << ".";
            }
            return OS << info.name << ": " << info.type << "]";
        }
        
        case ValueInfo::Kind::LocalVar: {
            LKFatalError("");
        }
        
        case ValueInfo::Kind::Function: {
            auto &[selfTy, funcDecl] = VI.getFunctionInfo();
            OS << "function[";
            if (funcDecl->isInstanceMethod()) {
                OS << selfTy << ".";
            } else if (funcDecl->isStaticMethod()) {
                OS << selfTy << "::";
            }
            return OS << funcDecl->getSignature() << "]";
        }
    }
}


std::vector<ValueInfo> NameLookup::lookup(const std::shared_ptr<ast::Expr> &expr) {
    if (auto ident = llvm::dyn_cast<ast::Ident>(expr)) {
        if (auto binding = irgen.localScope.get(ident->value)) {
            return { ValueInfo::localVar(binding->type) };
        } else {
            std::vector<ValueInfo> results;
            irgen.registerNamedDecls(ident->value);
            
            if (auto type = irgen.nominalTypes.get(ident->value)) {
                results.push_back(ValueInfo::typeRef(*type));
            }
            
            if (auto declInfos = util::map::get_opt(irgen.namedDeclInfos, ident->value)) {
                for (const NamedDeclInfo &declInfo : *declInfos) {
                    switch (declInfo.decl->getKind()) {
                        case NK::FunctionDecl:
                            // TODO just pass along the RC?!
                            results.push_back(ValueInfo::function(nullptr, llvm::cast<ast::FunctionDecl>(declInfo.decl)));
                            break;
                        case NK::StructDecl: {
                            auto SD = llvm::cast<ast::StructDecl>(declInfo.decl);
                            if (SD->isTemplateDecl()) {
                                results.push_back(ValueInfo::typeRefTmpl(SD));
                            }
                            break;
                        }
                        case NK::VariantDecl: {
                            auto VD = llvm::cast<ast::VariantDecl>(declInfo.decl);
                            if (VD->isTemplateDecl()) {
                                results.push_back(ValueInfo::typeRefTmpl(VD));
                            }
                            break;
                        }
                        case NK::TypealiasDecl:
                            LKFatalError("");
                        
                        default:
                            LKFatalError("");
                    }
                }
            }
            
            return results;
        }
        LKFatalError("unreachable?");
    
    } else if (auto memberExpr = llvm::dyn_cast<ast::MemberExpr>(expr)) {
        if (auto rawExpr = llvm::dyn_cast<ast::RawLLVMValueExpr>(memberExpr->target)) {
            // if the member expr's target is a raw expr, we're looking up a member function call
            // ^^ do we really know that for sure?
            // check whether the type has the member
            auto membersTable = computeMemberTableForType(rawExpr->type);
            if (auto members = util::map::get_opt(membersTable.members, memberExpr->memberName)) {
                return *members;
            } else {
                return {};
            }
        } else {
            std::vector<ValueInfo> results;
            
            auto handleForType = [&](Type *type) {
                auto membersTable = computeMemberTableForType(type);
                membersTable.dump();
                if (auto members = util::map::get_opt(membersTable.members, memberExpr->memberName)) {
                    util::vector::append(results, *members);
                }
            };
            
            for (auto &L : lookup(memberExpr->target)) {
                switch (L.kind) {
                    case ValueInfo::Kind::TypeRef:
                        handleForType(L.getTypeRef());
                        break;
                    
                    case ValueInfo::Kind::LocalVar:
                        handleForType(L.getTypeRef());
                        break;
                    
                    case ValueInfo::Kind::Property:
                        handleForType(L.getPropertyInfo().type);
                        break;
                    
                    case ValueInfo::Kind::Function:
                        LKFatalError("");
                        break;
                    
                    case ValueInfo::Kind::TypeRefTmpl:
                        // static member on a templated type, but we don't have the template arguments, and therefore cant resolve the ty0e in here
                        LKFatalError("");
                }
            }
            return results;
        }
    } else if (auto callExpr = llvm::dyn_cast<ast::CallExpr>(expr)) {
        // not really a name lookup, just return the call result type
        if (auto target = irgen.resolveCall_opt(callExpr, kSkipCodegen)) {
            auto retType = irgen.resolveTypeDesc(target->signature.returnType, false);
            return { ValueInfo::typeRef(retType) };
        }
        return {};
    }
    
    LKFatalError("unhandled node?");
}




TypeMembersTable NameLookup::computeMemberTableForType(Type *type) {
    if (auto refTy = llvm::dyn_cast<ReferenceType>(type)) {
        type = refTy->getReferencedType();
    }
    
    TypeMembersTable membersTable(type);
    
    if (auto structTy = llvm::dyn_cast<StructType>(type)) {
        for (auto &[memberName, memberType] : structTy->getMembers()) {
            membersTable.addProperty(type, memberName, memberType);
        }
    } else if (auto variantTy = llvm::dyn_cast<VariantType>(type)) {
        for (auto &[elemName, elemType] : variantTy->getElements()) {
            // elements w/ associated data get static functions generated
            if (!elemType) {
                membersTable.addProperty(type, elemName, type, true);
            }
        }
    }
    
    
    for (const auto &[name, declInfos] : irgen.namedDeclInfos) {
        for (const NamedDeclInfo &declInfo : declInfos) {
            if (auto funcDecl = llvm::dyn_cast<ast::FunctionDecl>(declInfo.decl)) {
                // collect all instance methods which, as their first parameter, can accept the type
//                util::fmt::print("decl->name: {} of sig '{}'", funcDecl->name, funcDecl->signature);
                
                auto shouldAdd = [&]() -> bool {
                    if (!(funcDecl->isInstanceMethod() || funcDecl->isStaticMethod())) {
                        return false;
                    }
                    if (funcDecl->getAttributes().int_isCtor) {
                        return false;
                    }
                    if (!isAcceptableFirstParam(type, funcDecl)) {
                        return false;
                    }
                    
                    return true;
                };
                
                if (shouldAdd()) {
                    membersTable.addMemberFunction(type, funcDecl);
                }
            }
        }
    }
    
    return membersTable;
}

bool NameLookup::isAcceptableFirstParam(Type *type, const std::shared_ptr<ast::FunctionDecl> &decl) {
    LKAssert(decl->getSignature().numberOfParameters() > 0);
    
    if (decl->getSignature().isTemplateDecl()) {
        ast::FunctionSignature sig = decl->getSignature();
        sig.paramTypes.erase(sig.paramTypes.begin() + 1, sig.paramTypes.end());

        auto callExpr = std::make_shared<ast::CallExpr>(nullptr);

        if (auto mapping = irgen.attemptToResolveTemplateArgumentTypesForCall(sig, callExpr, {{type, std::make_shared<ast::RawLLVMValueExpr>(nullptr, type)}})) {
            return true;
        }
        return false;
    }
    
    
    auto typeDesc = decl->getSignature().paramTypes[0];
//    util::fmt::print("type: {}, typeDesc: {}", type, typeDesc);
    
    if (auto resolved = typeDesc->getResolvedType()) {
        if (resolved == type) {
            return true;
        } else if (auto refTy = llvm::dyn_cast<ReferenceType>(resolved)) {
            return refTy->getReferencedType() == type;
        }
        return false;
    }
    
    if (typeDesc->isReference()) {
        typeDesc = typeDesc->getPointee();
    }
    
    // TODO this resolves all types, thus maybe instantiating templates we nay not (yet?) want instantiated
    // the switch below, at the end of the day, does the same thing, but before tries to return false early in some cases where its clear that the types don't match
    return irgen.resolveTypeDesc(typeDesc, false) == type;
    
//    switch (type->getTypeId()) {
//        case Type::TypeID::Struct: {
//            auto ST = llvm::cast<StructType>(type);
//            if (!(typeDesc->isNominal() || typeDesc->isNominalTemplated())) {
//                // non-nominal typedesc cannot resolve to a nominal type (note that the other way around is very much possible)
//                return false;
//            }
//            if (ST->isTemplateInstantiation() != typeDesc->isNominalTemplated()) {
//                return false;
//            }
//            return irgen.resolveTypeDesc(typeDesc, false) == type;
//        }
//
//        case Type::TypeID::Tuple: {
//            LKFatalError("");
//        }
//
//        case Type::TypeID::Variant: {
//            auto variantTy = llvm::cast<VariantType>(type);
//            LKFatalError("");
//        }
//        case Type::TypeID::Void:
//        case Type::TypeID::Numerical:
//        case Type::TypeID::Pointer:
//        case Type::TypeID::Reference:
//        case Type::TypeID::Function:
//            LKFatalError("");
//    }
    
    //LKFatalError("type: '%s', typeDesc: '%s'", type->str_desc().c_str(), typeDesc->str().c_str());
    LKFatalError("");
}


void TypeMembersTable::dump() const {
    util::fmt::print("TypeMembersTable for '{}':", type);
    for (auto &[name, members] : members) {
        for (auto &memberInfo : members) {
            util::fmt::print("- {}: {}", name, memberInfo);
        }
    }
}
