//
//  TypeDesc.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-08-14.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "TypeDesc.h"
#include "AST.h"

using namespace yo::ast;


std::string cc_to_str(yo::irgen::CallingConvention cc) {
    switch (cc) {
        case yo::irgen::CallingConvention::C: return "C";
    }
}


std::string TypeDesc::str() const {
    switch (kind) {
        case TypeDesc::Kind::Nominal:
            return getName();
        
        case TypeDesc::Kind::Pointer:
            return std::string("*").append(getPointee()->str());
        
        case Kind::Reference:
            return std::string("&").append(getPointee()->str());
        
        case TypeDesc::Kind::Function: {
            const auto &FTI = getFunctionTypeInfo();
            std::ostringstream OS;
            OS << "#[callingConvention=";
            OS << cc_to_str(FTI.callingConvention);
            OS << "] (";
            for (auto it = FTI.parameterTypes.begin(); it != FTI.parameterTypes.end(); it++) {
                OS << (*it)->str();
                if (it + 1 != FTI.parameterTypes.end()) {
                    OS << ", ";
                }
            }
            OS << ") -> " << FTI.returnType->str();
            return OS.str();
        }
        
        case Kind::Resolved:
            //return std::string("resolved(").append(getResolvedType()->str()).append(")");
            return getResolvedType()->str();
        
        case Kind::Decltype:
            return util::fmt::format("decltype({})", getDecltypeExpr()->description());
        
        case Kind::NominalTemplated: {
            auto &[name, types] = std::get<NominalTemplatedDataT>(this->data);
            std::ostringstream OS;
            OS << name;
            OS << "<";
            for (auto it = types.begin(); it != types.end(); it++) {
                OS << (*it)->str();
                if (it + 1 != types.end()) {
                    OS << ", ";
                }
            }
            OS << ">";
            return OS.str();
        }
    }
}
