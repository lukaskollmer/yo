//
//  TypeDesc.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-08-14.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "TypeDesc.h"
#include "AST.h"
#include "util/util.h"
#include "util/Format.h"
#include "util/VectorUtils.h"
#include "yo/Type.h"

using namespace yo::ast;


std::string cc_to_str(CallingConvention cc) {
    switch (cc) {
        case CallingConvention::C: return "C";
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
            return getResolvedType()->str_desc();
            //return std::string("resolved(").append(getResolvedType()->str_desc()).append(")");
        
        case Kind::Decltype:
            //return util::fmt::format("decltype({})", getDecltypeExpr()->description());
            return "decltype(<expr>)";
        
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
        
        case Kind::Tuple: {
            std::ostringstream OS;
            OS << '(';
            util::vector::iterl(getTupleMembers(), [&OS](auto &TD, bool isLast) {
                OS << TD->str();
                if (!isLast) OS << ", ";
            });
            OS << ')';
            return OS.str();
        }
    }
}
