//
//  TypeDesc.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-08-14.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "TypeDesc.h"

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
            const auto& FTI = getFunctionTypeInfo();
            std::string str;
            str.append("#[callingConvention=").append(cc_to_str(FTI.callingConvention)).append("] (");
            for (auto it = FTI.parameterTypes.begin(); it != FTI.parameterTypes.end(); it++) {
                str.append((*it)->str());
                if (it + 1 != FTI.parameterTypes.end()) {
                    str.append(", ");
                }
            }
            str.append(") -> ").append(FTI.returnType->str());
            return str;
        }
        
        case Kind::Resolved:
            return std::string("resolved(").append(getResolvedType()->str()).append(")");
    }
}
