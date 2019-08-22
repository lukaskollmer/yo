//
//  TypeDesc.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-08-14.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "TypeDesc.h"

using namespace yo::ast;

std::string TypeDesc::str() const {
    switch (kind) {
        case TypeDesc::Kind::Nominal:
            return getName();
        case TypeDesc::Kind::Pointer:
            return std::string("*").append(getPointee()->str());
        case TypeDesc::Kind::Function:
            LKFatalError("TODO");
        case Kind::Resolved:
            return std::string("resolved(").append(getResolvedType()->str()).append(")");
    }
}
