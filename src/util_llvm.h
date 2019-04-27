//
//  util_llvm.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-13.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <string>
#include <ostream>

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"

namespace util_llvm {
    inline llvm::raw_ostream& operator<<(llvm::raw_ostream &OS, llvm::Value *V) {
        V->print(OS);
        return OS;
    }
    
    inline llvm::raw_ostream& operator<<(llvm::raw_ostream &OS, llvm::Type *T) {
        T->print(OS);
        return OS;
    }
    
    
    template <typename T>
    inline std::string to_string(T *arg) {
        std::string S;
        llvm::raw_string_ostream OS(S);
        arg->print(OS);
        return OS.str();
    }
    
    
    inline std::ostream& operator<<(std::ostream& OS, llvm::Value *V) {
        return OS << to_string(V);
    }
    
    inline std::ostream& operator<<(std::ostream& OS, llvm::Type *T) {
        return OS << to_string(T);
    }
}
