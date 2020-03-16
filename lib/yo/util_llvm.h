//
//  util_llvm.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-13.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util/util.h"
#include "util/Format.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>
#include <string>
#include <ostream>



#pragma mark - Other

namespace llvm {
    inline raw_ostream& operator<<(raw_ostream &OS, Value *V) {
        V->print(OS);
        return OS;
    }

    inline raw_ostream& operator<<(raw_ostream &OS, Type *T) {
        T->print(OS);
        return OS;
    }
}

namespace yo::util::llvm_utils {
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


template <>
struct yo::util::fmt::formatter<llvm::Value *> {
    static void format(std::ostream &OS, std::string_view flags, const llvm::Value *value) {
        OS << yo::util::llvm_utils::to_string(value);
    }
};


template <>
struct yo::util::fmt::formatter<llvm::Type *> {
    static void format(std::ostream &OS, std::string_view flags, const llvm::Type *type) {
        OS << yo::util::llvm_utils::to_string(type);
    }
};


template <>
struct yo::util::fmt::formatter<llvm::StringRef> {
    static void format(std::ostream &OS, std::string_view flags, const llvm::StringRef &value) {
        for (char c : value) {
            OS << c;
        }
    }
};


template <unsigned N>
struct yo::util::fmt::formatter<llvm::SmallString<N>> {
    static void format(std::ostream &OS, std::string_view flags, const llvm::SmallString<N> &value) {
        formatter<llvm::StringRef>::format(OS, flags, value.str());
//        OS << value.str();
    }
};
