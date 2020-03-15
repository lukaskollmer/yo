//
//  util_llvm.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-13.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>
#include <string>
#include <ostream>


#pragma mark - Casting

namespace llvm {

inline llvm::raw_ostream& operator<<(llvm::raw_ostream &OS, llvm::Value *value) {
    value->print(OS);
    return OS;
}

inline llvm::raw_ostream& operator<<(llvm::raw_ostream &OS, llvm::Type *type) {
    type->print(OS);
    return OS;
}

// Specializations to llvm's casting APIs to add support for `std::shared_ptr` to `isa`, `cast`, et al

template<class T> struct simplify_type<std::shared_ptr<T>> {
    using SimpleType = std::shared_ptr<T>;
    
    static SimpleType getSimplifiedValue(std::shared_ptr<T> &val) {
        return val;
    }
};


template <typename To, typename From>
struct isa_impl_cl<To, const std::shared_ptr<From>> {
  static inline bool doit(const std::shared_ptr<From> &val) {
      assert(val && "isa<> used on a null pointer");
      return isa_impl_cl<To, From>::doit(*val);
  }
};


template <class To, class From>
struct cast_retty_impl<To, std::shared_ptr<From>> {
private:
    using PointerType = typename cast_retty_impl<To, From *>::ret_type;
    using ResultType = typename std::remove_pointer<PointerType>::type;

public:
    using ret_type = std::shared_ptr<ResultType>;
};

template <class To, class From>
struct cast_retty_impl<To, const std::shared_ptr<From>> {
    using ret_type = std::add_const_t<typename cast_retty_impl<To, std::shared_ptr<From>>::ret_type>;
};


template<class To, class FromTy>
struct cast_convert_val<To, std::shared_ptr<FromTy>, std::shared_ptr<FromTy>> { // <To, From, SimpleFrom>
private:
    using ReturnType = typename cast_retty<To, std::shared_ptr<FromTy>>::ret_type;
    static_assert(yo::util::typeinfo::is_shared_ptr_v<ReturnType>, "");

public:
    static ReturnType doit(const std::shared_ptr<FromTy> &val) {
        return std::static_pointer_cast<To>(val);
  }
};

template<class To, class FromTy>
struct cast_convert_val<To, const std::shared_ptr<FromTy>, const std::shared_ptr<FromTy>> { // <To, From, SimpleFrom>
private:
    using ReturnType = typename cast_retty<To, const std::shared_ptr<FromTy>>::ret_type;
    static_assert(yo::util::typeinfo::is_shared_ptr_v<ReturnType>, "");

public:
    static ReturnType doit(const std::shared_ptr<FromTy> &val) {
        return std::static_pointer_cast<To>(val);
  }
};

} // namespace llvm


#pragma mark - Other

namespace yo::util::llvm_utils {
//    inline llvm::raw_ostream& operator<<(llvm::raw_ostream &OS, llvm::Value *V) {
//        V->print(OS);
//        return OS;
//    }
//
//    inline llvm::raw_ostream& operator<<(llvm::raw_ostream &OS, llvm::Type *T) {
//        T->print(OS);
//        return OS;
//    }
    
    
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


template <int N>
struct yo::util::fmt::formatter<llvm::SmallString<N>> {
    static void format(std::ostream &OS, std::string_view flags, const llvm::SmallString<N> &value) {
        formatter<llvm::StringRef>::format(OS, flags, value.str());
//        OS << value.str();
    }
};
