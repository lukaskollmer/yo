//
//  llvm_casting.cpp
//  yo
//
//  Created by Lukas Kollmer on 2020-03-16.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "llvm/Support/Casting.h"
#include <memory>


namespace llvm {

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
