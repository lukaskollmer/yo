//
//  util.h
//  yo
//
//  Created by Lukas Kollmer on 2019-02-25.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <string>
#include <vector>
#include <functional>
#include <ostream>
#include <csignal>
#include <map>
#include <optional>


#define NS_START(x) namespace x {
#define NS_END }


NS_START(yo::util)


inline void noop() {}

using LKInteger = std::int64_t;
using LKUInteger = std::uint64_t;

#define LKLog(fmt, ...) printf("[%s] " fmt "\n", __PRETTY_FUNCTION__, ## __VA_ARGS__)


//#define LK_PUSH_CLANG_IGNORE(name) \
//_Pragma(
//#pragma clang diagnostic push
//#pragma clang diagnostic ignored "-Wunused-lambda-capture"

#define getter(name) auto &get##name() { return name; }


#define STR(x) #x
#define CONCAT_IMPL(x, y) x##y
#define CONCAT(x, y) CONCAT_IMPL(x, y)


#define LKFatalError(fmt, ...) \
{ printf("Fatal Error: " fmt ". func: %s, file: %s, line: %i\n", ## __VA_ARGS__ , __func__, __FILE__, __LINE__); raise(SIGABRT); exit(1); }



#define LKAssertImplication(x, y) LKAssert(!(x) || (y));

#define LKAssert(e) \
(__builtin_expect(!(e), 0) ? (void)(printf("LKAssert Failed: (%s) function %s, file %s, line %i\n", #e, __func__, __FILE__, __LINE__) & raise(SIGABRT)) : (void)0)


#define LKAssertMsg(e, msg_expr) \
if (__builtin_expect(!(bool)(e), 0)) {                                                  \
    auto msg_f = [&]() -> std::string { return std::string(msg_expr); };                \
    auto msg = msg_f();                                                                 \
    printf("LKAssert failed: %s, at %s in %s:%i\n",                                 \
           msg.empty() ? #e : msg.c_str(), __PRETTY_FUNCTION__, __FILE__, __LINE__);    \
    raise(SIGABRT);                                                                     \
}



__attribute__((format(printf, 1, 2)))
char *fmt_cstr(const char *format, ...);


struct Range {
    long location;
    long length;
    
    Range() : location(0), length(0) {}
    
    Range(long location, long length) : location(location), length(length) {}
};




class LKDeferHandle {
public:
    using Imp = std::function<void()>;
    
    explicit LKDeferHandle(Imp fn) : fn(fn) {}
    ~LKDeferHandle() {
        fn();
    }
    
private:
    Imp fn;
};

#define defer(imp) LKDeferHandle CONCAT(__defer_handle__, __COUNTER__)(imp);



struct _LKExitHandler {
    std::function<void()> invoke;
    _LKExitHandler(std::function<void()> invoke) : invoke(invoke) {}
};

inline std::ostream& operator<<(std::ostream &OS, const _LKExitHandler &EH) {
    OS << std::endl;
    EH.invoke();
    return OS;
}


#define fatalError yo::util::_LKExitHandler([]() { raise(SIGABRT); });



namespace typeinfo {
    // Demangling (forward to __cxa_demangle)
    std::string demangle(const char *name);
    
    
    template <typename T>
    using is_vector = std::is_same<T, std::vector<typename T::value_type, typename T::allocator_type>>;
    
    // True if `T` is a `std::vector`
    template <typename T>
    inline constexpr bool is_vector_v = is_vector<T>::value;
    
    // True if `T` is an `std::vector<U>`
    template <typename T, typename U>
    inline constexpr bool is_vector_of_v = is_vector_v<T> && std::is_same_v<typename T::value_type, U>;
    
    // True if `T` is an `std::vector` of elements convertible to `U`
    template <typename T, typename U>
    inline constexpr bool is_vector_of_convertible_v = std::is_same<T, std::vector<typename T::value_type, typename T::allocator_type>>::value && std::is_convertible<typename T::value_type, U>::value;

    
    
    template <typename T>
    std::string getTypename(const T &arg) {
        return demangle(typeid(arg).name());
    }
    
    
    template <typename T>
    struct TypeInfo {
        static const std::string name;
    };
    
    template <typename T>
    const std::string TypeInfo<T>::name = demangle(typeid(T).name());
}


namespace vector {
    template <typename T>
    inline bool contains(const std::vector<T> &vector, const T &element) {
        return std::find(vector.begin(), vector.end(), element) != vector.end();
    }
    
    template <typename T, typename F>
    bool contains_where(const std::vector<T> &vector, F fn) {
        for (auto &elem : vector) {
            if (fn(elem)) return true;
        }
        return false;
    }
    
    template <typename T, typename F>
    std::optional<T> first_where(const std::vector<T> &vector, F fn) {
        for (auto &elem : vector) {
            if (fn(elem)) return elem;
        }
        return std::nullopt;
    }
    
    template <typename T, typename F>
    std::vector<std::invoke_result_t<F, T&>> map(const std::vector<T> &vector, F Fn) {
        std::vector<std::invoke_result_t<F, T&>> mapped(vector.size());
        std::transform(vector.begin(), vector.end(), mapped.begin(), Fn);
        return mapped;
    }
    
    template <typename T, typename F>
    std::pair<std::vector<T>, std::vector<T>> filter_keeping_all(std::vector<T> &vector, F f) {
        std::vector<T> matched, unmatched;
        for (auto &element : vector) {
            (f(element) == true ? matched : unmatched).push_back(element);
        }
        return {matched, unmatched};
    }
    
    
    // Different behaviour depending on whether F returns void or U
    template <template <typename...> class container, typename T, typename U, typename F>
    U reduce(const container<T> &input, U initialValue, F fn) {
        U accumulator = initialValue;
        for (auto it = input.begin(); it != input.end(); it++) {
            if constexpr(std::is_void_v<std::invoke_result_t<F, U&, const T&, uint64_t>>) {
                fn(accumulator, *it, it - input.begin());
            } else {
                static_assert(std::is_same_v<U, std::invoke_result_t<F, F, const U&, const T&, uint64_t>>, "");
                accumulator = fn(accumulator, *it, it - input.begin());
            }
        }
        return accumulator;
    }


}


namespace map {
    template <typename K, typename V>
    inline bool contains_key(const std::map<K, V> &map, const K &key) {
        return map.find(key) != map.end();
    }
    
    template <typename K, typename V>
    inline std::optional<V> get_opt(const std::map<K, V> &map, const K &key) {
        if (contains_key(map, key)) return map.at(key);
        else return std::nullopt;
    }
}


namespace string {
    bool contains(const std::string_view string, const std::string_view other);
    
    // There's a good reason why these two use std::string_view, but i don't remember it
    bool has_prefix(const std::string_view string, const std::string_view prefix);
    bool has_suffix(const std::string_view string, const std::string_view suffix);
    
    std::string substr_from_index(const std::string string, LKUInteger index);
    
    // Returns a substring from the start of the string to the specified index
    // If `Index` is negative, it's counted from the end of the string
    std::string substr_to_index(const std::string string, LKInteger index);
    
    std::string substr_with_range(const std::string string, Range range);
    
    
    std::string replace_all(const std::string string, const std::string pattern, const std::string replacement);
    
    std::vector<std::string> split(const std::string string, const std::string delimiter);
    std::string join(const std::vector<std::string> &strings, const std::string delimiter);
    
    std::string& append_with_indentation(std::string &target, std::string &&other, unsigned indent);
    
    
    // TODO move this into a path_utils namespace or something like that?
    std::string lastPathCompotent(const std::string &path);
    std::string excludingLastPathComponent(const std::string &path);
    std::string excludingFileExtension(const std::string &path);
    std::pair<std::string, std::string> extractPathAndFilename(const std::string &path);
}



namespace fs {
    std::string read_file(const std::string path);
}

namespace fs::path_utils {
    std::string getFilename(const std::string &path);
}



NS_END
