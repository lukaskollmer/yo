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


inline void noop() {}

using LKInteger = std::int64_t;
using LKUInteger = std::uint64_t;

#define LKLog(fmt, ...) printf("[%s] " fmt "\n", __PRETTY_FUNCTION__, ## __VA_ARGS__)



#define getter(name) auto &get##name() { return name; }


#define NS_START(x) namespace x {
#define NS_END }


#define STR(x) #x
#define CONCAT_IMPL(x, y) x##y
#define CONCAT(x, y) CONCAT_IMPL(x, y)



#define LKFatalError(fmt, ...) \
{ printf("Fatal Error: " fmt ". func: %s, file: %s, line: %i\n", ## __VA_ARGS__ , __func__, __FILE__, __LINE__); raise(SIGABRT); exit(1); }



#define precondition1(e) \
(__builtin_expect(!(e), 0) ? (void)(printf("Precondition Failed: (%s) function %s, file %s, line %i\n", #e, __func__, __FILE__, __LINE__) & raise(SIGABRT)) : (void)0)

#define precondition2(e, msg_expr)                                                      \
if (__builtin_expect(!(bool)(e), 0)) {                                                  \
    auto msg_f = [&]() -> std::string { return std::string(msg_expr); };                \
    auto msg = msg_f();                                                                 \
    printf("Precondition failed: %s, at %s in %s:%i\n",                                 \
           msg.empty() ? #e : msg.c_str(), __PRETTY_FUNCTION__, __FILE__, __LINE__);    \
    raise(SIGABRT);                                                                     \
}


#define GET_MACRO(_1,_2,NAME,...) NAME
#define precondition(...) GET_MACRO(__VA_ARGS__, precondition2, precondition1)(__VA_ARGS__)

#define assert_implication(x, y) precondition1(!(x) || (y))


__attribute__((format(printf, 1, 2)))
char *LKStringUtils_FormatIntoNewBuffer(const char *format, ...);

#define fmt_c LKStringUtils_FormatIntoNewBuffer


struct Range {
    long Location;
    long Length;
    
    Range() : Location(0), Length(0) {}
    
    Range(long Location, long Length) : Location(Location), Length(Length) {}
};




class LKDeferHandle {
public:
    using Imp = std::function<void()>;
    
    explicit LKDeferHandle(Imp Function) : Function(Function) {}
    ~LKDeferHandle() {
        Function();
    }
    
private:
    Imp Function;
};

#define defer(imp) LKDeferHandle CONCAT(__defer_handle__, __COUNTER__)(imp);


namespace util::typeinfo {
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
    std::string GetTypename(const T &arg) {
        return demangle(typeid(arg).name());
    }
    
    
    template <typename T>
    struct LKTypeInfo {
        static const std::string Name;
    };
    
    template <typename T>
    const std::string LKTypeInfo<T>::Name = demangle(typeid(T).name());
    
    

}


namespace util::vector {
    template <typename T>
    inline bool contains(const std::vector<T> &Vector, const T &Element) {
        return std::find(Vector.begin(), Vector.end(), Element) != Vector.end();
    }
    
    template <typename T, typename F>
    std::vector<std::invoke_result_t<F, T&>> map(const std::vector<T> &Vector, F Fn) {
        std::vector<std::invoke_result_t<F, T&>> Mapped(Vector.size());
        std::transform(Vector.begin(), Vector.end(), Mapped.begin(), Fn);
        return Mapped;
    }
    
}


namespace util::string {
    std::string repeating(const char C, std::string::size_type N);
    bool contains(const std::string_view String, const std::string_view Other);
    
    // There's a good reason why these two use std::string_view, but i don't remember it
    bool has_prefix(const std::string_view String, const std::string_view Prefix);
    bool has_suffix(const std::string_view String, const std::string_view Suffix);
    
    std::string substr_from_index(const std::string String, LKUInteger Index);
    
    // Returns a substring from the start of the string to the specified index
    // If `Index` is negative, it's counted from the end of the string
    std::string substr_to_index(const std::string String, LKInteger Index);
    
    std::string substr_with_range(const std::string String, Range Range);
    
    
    std::string replace_all(const std::string String, const std::string Pattern, const std::string Replacement);
    
    std::vector<std::string> split(const std::string String, const std::string Delimiter);
    std::string join(const std::vector<std::string> &Strings, const std::string Delimiter);
    
    std::string& append_with_indentation(std::string &Target, std::string &&Other, unsigned Indent);
    
    std::string lastPathCompotent(std::string &Path);
    std::string excludingLastPathComponent(std::string &Path);
    std::string excludingFileExtension(const std::string &Path);
}

