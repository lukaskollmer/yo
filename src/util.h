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
#include <memory>
#include <sstream>
#include <iostream>
#include <type_traits>
#include <iomanip>


#define NS_START(x) namespace x {
#define NS_END }

#define let const auto
#define var auto

extern bool LKCompilerInternalOptionSigabrtOnFatalError();

NS_START(yo::util)

inline void noop() {}


// basically, the idea is to trigger the debugger if we're running inside lldb, and just exit otherwise
// this requires the `--int_sigabrt-on-fatal-error` flag to be set
[[noreturn]]
inline void exitOrAbort() {
    if (LKCompilerInternalOptionSigabrtOnFatalError()) {
        raise(SIGABRT);
    }
    
    exit(1);
}


#define LKLog(fmt, ...) printf("[%s] " fmt "\n", __PRETTY_FUNCTION__, ## __VA_ARGS__)


#define STR(x) #x
#define CONCAT_IMPL(x, y) x##y
#define CONCAT(x, y) CONCAT_IMPL(x, y)



#define LKFatalError(fmt, ...)  \
do {                            \
    printf("Fatal Error: " fmt ". func: %s, file: %s, line: %i\n", ## __VA_ARGS__ , __func__, __FILE__, __LINE__);  \
    if (LKCompilerInternalOptionSigabrtOnFatalError()) raise(SIGABRT);      \
    exit(1); \
} while (0)


#define LKAssert(e) \
do { if (__builtin_expect(!(bool)(e), 0)) { LKFatalError("LKAssert failed: %s", #e); } } while (0)


#define LKAssertMsg(e, msg_expr)                                            \
do { if (__builtin_expect(!(bool)(e), 0)) {                                 \
    auto msg_f = [&]() -> std::string { return std::string(msg_expr); };    \
    auto msg = msg_f();                                                     \
    LKFatalError("LKAssert failed: (%s): %s", #e, msg.c_str());             \
} } while (0)


#define LKAssertImplication(x, y) LKAssert(!(x) || (y));


__attribute__((format(printf, 1, 2)))
char* fmt_cstr(const char *fmt, ...);

char* fmt_cstr(const char *fmt, va_list);


struct Range {
    int64_t location;
    int64_t length;

    Range() : location(0), length(0) {}

    Range(int64_t location, int64_t length) : location(location), length(length) {}
};



template <typename T>
T abs(T value) {
    return value >= 0 ? value : -value;
}

inline bool isDigit(char c) {
    return c >= '0' && c <= '9';
}


template <typename R, typename T>
R bitcast(T value) {
    static_assert(sizeof(T) == sizeof(R), "cannot bitcast types of different size");
    return *reinterpret_cast<R*>(&value);
}



template <typename F>
class DeferHandle {
    F fn;
public:
    explicit DeferHandle(F fn) : fn(fn) {}
    ~DeferHandle() {
        fn();
    }
};

#define defer(imp) DeferHandle CONCAT(__defer_handle__, __COUNTER__)(imp);



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



#pragma mark - typeinfo

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
struct is_shared_ptr : std::false_type {};

template <typename T>
struct is_shared_ptr<std::shared_ptr<T>> : std::true_type {};

template <typename T>
inline constexpr bool is_shared_ptr_v = is_shared_ptr<T>::value;


template <typename T>
inline constexpr bool is_nullable_v = std::is_pointer_v<T> || is_shared_ptr_v<T>;


template <typename T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;




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


// Extracts a member pointer's class and member type

template <typename T>
struct member_ptr;

template <typename ClassT, typename MemberT>
struct member_ptr<MemberT ClassT::*> {
    using classT = ClassT;
    using memberT = MemberT;
};

} // namespace typeinfo





#pragma mark - vector

namespace vector {

template <typename T>
inline bool contains(const std::vector<T> &vector, const T &element) {
    return std::find(vector.begin(), vector.end(), element) != vector.end();
}


template <typename T, typename F>
bool contains_where(const std::vector<T> &vector, F fn) {
    for (const auto& elem : vector) {
        if (fn(elem)) return true;
    }
    return false;
}


template <typename T, typename F>
std::optional<T> first_where(const std::vector<T> &vector, F fn) {
    for (const auto& elem : vector) {
        if (fn(elem)) return elem;
    }
    return std::nullopt;
}

/// Iterate over a vector, 2nd `F` parameter is the current index
template <typename T, typename F>
void iteri(const std::vector<T> &vec, F &&fn) {
    for (size_t i = 0; i < vec.size(); i++) {
        fn(i, vec[i]);
    }
}

/// Iterate over a vector, 2nd `F` parameter  indicates whether this is the last element
template <typename T, typename F>
void iterl(const std::vector<T> &vec, F &&fn) {
    for (auto it = vec.begin(); it != vec.end(); it++) {
        fn(*it, it + 1 == vec.end());
    }
}


template <typename T, typename F>
auto map(const std::vector<T>& vec, F&& fn) {
    std::vector<std::invoke_result_t<F, const T&>> mapped;
    mapped.reserve(vec.size());
    for (const auto& elem : vec) {
        mapped.push_back(fn(elem));
    }
    return mapped;
}


template <typename T, typename F>
auto mapi(const std::vector<T>& vec, F&& fn) {
    std::vector<std::invoke_result_t<F, typename std::iterator_traits<typename std::vector<T>::iterator>::difference_type, const T&>> mapped;
    mapped.reserve(vec.size());
    for (auto it = vec.begin(); it != vec.end(); it++) {
        auto idx = std::distance(vec.begin(), it);
        mapped.push_back(fn(idx, *it));
    }
    return mapped;
}


template <typename T, typename F>
std::pair<std::vector<T>, std::vector<T>> filter_keeping_all(std::vector<T> &vector, F f) {
    std::vector<T> matched, unmatched;
    for (const auto& element : vector) {
        (f(element) == true ? matched : unmatched).push_back(element);
    }
    return {matched, unmatched};
}


template <typename T, typename F>
std::vector<T> filter(const std::vector<T> &vector, F &&fn) {
    std::vector<T> results;
    results.reserve(vector.size());
    for (const auto &element : vector) {
        if (fn(element)) results.push_back(element);
    }
    return results;
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


template <typename T>
void append(std::vector<T> &dest, const std::vector<T> &src) {
    dest.insert(dest.end(), src.begin(), src.end());
}

} // namespace vector



#pragma mark - map

namespace map {

template <typename K, typename V>
inline bool has_key(const std::map<K, V> &map, const K &key) {
    return map.find(key) != map.end();
}

template <typename K, typename V>
inline std::optional<V> get_opt(const std::map<K, V> &map, const K &key) {
    if (has_key(map, key)) return map.at(key);
    else return std::nullopt;
}

template <typename K, typename V>
std::optional<K> reverse_lookup(const std::map<K, V> &map, const V &value) {
    for (auto &[key, val] : map) {
        if (val == value) return key;
    }
    return std::nullopt;
}

} // namespace map




#pragma mark - string

namespace string {

bool contains(const std::string_view string, const std::string_view other);

template <typename F>
bool allCharsMatch(std::string_view string, F &&fn) {
    return std::all_of(string.begin(), string.end(), fn);
}

// There's a good reason why these two use std::string_view, but i don't remember it
bool has_prefix(const std::string_view string, const std::string_view prefix);
bool has_suffix(const std::string_view string, const std::string_view suffix);

std::string substr_from_index(const std::string &string, uint64_t index);

// Returns a substring from the start of the string to the specified index
// If `Index` is negative, it's counted from the end of the string
std::string substr_to_index(const std::string &string, uint64_t index);

std::string substr_with_range(const std::string &string, Range range);


std::string replace_all(const std::string &string, const std::string &pattern, const std::string &replacement);

std::vector<std::string> split(const std::string &string, const std::string &delimiter);
std::string join(const std::vector<std::string> &strings, const std::string &delimiter);

std::ostream& append_with_indentation(std::ostream &OS, const std::string &other, unsigned indent);
std::string& append_with_indentation(std::string &target, const std::string &other, unsigned indent);


// TODO move this into a path_utils namespace or something like that?
std::string lastPathCompotent(const std::string &path);
std::string excludingLastPathComponent(const std::string &path);
std::string excludingFileExtension(const std::string &path);
std::pair<std::string, std::string> extractPathAndFilename(const std::string &path);

} // namespace string



namespace fs {
bool file_exists(const std::string &path);
std::string read_file(const std::string& path);
std::string read_specific_line(const std::string& path, uint64_t lineNumber);
}

namespace fs::path_utils {
std::string getFilename(const std::string& path);
}









#pragma mark - fmt

namespace fmt {

// TODO somehow allow overloading this to get custom formatting for custom types?
template <typename T>
void value_formatter(std::ostream &OS, std::string_view flags, const T &arg) {
    if constexpr(typeinfo::is_nullable_v<T>) {
        if (flags == "p") {
            if constexpr(std::is_pointer_v<T>) {
                OS << reinterpret_cast<const void *>(arg);
            } else if constexpr(typeinfo::is_shared_ptr_v<T>) {
                OS << reinterpret_cast<const void *>(arg.get());
            } else {
                static_assert(std::is_same_v<T, void>, "should never reach here");
            }
            return;
        }
    }
    
    OS << arg;
}

template <>
inline void value_formatter<bool>(std::ostream &OS, std::string_view flags, const bool &arg) {
    OS << (arg ? "true" : "false");
}

template <>
inline void value_formatter<uint8_t>(std::ostream &OS, std::string_view flags, const uint8_t &arg) {
    if (flags == "c") {
        OS << arg;
    } else {
        OS << static_cast<uint32_t>(arg);
    }
    //OS << (arg ? "true" : "false");
}



template <typename T, typename... Ts>
void format_imp(std::ostream &OS, std::string_view format, T &&arg, Ts&&... args) {
    auto pos = format.find_first_of('{');
    if (pos == std::string_view::npos) {
        OS << format;
        return;
    }
    
    OS << format.substr(0, pos);
    if (format[pos + 1] == '}') {
        value_formatter<typeinfo::remove_cvref_t<T>>(OS, "", arg);
        format.remove_prefix(pos + 2);
    } else {
        auto end_pos = format.find_first_of('}', pos);
        LKAssert(end_pos != std::string_view::npos);
        std::string_view flags = format.substr(pos + 1, end_pos - 1 - pos);
        
        value_formatter<typeinfo::remove_cvref_t<T>>(OS, flags, arg);
        format.remove_prefix(end_pos + 1);
    }

    if constexpr(sizeof...(args) == 0) {
        OS << format;
        LKAssert(format.find('{') == format.npos);
    } else {
        LKAssert(format.find('{') != format.npos);
        format_imp(OS, format, std::forward<Ts>(args)...);
    }
}


template <typename... Ts>
std::string format(std::string_view format, Ts &&...args) {
    // TODO would be cool it this somehow worked
    //static_assert((format.find('{') != std::string_view::npos) == (sizeof...(args) > 0), "invalid format arguments");
    
    std::ostringstream OS;
    if constexpr(sizeof...(args) == 0) {
        LKAssert(format.find('{') == format.npos);
        OS << format;
    } else {
        format_imp(OS, format, std::forward<Ts>(args)...);
    }
    return OS.str();
}


template <typename... Ts>
void print(std::string_view format, Ts &&...args) {
    std::ostream &OS = std::cout;
    if constexpr(sizeof...(args) == 0) {
        LKAssert(format.find('{') == format.npos);
        OS << format;
    } else {
        format_imp(OS, format, std::forward<Ts>(args)...);
    }
    OS << std::endl;
}


} // namespace fmt



NS_END
