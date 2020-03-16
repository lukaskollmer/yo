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
#include <ostream>
#include <memory>
#include <type_traits>
#include <cstring>
#include <csignal>


#define NS_START(x) namespace x {
#define NS_END }


extern "C" bool LKCompilerInternalOptionSigabrtOnFatalError();

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



/// Print an error message to stdout and abort
#define LKFatalError(fmt, ...)  \
do {                            \
    printf("Fatal Error: " fmt ". func: %s, file: %s, line: %i\n", ## __VA_ARGS__ , __func__, __FILE__, __LINE__);  \
    if (LKCompilerInternalOptionSigabrtOnFatalError()) raise(SIGABRT);      \
    exit(1); \
} while (0)


/// Assert that a condition is true, otherwise abort
#define LKAssert(e) \
do { if (__builtin_expect(!(bool)(e), 0)) { LKFatalError("LKAssert failed: %s", #e); } } while (0)


/// Assert that a condition is true, otherwise abort w/ a custom message
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


template <typename To, typename From>
auto bitcast(const From &value) -> std::enable_if_t<(sizeof(From) == sizeof(To)) && std::is_trivially_copyable_v<From> && std::is_trivial_v<To>, To> {
    To retval;
    std::memcpy(&retval, &value, sizeof(From));
    return retval;
}





template <typename F>
class DeferHandle {
    F fn;
public:
    DeferHandle(F fn) : fn(fn) {}
    ~DeferHandle() {
        fn();
    }
};



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
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;



template <typename T>
struct is_shared_ptr_impl : std::false_type {};

template <typename T>
struct is_shared_ptr_impl<std::shared_ptr<T>> : std::true_type {};


template <typename T>
inline constexpr bool is_shared_ptr_v = is_shared_ptr_impl<remove_cvref_t<T>>::value;


template <typename T>
inline constexpr bool is_nullable_v = std::is_pointer_v<T> || is_shared_ptr_v<T>;


// Extracts a member pointer's class and member type

template <typename>
struct member_ptr;

template <typename C, typename M>
struct member_ptr<M C::*> {
    using ClassT = C;
    using MemberT = M;
};



// TODO properly implement the typename stuff!


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


} // namespace typeinfo






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

void pad_left(std::string &, size_t length, char c);
void pad_right(std::string &, size_t length, char c);

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
std::string path_get_filename(const std::string& path);
}


NS_END
