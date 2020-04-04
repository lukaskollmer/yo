//
//  VectorUtils.h
//  yo
//
//  Created by Lukas Kollmer on 2020-03-16.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"

#include <vector>
#include <optional>
#include <algorithm>
#include <iterator>
#include <type_traits>


NS_START(yo::util::vector)

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


/// Iterate over a vector, 1st `F` parameter is the current index
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

template <typename T>
void insert_at_front(std::vector<T> &vec, const T &elem) {
    vec.insert(vec.begin(), elem);
}

NS_END // yo::util::vector
