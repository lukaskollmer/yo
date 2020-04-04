//
//  MapUtils.h
//  yo
//
//  Created by Lukas Kollmer on 2020-03-16.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"

#include <map>
#include <optional>


namespace yo::util::map {

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


template <typename K, typename V, typename F>
bool contains_where(const std::map<K, V> &map, F &&fn) {
    for (auto &[key, value] : map) {
        if (fn(key, value)) return true;
    }
    return false;
}


template <typename K, typename V, typename F>
void iterl(const std::map<K, V> &map, F &&fn) {
    for (auto it = map.begin(); it != map.end();) {
        fn(it->first, it->second, ++it == map.end());
    }
}

} // yo::util::map
