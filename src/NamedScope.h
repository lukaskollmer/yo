//
//  NamedScope.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-07.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"
#include "Type.h"
#include "llvm/IR/Value.h"

#include <tuple>
#include <vector>
#include <functional>
#include <utility>
#include <optional>


namespace yo {

template <typename T>
class NamedScope {
    using Entry = std::pair<std::string, T>;
    std::vector<Entry> entries;
    
public:
    using Marker = uint64_t;
    
    bool isEmpty() const { return entries.empty(); }
    uint64_t size() const { return entries.size(); }
    
    void insert(const std::string &ident, const T entry) {
        entries.emplace_back(std::make_pair(ident, entry));
    }
    
    bool contains(const std::string &ident) const {
        return util::vector::contains_where(entries, [&ident](const auto &entry) {
            return std::get<0>(entry) == ident;
        });
    }
    
    std::optional<T> get(const std::string &ident) const {
        for (auto it = entries.rbegin(); it != entries.rend(); it++) {
            if (it->first == ident) {
                return it->second;
            }
        }
        return std::nullopt;
    }
    
    Marker getMarker() const {
        return entries.size();
    }
    
    std::vector<Entry> getEntriesSinceMarker(Marker M) const {
        if (M >= entries.size()) return {};
        return std::vector<Entry>(entries.begin() + M, entries.end());
    }
    
    void remove(const std::string &ident) {
        auto it = std::find_if(entries.rbegin(), entries.rend(), [ident](const auto &entry) -> bool {
            return entry.first == ident;
        });
        
        if (it == entries.rend()) {
            LKFatalError("Cannot delete nonexistent entry with ident '%s'", ident.c_str());
        } else {
            entries.erase((it + 1).base());
        }
    }
    
    void removeAllSinceMarker(Marker M) {
        entries.erase(entries.begin() + M, entries.end());
    }
    
    void removeAll() {
        entries = {};
    }
};


} // end namespace yo
