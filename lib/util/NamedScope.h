//
//  NamedScope.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-07.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"

#include <tuple>
#include <vector>
#include <functional>
#include <utility>
#include <optional>


namespace yo {

template <typename T>
class NamedScope {
public:
    using ID = uint64_t;
    using Entry = std::tuple<std::string, ID, T>;
    
private:
    ID id = 0;
    std::vector<Entry> entries;
    
public:
    using Marker = uint64_t;
    
    bool isEmpty() const { return entries.empty(); }
    uint64_t size() const { return entries.size(); }
    
    ID insert(const std::string &ident, const T entry) {
        entries.emplace_back(ident, ++id, entry);
        return id;
    }
    
    bool contains(const std::string &ident) const {
        return util::vector::contains_where(entries, [&ident](const auto &entry) {
            return std::get<0>(entry) == ident;
        });
    }
    
    std::optional<T> get(const std::string &ident) const {
        for (auto it = entries.rbegin(); it != entries.rend(); it++) {
            if (std::get<0>(*it) == ident) {
                return std::get<2>(*it);
            }
        }
        return std::nullopt;
    }
    
    Marker getMarker() const {
        return entries.size();
    }
    
    const std::vector<Entry>& getEntries() const { return entries; }
    
    std::vector<Entry> getEntriesSinceMarker(Marker M) const {
        if (M >= entries.size()) return {};
        return std::vector<Entry>(entries.begin() + M, entries.end());
    }
    
    /// Remove the most recently added entry with the specified name
    void remove(const std::string &ident) {
        auto it = std::find_if(entries.rbegin(), entries.rend(), [&ident](const auto &entry) -> bool {
            return std::get<0>(entry) == ident;
        });
        
        if (it == entries.rend()) {
            LKFatalError("Cannot delete nonexistent entry with ident '%s'", ident.c_str());
        } else {
            entries.erase((it + 1).base());
        }
    }
    
    /// Remove an entry by id
    void remove(ID id) {
        for (auto it = entries.rbegin(); it != entries.rend(); it++) {
            if (std::get<1>(*it) == id) {
                entries.erase((it + 1).base());
                return;
            }
        }
    }
    
    /// Remove multiple entries by ids
    void removeAll(const std::vector<ID> &ids) {
        // TODO surely this can be implemented better?
        for (auto id : ids) {
            remove(id);
        }
    }
    
    /// Removes all entries *including* the marker
    void removeAllSinceMarker(Marker M) {
        entries.erase(entries.begin() + M, entries.end());
    }
    
    void removeAll() {
        entries = {};
    }
};


} // end namespace yo
