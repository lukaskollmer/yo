//
//  NamedScope.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-07.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "util.h"
#include "Format.h"
#include "Counter.h"
#include "VectorUtils.h"

#include <string>
#include <tuple>
#include <vector>
#include <algorithm>
#include <optional>


namespace yo::util {


template <typename Value, typename Key = std::string>
class NamedScope {
public:
    using ID = uint64_t;
    using Entry = std::tuple<Key, ID, Value>;
    
private:
    Counter<ID> idCounter;
    std::vector<Entry> entries;
    
public:
    using Marker = uint64_t;
    
    bool isEmpty() const {
        return entries.empty();
    }
    
    uint64_t size() const {
        return entries.size();
    }
    
    ID insert(const Key &key, const Value &value) {
        auto id = idCounter.increment();
        entries.emplace_back(key, id, value);
        return id;
    }
    
    bool contains(const Key &key) const {
        return util::vector::contains_where(entries, [&key](const auto &entry) {
            return std::get<0>(entry) == key;
        });
    }
    
    std::optional<Value> get(const Key &key) const {
        for (auto it = entries.rbegin(); it != entries.rend(); it++) {
            if (std::get<0>(*it) == key) {
                return std::get<2>(*it);
            }
        }
        return std::nullopt;
    }
    
    Marker getMarker() const {
        return entries.size();
    }
    
    const std::vector<Entry>& getEntries() const {
        return entries;
    }
    
    std::vector<Entry> getEntriesSinceMarker(Marker M) const {
        if (M >= entries.size()) return {};
        return std::vector<Entry>(entries.begin() + M, entries.end());
    }
    
    /// Remove the most recently added entry with the specified key
    void remove(const Key &key) {
        auto it = std::find_if(entries.rbegin(), entries.rend(), [&key](const auto &entry) -> bool {
            return std::get<0>(entry) == key;
        });
        
        if (it == entries.rend()) {
            auto msg = fmt::format("cannot delete nonexistent entru with key '{}'", key);
            LKFatalError("%s", msg.c_str());
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


} // ns yo::util
