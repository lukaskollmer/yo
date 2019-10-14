//
//  Scope.h
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


NS_START(yo::irgen)

struct ValueBinding {
    using ReadImp  = std::function<llvm::Value*(void)>;
    using WriteImp = std::function<void(llvm::Value*)>;
    
    const llvm::Value *value;
    const ReadImp  read;
    const WriteImp write;
    
    ValueBinding(llvm::Value *value, ReadImp read, WriteImp write) : value(value), read(read), write(write) {}
};



class Scope {
    struct Entry {
        std::string ident;
        Type *type;
        std::shared_ptr<ValueBinding> binding;
    };
    
    std::vector<Entry> entries;
    
public:
    using Marker = uint64_t;
    
    void insert(const std::string &identifier, Type *type, ValueBinding binding);
    
    // All of these return null if the scope doesn't contain the identifier
    bool contains(const std::string &name);
    ValueBinding *getBinding(const std::string &identifier);
    Type *getType(const std::string &identifier);
    
    Entry remove(const std::string &identifier);
    Entry *_getEntry(const std::string &odentifier, std::vector<Entry>::const_iterator *pos = nullptr);
    
    uint64_t size() { return entries.size(); }
    bool isEmpty() { return entries.empty(); }
    
    std::vector<Entry> getAllEntries() {
        return entries;
    }
    
    Marker getMarker();
    std::vector<Entry> getEntriesSinceMarker(Marker M);
};







template <typename T>
class NamedScope {
    using Entry = std::pair<std::string, T>;
    std::vector<Entry> entries;
    
public:
    using Marker = uint64_t;
    
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
    
    
    void removeAllSinceMarker(Marker M) {
        entries.erase(entries.begin() + M, entries.end());
    }
};


NS_END
