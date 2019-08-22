//
//  Scope.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-07.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <tuple>
#include <vector>
#include <functional>
#include "llvm/IR/Value.h"
#include "Type.h"

#include "util.h"


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

NS_END
