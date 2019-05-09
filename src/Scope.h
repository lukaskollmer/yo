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
#include "TypeInfo.h"

#include "util.h"


NS_START(yo::irgen)

struct ValueBinding {
    using ReadImp  = std::function<llvm::Value*(void)>;
    using WriteImp = std::function<void(llvm::Value*)>;
    
    const llvm::Value *Value;
    const ReadImp  Read;
    const WriteImp Write;
    
    ValueBinding(llvm::Value *Value, ReadImp Read, WriteImp Write) : Value(Value), Read(Read), Write(Write) {}
};



class Scope {
    struct Entry {
        std::string Ident;
        TypeInfo *Type;
        std::shared_ptr<ValueBinding> Binding;
    };
    
    std::vector<Entry> Entries;
    
public:
    using Marker = uint64_t;
    
    void Insert(const std::string &Identifier, TypeInfo *Type, ValueBinding Binding);
    
    // All of these return null if the scope doesn't contain the identifier
    bool Contains(const std::string &name);
    ValueBinding *GetBinding(const std::string &Identifier);
    TypeInfo *GetType(const std::string &Identifier);
    
    Entry Remove(const std::string &Identifier);
    Entry *_GetEntry(const std::string &Identifier, std::vector<Entry>::const_iterator *Pos = nullptr);
    
    uint64_t size() { return Entries.size(); }
    bool isEmpty() { return Entries.empty(); }
    
    std::vector<Entry> GetAllEntries() {
        return Entries;
    }
    
    Marker GetMarker();
    std::vector<Entry> GetEntriesSinceMarker(Marker M);


};

NS_END
