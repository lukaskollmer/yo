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

#include "util.h"



NS_START(irgen)




struct ValueBinding {
    using ReadImp  = std::function<llvm::Value*(void)>;
    using WriteImp = std::function<void(llvm::Value*)>;
    
    const ReadImp  Read;
    const WriteImp Write;
    const llvm::Value *Value;
    
    ValueBinding(llvm::Value *Value, ReadImp Read, WriteImp Write) : Value(Value), Read(Read), Write(Write) {}
};



class Scope {
    using Ident = std::string;
    using Entry = std::tuple<std::string, llvm::Type *, std::shared_ptr<ValueBinding>>;
    using V = std::vector<Entry>;
    
    V Symbols;
    
public:
    using Marker = uint64_t;
    
    void Insert(Ident Identifier, llvm::Type *Type, ValueBinding Binding);
    
    // All of these return null if the scope doesn't contain the identifier
    ValueBinding *GetBinding(Ident Identifier);
    llvm::Type *GetType(Ident Identifier);
    
    Entry Remove(Ident Identifier);
    Entry *_GetEntry(Ident Identifier, V::const_iterator *Pos = nullptr);
    
    bool IsEmpty() { return Symbols.empty(); }
    void Clear() { Symbols.clear(); }
    
    
    Marker GetMarker();
    V GetEntriesSinceMarker(Marker M);
};





NS_END
