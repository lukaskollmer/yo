//
//  Scope.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-07.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Scope.h"

using namespace irgen;



void Scope::Insert(Ident Identifier, llvm::Type *Type, ValueBinding Binding) {
    auto B = std::make_shared<ValueBinding>(std::move(Binding));
    Symbols.push_back({Identifier, Type, B});
}


Scope::Entry Scope::Remove(Ident Identifier) {
    V::const_iterator Pos;
    auto E = *_GetEntry(Identifier, &Pos);
    Symbols.erase(Pos);
    return E;
}


Scope::Entry *Scope::_GetEntry(Ident Identifier, V::const_iterator *Pos) {
    for (auto It = Symbols.end(); It-- != Symbols.begin();) {
        if (std::get<0>(*It) == Identifier) {
            if (Pos) *Pos = It;
            return &*It;
        }
    }
    return nullptr;
}


Scope::Marker Scope::GetMarker() {
    return Symbols.size();
}


Scope::V Scope::GetEntriesSinceMarker(Marker M) {
    if (M >= Symbols.size()) return {};
    return Scope::V(Symbols.begin() + M, Symbols.end());
}


