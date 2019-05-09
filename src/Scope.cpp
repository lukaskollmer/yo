//
//  Scope.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-07.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Scope.h"

using namespace yo;
using namespace irgen;


void Scope::Insert(const std::string &Identifier, TypeInfo *Type, ValueBinding Binding) {
    auto B = std::make_shared<ValueBinding>(std::move(Binding));
    Entries.push_back({Identifier, Type, B});
}

ValueBinding *Scope::GetBinding(const std::string &Identifier) {
    return _GetEntry(Identifier)->Binding.get();
}

TypeInfo *Scope::GetType(const std::string &Identifier) {
    return _GetEntry(Identifier)->Type;
}


bool Scope::Contains(const std::string &name) {
    return _GetEntry(name) != nullptr;
}


Scope::Entry Scope::Remove(const std::string &Identifier) {
    std::vector<Entry>::const_iterator Pos;
    auto E = *_GetEntry(Identifier, &Pos);
    Entries.erase(Pos);
    return E;
}

Scope::Entry *Scope::_GetEntry(const std::string &Identifier, std::vector<Entry>::const_iterator *Pos) {
    for (auto It = Entries.end(); It-- != Entries.begin();) {
        if (It->Ident == Identifier) {
            if (Pos) *Pos = It;
            return &*It;
        }
    }
    return nullptr;
}

Scope::Marker Scope::GetMarker() {
    return Entries.size();
}

std::vector<Scope::Entry> Scope::GetEntriesSinceMarker(Marker M) {
    if (M >= Entries.size()) return {};
    return std::vector<Entry>(Entries.begin() + M, Entries.end());
}
