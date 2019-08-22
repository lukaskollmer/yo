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


void Scope::insert(const std::string &identifier, Type *type, ValueBinding binding) {
    auto B = std::make_shared<ValueBinding>(std::move(binding));
    entries.push_back({identifier, type, B});
}

ValueBinding *Scope::getBinding(const std::string &identifier) {
    return _getEntry(identifier)->binding.get();
}

Type *Scope::getType(const std::string &identifier) {
    return _getEntry(identifier)->type;
}


bool Scope::contains(const std::string &name) {
    return _getEntry(name) != nullptr;
}


Scope::Entry Scope::remove(const std::string &identifier) {
    std::vector<Entry>::const_iterator pos;
    auto entry = *_getEntry(identifier, &pos);
    entries.erase(pos);
    return entry;
}

Scope::Entry *Scope::_getEntry(const std::string &identifier, std::vector<Entry>::const_iterator *pos) {
    for (auto it = entries.end(); it-- != entries.begin();) {
        if (it->ident == identifier) {
            if (pos) *pos = it;
            return &*it;
        }
    }
    return nullptr;
}

Scope::Marker Scope::getMarker() {
    return entries.size();
}

std::vector<Scope::Entry> Scope::getEntriesSinceMarker(Marker M) {
    if (M >= entries.size()) return {};
    return std::vector<Entry>(entries.begin() + M, entries.end());
}
