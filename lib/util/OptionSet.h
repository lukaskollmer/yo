//
//  OptionSet.h
//  yo
//
//  Created by Lukas Kollmer on 2020-03-16.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <type_traits>
#include <initializer_list>

namespace yo::util {


/// An `OptionSet` is an object which stores a bitfield of enum values
/// Note that, for this to be used as intended, the enum's values need to be powers of 2
template <typename Flags>
class OptionSet {
    using Data = std::make_unsigned_t<std::underlying_type_t<Flags>>;
    Data data;
    
public:
    OptionSet() : data(0) {}
    
    OptionSet(Flags flags) : data(static_cast<Data>(flags)) {}
    
    OptionSet(std::initializer_list<Flags> flags) : data(0) {
        for (Flags F : flags) {
            insert(F);
        }
    }
    
    bool isEmpty() const {
        return data == 0;
    }
    
    bool contains(Flags val) const {
        return data & static_cast<Data>(val);
    }
    
    void insert(Flags val) {
        data |= static_cast<Data>(val);
    }
    
    bool remove(Flags val) {
        data &= ~static_cast<Data>(val);
    }
};



} // ns yo::util

