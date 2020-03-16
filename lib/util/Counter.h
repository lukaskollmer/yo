//
//  Counter.h
//  yo
//
//  Created by Lukas Kollmer on 2020-03-16.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#pragma once

#include <cstdint>

namespace yo {
namespace util {


template <typename T = uint64_t>
class Counter {
    T value;
    
public:
    Counter() : value(0) {}
    explicit Counter(T initial) : value(initial) {}
    
    T currentValue() const {
        return value;
    }
    
    T increment() {
        T val = value;
        value += 1;
        return val;
    }
};



}
}
