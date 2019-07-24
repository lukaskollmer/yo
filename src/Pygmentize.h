//
//  Pygmentize.h
//  yo
//
//  Created by Lukas Kollmer on 2019-07-23.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Token.h"
#include <string>

namespace yo::lex {
    std::string pygmentize(const parser::TokenList &tokens);
}
