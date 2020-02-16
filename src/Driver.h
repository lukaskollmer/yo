//
//  Driver.h
//  yo
//
//  Created by Lukas Kollmer on 2020-02-15.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//


// TODO give this a nice API for programmatically invoking individual compiler instances

namespace yo::driver {
    int run(int argc, const char * argv[], const char *const *envp);
}
