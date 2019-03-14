//
//  Version.h
//  yo
//
//  Created by Lukas Kollmer on 2019-03-14.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#pragma once


#define YO_VERSION "0.0.1"

#define YO_LLVM_VERSION "7.0.1"

#define YO_SYSTEM_NAME "Darwin"


// Clang pretends to be gcc, so we have to check for that first
#if defined(__clang__)
#define COMPILER "Clang: " __clang_version__
#elif defined(__GNUC__)
#define COMPILER "GCC: " __VERSION__
#else
#define COMPILER "CXX: " __VERSION__
#endif
