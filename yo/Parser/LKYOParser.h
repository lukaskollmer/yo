//
//  LKYOParser.h
//  yo
//
//  Created by Lukas Kollmer on 06.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

#import <Foundation/Foundation.h>
#include "mpc.h"

NS_ASSUME_NONNULL_BEGIN

@interface LKYOParser : NSObject

+ (instancetype)sharedInstance;

- (nullable mpc_ast_t *)parseFileAtPath:(NSString *)filePath;
- (void)freeAST:(mpc_ast_t *)ast;

@end

NS_ASSUME_NONNULL_END
