//
//  LKUtilities.m
//  yo
//
//  Created by Lukas Kollmer on 08.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

#import "LKUtilities.h"


@implementation NSString (LKUtilities)

+ (instancetype)lk_stringByRepeatingCharacter:(unichar)character count:(NSUInteger)count {
    char bytes[count + 1];
    memset(bytes, 0, count + 1);
    
    for (NSUInteger i = 0; i < count; i++) {
        bytes[i] = character;
    }
    
    return [NSString stringWithUTF8String:bytes];
}

- (NSString *)lk_stringByTrimmingWhitespace {
    return [self stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
}


- (NSString *)lk_stringByTrimmingTrailingWhitespace {
    if ([self length] == 0) return self;
    
    NSInteger index = [self length] - 1;
    while (index >= 0 && [[NSCharacterSet whitespaceCharacterSet] characterIsMember:[self characterAtIndex:index]]) {
        index -= 1;
    }
    return [self substringToIndex:index + 1];
}


- (BOOL)lk_allCharactersInCharacterSet:(NSCharacterSet *)characterSet {
    for (NSUInteger i = 0; i < [self length]; i++) {
        if (![characterSet characterIsMember:[self characterAtIndex:i]]) {
            return NO;
        }
    }
    return YES;
}

@end


@implementation NSArray (LKUtilities)

- (NSArray *)lk_mapUsingBlock:(id  _Nonnull (^)(id _Nonnull))block {
    NSMutableArray *retval = [NSMutableArray new];
    for (id object in self) {
        [retval addObject:block(object)];
    }
    return retval;
}

@end


