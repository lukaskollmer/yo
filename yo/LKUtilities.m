//
//  LKUtilities.m
//  yo
//
//  Created by Lukas Kollmer on 08.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

#import "LKUtilities.h"


@implementation NSString (LKUtilities)

- (NSString *)stringByTrimmingWhitespace {
    return [self stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
}

@end


