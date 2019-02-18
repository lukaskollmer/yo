//
//  LKUtilities.h
//  yo
//
//  Created by Lukas Kollmer on 08.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface NSString (LKUtilities)

+ (instancetype)lk_stringByRepeatingCharacter:(unichar)character count:(NSUInteger)count;

- (NSString *)lk_stringByTrimmingWhitespace;
- (NSString *)lk_stringByTrimmingTrailingWhitespace;
- (BOOL)lk_allCharactersInCharacterSet:(NSCharacterSet *)characterSet;

@end



@interface NSArray<__covariant ObjectType> (LKUtilities)

- (NSArray *)lk_mapUsingBlock:(id (^)(ObjectType))bloc;

@end

NS_ASSUME_NONNULL_END
