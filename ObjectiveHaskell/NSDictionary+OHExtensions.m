//
//  NSDictionary+OHExtensions.m
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 08.08.12.
//  Copyright (C) 2013 Justin Spahr-Summers.
//

#import "NSDictionary+OHExtensions.h"
#import "NSDictionary_stub.h"

@implementation NSDictionary (OHExtensions)

+ (instancetype)objectWithHaskellPointer:(OHHaskellPtr)haskellPointer {
	return OHNSDictionaryFromHaskellPtr(haskellPointer);
}

- (OHHaskellPtr)haskellPointer {
	return OHHaskellPtrFromNSDictionary(self);
}

@end
