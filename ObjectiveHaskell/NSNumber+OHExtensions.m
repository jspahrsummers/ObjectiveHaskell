//
//  NSNumber+OHExtensions.m
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 08.08.12.
//  Copyright (C) 2013 Justin Spahr-Summers.
//

#import "NSNumber+OHExtensions.h"
#import "NSNumber_stub.h"

@implementation NSNumber (OHExtensions)

+ (instancetype)objectWithHaskellPointer:(OHHaskellPtr)haskellPointer {
	return OHNSNumberFromHaskellPtr(haskellPointer);
}

- (OHHaskellPtr)haskellPointer {
	return OHHaskellPtrFromNSNumber(self);
}

@end
