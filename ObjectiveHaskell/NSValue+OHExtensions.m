//
//  NSValue+OHExtensions.m
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 08.08.12.
//  Copyright (c) 2013 Justin Spahr-Summers. All rights reserved.
//

#import "NSValue+OHExtensions.h"
#import "NSValue_stub.h"

@implementation NSValue (OHExtensions)

+ (instancetype)objectWithHaskellPointer:(OHHaskellPtr)haskellPointer {
	return OHNSValueFromHaskellPtr(haskellPointer);
}

- (OHHaskellPtr)haskellPointer {
	NSAssert(self.objCType[0] == '^', @"Can only get the Haskell pointer for pointer NSValues, not %@", self);
	return OHHaskellPtrFromNSValue(self);
}

@end
