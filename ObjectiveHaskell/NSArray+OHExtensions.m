//
//  NSArray+OHExtensions.m
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 08.08.12.
//  Released into the public domain.
//

#import "NSArray+OHExtensions.h"
#import "NSArray_stub.h"

@implementation NSArray (OHExtensions)

+ (instancetype)objectWithHaskellPointer:(OHHaskellPtr)haskellPointer {
	return OHNSArrayFromHaskellPtr(haskellPointer);
}

- (OHHaskellPtr)haskellPointer {
	return OHHaskellPtrFromNSArray(self);
}

@end
