//
//  NSData+OHExtensions.m
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 08.08.12.
//  Copyright (C) 2013 Justin Spahr-Summers.
//

#import "NSData+OHExtensions.h"
#import "NSData_stub.h"

@implementation NSData (OHExtensions)

+ (instancetype)objectWithHaskellPointer:(OHHaskellPtr)haskellPointer {
	return OHNSDataFromHaskellPtr(haskellPointer);
}

- (OHHaskellPtr)haskellPointer {
	return OHHaskellPtrFromNSData(self);
}

@end
