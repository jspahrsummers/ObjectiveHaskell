//
//  OHModel.m
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 2012-08-03.
//  Released into the public domain.
//

#import "OHModel.h"

@implementation OHModel

#pragma mark Lifecycle

- (id)init {
	// TODO
	return nil;
}

#pragma mark NSCopying

- (id)copyWithZone:(NSZone *)zone {
	return self;
}

#pragma mark NSObject

- (NSString *)description {
	// TODO
	return nil;
}

- (NSUInteger)hash {
	// TODO
	return 0;
}

- (BOOL)isEqual:(OHModel *)model {
	if (![model isMemberOfClass:self.class]) return NO;

	// TODO
	return YES;
}

@end
