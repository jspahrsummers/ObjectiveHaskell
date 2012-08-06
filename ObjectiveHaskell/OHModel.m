//
//  OHModel.m
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 2012-08-03.
//  Released into the public domain.
//

#import "OHModel.h"
#import "EXTScope.h"
#import <objc/runtime.h>

@implementation OHModel

#pragma mark Lifecycle

- (instancetype)init {
	NSAssert(NO, @"The designated initializer for OHModel is -initWithHaskellPointer:");
	return nil;
}

- (instancetype)initWithHaskellPointer:(OHOpaqueHaskellPtr)haskellPointer {
	self = [super init];
	if (self == nil) return nil;

	// TODO: This should return nil if given a Nothing.
	_haskellPointer = haskellPointer;
	return self;
}

#pragma mark Reflection

// The block given here may be invoked more than once for the same property
// name if that property is implemented on multiple classes in the hierarchy.
+ (void)enumeratePropertiesUsingBlock:(void (^)(objc_property_t property, BOOL *stop))block {
	Class cls = self;

	// Enumerate all superclasses up to the base class.
	while (![cls isEqual:[OHModel class]]) {
		unsigned count = 0;
		objc_property_t *properties = class_copyPropertyList(cls, &count);

		@onExit {
			free(properties);
		};

		for (unsigned i = 0; i < count; i++) {
			BOOL stop = NO;
			block(properties[i], &stop);

			if (stop) return;
		}

		cls = cls.superclass;
	}
}

- (NSDictionary *)dictionaryValue {
	NSMutableSet *propertyNames = [NSMutableSet set];

	[self.class enumeratePropertiesUsingBlock:^(objc_property_t property, BOOL *stop){
		[propertyNames addObject:@(property_getName(property))];
	}];

	return [self dictionaryWithValuesForKeys:propertyNames.allObjects];
}

#pragma mark Updating

- (instancetype)updateValue:(id)value forKey:(NSString *)key {
	SEL updateSelector = NSSelectorFromString([key stringByAppendingString:@"Update:"]);
	NSMethodSignature *signature = [self methodSignatureForSelector:updateSelector];
	if (signature == nil) {
		NSAssert(NO, @"%@ does not implement an update method for key \"%@\"", self, key);
		return nil;
	}

	NSInvocation *invocation = [NSInvocation invocationWithMethodSignature:signature];
	invocation.target = self;
	invocation.selector = updateSelector;
	[invocation setArgument:&value atIndex:2];
	[invocation invoke];

	__unsafe_unretained id result = nil;
	[invocation getReturnValue:&result];

	return result;
}

- (instancetype)updateValuesForKeysWithDictionary:(NSDictionary *)keyValuePairs {
	id model = self;

	for (NSString *key in keyValuePairs) {
		NSAssert([key isKindOfClass:[NSString class]], @"Non-string key %@ in dictionary %@", key, keyValuePairs);

		id value = [keyValuePairs objectForKey:key];
		if ([value isEqual:[NSNull null]]) value = nil;

		// TODO: This should update all the properties at once instead of one at
		// a time.
		model = [model updateValue:value forKey:key];
		if (model == nil) break;
	}

	return model;
}

#pragma mark NSCopying

- (id)copyWithZone:(NSZone *)zone {
	return self;
}

#pragma mark NSObject

- (NSString *)description {
	// TODO: This should use Haskell's Show class instead
	return [NSString stringWithFormat:@"<%@: %p>%@", self.class, self, self.dictionaryValue];
}

- (NSUInteger)hash {
	return self.dictionaryValue.hash;
}

- (BOOL)isEqual:(OHModel *)model {
	if (![model isMemberOfClass:self.class]) return NO;

	// TODO: This should use Haskell's Eq class instead
	return [self.dictionaryValue isEqualToDictionary:model.dictionaryValue];
}

@end
