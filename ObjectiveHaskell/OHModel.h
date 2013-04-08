//
//  OHModel.h
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 2012-08-03.
//  Copyright (C) 2012 Justin Spahr-Summers.
//  Released under the MIT license.
//

#import <Foundation/Foundation.h>
#import <ObjectiveHaskell/OHBridged.h>
#import <ObjectiveHaskell/OHTypes.h>

/*
 * @haskellDynamic(Class, foobar) implements a property "foobar", corresponding
 * to a Haskell record field also named "foobar".
 *
 * In addition to generating a getter, this will create a private
 * -foobarUpdate: method, which returns a copy of the receiver with a new value
 * for "foobar". This generated method is used by -updateValue:forKey:.
 */
#define haskellDynamic(CLASS, PROPERTY) \
	dynamic PROPERTY; \
	\
	- (id)PROPERTY { \
		return CLASS ## _ ## PROPERTY(self.haskellPointer); \
	} \
	\
	- (instancetype)PROPERTY ## Update:(id)value { \
		OHHaskellPtr newPtr = CLASS ## _update_ ## PROPERTY(self.haskellPointer, value); \
		return [[self.class alloc] initWithHaskellPointer:newPtr]; \
	}

/*
 * A base class for models that are implemented in Haskell using the
 * ObjectiveHaskell.Model module.
 */
@interface OHModel : NSObject <NSCopying, OHBridged>

/*
 * Initializes the receiver with the value wrapped by the given Haskell pointer.
 *
 * This is the designated initializer for this class.
 */
- (instancetype)initWithHaskellPointer:(OHHaskellPtr)haskellPointer;

/*
 * The Haskell pointer to the value wrapped by the receiver.
 */
@property (nonatomic, assign, readonly) OHHaskellPtr haskellPointer; 

/*
 * A dictionary representing the properties of the receiver, along with their
 * associated values.
 *
 * This will only include the values associated with @property declarations.
 */
@property (nonatomic, copy, readonly) NSDictionary *dictionaryValue;

/*
 * Returns a copy of the receiver, with the given key updated to the given
 * value, or nil if an error occurs.
 *
 * The given key must refer to a property implemented with @haskellDynamic().
 */
- (instancetype)updateValue:(id)value forKey:(NSString *)key;

/*
 * Returns a copy of the receiver, with the given keys updated to their associated
 * values (converting any NSNull values to nil), or nil if an error occurs.
 *
 * All of the keys in the given dictionary must refer to properties implemented
 * with @haskellDynamic().
 */
- (instancetype)updateValuesForKeysWithDictionary:(NSDictionary *)keyValuePairs;

@end
