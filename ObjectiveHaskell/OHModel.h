//
//  OHModel.h
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 2012-08-03.
//  Released into the public domain.
//

#import <Foundation/Foundation.h>
#import "OHTypes.h"

/*
 * @haskellDynamic(Class, foobar) implements a getter -[Class foobar], which will
 * call the Haskell function Class_foobar(), passing self.haskellPointer as the
 * only argument, and return the result.
 */
#define haskellDynamic(CLASS, PROPERTY) \
	dynamic PROPERTY; \
	\
	- (id)PROPERTY { \
		return CLASS ## _ ## PROPERTY(self.haskellPointer); \
	}

/*
 * A base class for models that are implemented in Haskell using the
 * ObjectiveHaskell.Model module.
 */
@interface OHModel : NSObject <NSCopying>

/*
 * Initializes the receiver with the value wrapped by the given Haskell pointer.
 *
 * This is the designated initializer for this class.
 */
- (id)initWithHaskellPointer:(OHOpaqueHaskellPtr)haskellPointer;

/*
 * The Haskell pointer to the value wrapped by the receiver.
 */
@property (nonatomic, assign, readonly) OHOpaqueHaskellPtr haskellPointer; 

/*
 * A dictionary representing the properties of the receiver, along with their
 * associated values.
 *
 * This will only include the values associated with @property declarations.
 */
@property (nonatomic, copy, readonly) NSDictionary *dictionaryValue;

@end
