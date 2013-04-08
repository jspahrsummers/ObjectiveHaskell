//
//  OHBridged.h
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 08.08.12.
//  Copyright (C) 2012 Justin Spahr-Summers.
//  Released under the MIT license.
//

#import <Foundation/Foundation.h>
#import <ObjectiveHaskell/OHTypes.h>

/*
 * Represents any class that can be bridged to and from Haskell.
 */
@protocol OHBridged <NSObject>
@required

/*
 * Returns an object that has the value wrapped by the given Haskell pointer.
 */
+ (instancetype)objectWithHaskellPointer:(OHHaskellPtr)haskellPointer;

/*
 * Returns a Haskell pointer corresponding to the receiver.
 */
- (OHHaskellPtr)haskellPointer; 

@end
