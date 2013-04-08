//
//  NSValue+OHExtensions.h
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 08.08.12.
//  Copyright (c) 2013 Justin Spahr-Summers. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <ObjectiveHaskell/OHBridged.h>

// Bridges between pointer `NSValue`s and Haskell `Ptr` values.
//
// Other NSValue types are not supported.
@interface NSValue (OHExtensions) <OHBridged>
@end
