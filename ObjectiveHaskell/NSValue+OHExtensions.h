//
//  NSValue+OHExtensions.h
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 08.08.12.
//  Copyright (c) 2012 Justin Spahr-Summers. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <ObjectiveHaskell/OHBridged.h>

/*
 * Bridges between pointer NSValues and Haskell Ptr values.
 *
 * Other NSValue types are not supported.
 */
@interface NSValue (OHExtensions) <OHBridged>
@end
