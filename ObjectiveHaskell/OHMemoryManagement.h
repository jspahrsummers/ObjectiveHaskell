//
//  OHMemoryManagement.h
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 2012-07-14.
//  Released into the public domain.
//

/*
 * This header is associated with an Objective-C implementation, but needs to be
 * exposed as C for the ObjectiveHaskell.ObjC Haskell module.
 *
 * In other words, there should be no use of Objective-C constructs in here.
 */

extern void *OHRetain (void *obj);
extern void OHRelease (void *obj);
