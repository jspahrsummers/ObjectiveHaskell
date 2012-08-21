//
//  NSObject+OHExtensions.m
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 2012-07-13.
//  Copyright (C) 2012 Justin Spahr-Summers.
//  Released under the MIT license.
//

#import <crt_externs.h>

@interface NSObject (OHExtensions)
@end

/**
 * This private category ensures that the Haskell runtime is loaded at about the
 * same time as the Objective-C runtime.
 */
@implementation NSObject (OHExtensions)

+ (void)load {
    hs_init(_NSGetArgc(), _NSGetArgv());

    // Shut down the Haskell runtime upon exit
    atexit(&hs_exit);
}

@end
