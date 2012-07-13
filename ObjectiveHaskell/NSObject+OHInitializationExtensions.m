//
//  NSObject+OHInitializationExtensions.m
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 2012-07-13.
//  Released into the public domain.
//

#import <crt_externs.h>

@interface NSObject (OHInitializationExtensions)
@end

/**
 * This private category ensures that the Haskell runtime is loaded at about the
 * same time as the Objective-C runtime.
 */
@implementation NSObject (OHInitializationExtensions)

+ (void)load {
    hs_init(_NSGetArgc(), _NSGetArgv());

    // Shut down the Haskell runtime upon exit
    atexit(&hs_exit);
}

@end
