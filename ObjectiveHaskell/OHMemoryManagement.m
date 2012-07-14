//
//  OHMemoryManagement.m
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 2012-07-14.
//  Released into the public domain.
//

#import <Foundation/Foundation.h>
#import "OHMemoryManagement.h"

#if __has_feature(objc_arc)
#error "This file must be compiled without ARC."
#endif

void *OHRetain (void *obj) {
    return [(id)obj retain];
}

void OHRelease (void *obj) {
    [(id)obj release];
}
