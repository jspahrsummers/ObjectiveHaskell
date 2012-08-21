//
//  OHTypes.h
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 2012-08-03.
//  Copyright (C) 2012 Justin Spahr-Summers.
//  Released under the MIT license.
//

#import "HsFFI.h"

// This makes Haskell functions compatible with ARC, but will effectively
// prevent ever passing "primitive" C pointers to or from Haskell
#define HsPtr id

typedef struct OHOpaqueHaskellType *OHHaskellPtr;

// This should be compatible with the normal typedef of HsStablePtr to void *,
// but prevents implicit conversions to other pointer types (because stable
// pointers shouldn't be inspected in any way)
#define HsStablePtr OHHaskellPtr
