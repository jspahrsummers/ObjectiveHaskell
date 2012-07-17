-- TODO: this module can probably be eliminated
module ObjectiveHaskell.NSObject (
        objc_copy, objc_count
    ) where

import Foreign.C.Types
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

-- Some common messages for Objective-C objects,
-- including those that are not necessarily at the NSObject level.
declMessage "objc_copy" [t| Id -> IO Id |] "copy"
declMessage "objc_count" [t| Id -> IO NSUInteger |] "count"
