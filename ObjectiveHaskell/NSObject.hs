module ObjectiveHaskell.NSObject (
        objc_copy, objc_count
    ) where

import Foreign.C.Types
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

-- Some common messages for Objective-C objects,
-- including those that are not necessarily at the NSObject level.
declMessage "objc_copy" "copy" ''Id []
declMessage "objc_count" "count" ''CSize []
