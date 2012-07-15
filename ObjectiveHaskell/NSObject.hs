{-# LANGUAGE TemplateHaskell #-}

module ObjectiveHaskell.NSObject (
        hash, objc_copy, objc_count, objc_hash
    ) where

import Data.Int
import Foreign.C.Types
import Foreign.Marshal.Unsafe
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

-- Some common messages for Objective-C objects,
-- including those that are not necessarily at the NSObject level.
declMessage "objc_copy" "copy" ''Id []
declMessage "objc_count" "count" ''CSize []
declMessage "objc_hash" "hash" ''CSize []

-- Hashes an Objective-C object.
-- This depends on an object's -hash remaining the same (which is required for NSDictionary anyways).
hash :: Id -> Int32
hash obj = fromIntegral $ unsafeLocalState $ objc_hash obj
