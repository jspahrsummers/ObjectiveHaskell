{-# LANGUAGE FlexibleInstances #-}
module ObjectiveHaskell.NSValue (
        fromNSValue, toNSValue
    ) where

import Foreign.Ptr
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

-- NSValue methods
declMessage "valueWithPointer" [t| Ptr () -> Id -> IO Id |] "valueWithPointer:"
declMessage "pointerValue" [t| Id -> IO (Ptr ()) |] "pointerValue"

-- Converts a pointer NSValue into a raw Ptr ()
fromNSValue :: Id -> IO (Ptr ())
fromNSValue val = val @. pointerValue

-- Converts a Ptr () into an NSValue
toNSValue :: Ptr () -> IO Id
toNSValue ptr = getClass "NSValue" >>= valueWithPointer ptr

instance Bridged (Ptr ()) where
    toObjC = toNSValue
    fromObjC = fromNSValue
