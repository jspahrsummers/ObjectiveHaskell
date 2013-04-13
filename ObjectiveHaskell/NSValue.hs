{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

-- | Bridging to and from @NSValue@
module ObjectiveHaskell.NSValue (
        fromNSValue, toNSValue
    ) where

import Foreign.Ptr
import Foreign.StablePtr
import ObjectiveHaskell.TH.MsgSend
import ObjectiveHaskell.TH.ObjC

-- NSValue methods
declMessage "valueWithPointer" [t| Ptr () -> Id -> IO Id |] "valueWithPointer:"
declMessage "pointerValue" [t| Id -> IO (Ptr ()) |] "pointerValue"

-- | Returns the @-pointerValue@ of an @NSValue@.
fromNSValue :: Id -> IO (Ptr ())
fromNSValue val = val @. pointerValue

-- | Returns an @NSValue@ wrapping a pointer.
toNSValue :: Ptr () -> IO Id
toNSValue ptr = getClass "NSValue" >>= valueWithPointer ptr

instance Bridged (Ptr ()) where
    toObjC = toNSValue
    fromObjC = fromNSValue

fromNSValueObjC :: Id -> IO (StablePtr (Ptr ()))
fromNSValueObjC obj = fromNSValue obj >>= newStablePtr

toNSValueObjC :: StablePtr (Ptr ()) -> IO Id
toNSValueObjC ptr = deRefStablePtr ptr >>= toNSValue

exportFunc "OHHaskellPtrFromNSValue" [t| UnsafeId -> IO (StablePtr (Ptr ())) |] 'fromNSValueObjC
exportFunc "OHNSValueFromHaskellPtr" [t| StablePtr (Ptr ()) -> IO UnsafeId |] 'toNSValueObjC
