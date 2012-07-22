{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy #-}

-- | Bridging to and from @NSArray@
module ObjectiveHaskell.NSArray (
        fromNSArray, toNSArray
    ) where

import Control.Monad
import Data.Foldable
import Data.Sequence as Seq
import Foreign.C.Types
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.NSObject
import ObjectiveHaskell.ObjC

-- NSArray methods
declMessage "array" [t| Class -> IO Id |] "array"
declMessage "addObject" [t| Id -> Id -> IO () |] "addObject:"
declMessage "objectAtIndex" [t| NSUInteger -> Id -> IO Id |] "objectAtIndex:"

-- | Converts an @NSArray@ into a 'Seq'.
fromNSArray :: Id -> IO (Seq Id)
fromNSArray arr = do
    c <- objc_count arr

    let fromNSArray' :: NSUInteger -> IO (Seq Id) -> IO (Seq Id)
        fromNSArray' i s =
            -- TODO: This should use something like fast enumeration instead (blocked on issue #1)
            let s' = liftM2 (|>) s $ arr @. objectAtIndex i
            in if i + 1 < c
                then fromNSArray' (i + 1) s'
                else s'
    
    fromNSArray' 0 (return empty)

-- | Converts a 'Seq' into an immutable @NSArray@.
toNSArray :: Seq Id -> IO Id
toNSArray s = do
    arr <- getClass "NSMutableArray" >>= array
    mapM (\obj -> arr @. addObject obj) $ toList s

    objc_copy arr

instance Bridged (Seq Id) where
    toObjC = toNSArray
    fromObjC = fromNSArray
