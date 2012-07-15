{-# LANGUAGE TemplateHaskell #-}

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
declMessage "array" "array" ''Id []
declMessage "addObject" "addObject:" ''() [''Id]
declMessage "objectAtIndex" "objectAtIndex:" ''Id [''CSize]

-- Converts an NSArray into a Seq.
fromNSArray :: Id -> IO (Seq Id)
fromNSArray arr = do
    c <- objc_count arr

    let fromNSArray' :: CSize -> IO (Seq Id) -> IO (Seq Id)
        fromNSArray' i s =
            -- TODO: This should use something like fast enumeration instead (blocked on issue #1)
            let s' = liftM2 (|>) s $ arr `objectAtIndex` i
            in if i + 1 < c
                then fromNSArray' (i + 1) s'
                else s'
    
    fromNSArray' 0 (return empty)

-- Converts a Seq into an immutable NSArray.
toNSArray :: Seq Id -> IO Id
toNSArray s = do
    arr <- getClass "NSMutableArray" >>= array
    mapM (addObject arr) $ toList s

    objc_copy arr
