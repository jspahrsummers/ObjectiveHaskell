{-# LANGUAGE TemplateHaskell #-}

module Collections (
        fromNSArray, fromNSDictionary
    ) where

import Data.Foldable as Foldable
import Data.HashTable as HashTable
import Data.Int
import Data.List as List
import Data.Sequence as Seq
import Control.Applicative
import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Unsafe
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

declMessage "objc_count" "count" ''CSize []
declMessage "objc_hash" "hash" ''CSize []

declMessage "allKeys" "allKeys" ''Id []
declMessage "objectAtIndex" "objectAtIndex:" ''Id [''CSize]
declMessage "objectForKey" "objectForKey:" ''Id [''Id]

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
    
    fromNSArray' 0 (return Seq.empty)

-- Hashes an Objective-C object.
-- This depends on an object's -hash remaining the same (which is required for NSDictionary anyways).
hash :: Id -> Int32
hash obj = fromIntegral $ unsafeLocalState $ objc_hash obj

-- Converts an NSDictionary into a HashTable.
fromNSDictionary :: Id -> IO (HashTable Id Id)
fromNSDictionary dict = do
    keys <- Foldable.toList <$> (allKeys dict >>= fromNSArray)
    vals <- mapM (objectForKey dict) keys

    HashTable.fromList hash $ List.zip keys vals
