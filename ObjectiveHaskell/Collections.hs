{-# LANGUAGE TemplateHaskell #-}

module Collections (
        fromNSArray, fromNSDictionary,
        toNSArray, toNSDictionary
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

-- Generic methods
-- TODO: move these into a module suitable for user importing
declMessage "objc_copy" "copy" ''Id []
declMessage "objc_count" "count" ''CSize []
declMessage "objc_hash" "hash" ''CSize []

-- TODO: split NSArray and NSDictionary into two new modules

-- NSArray methods
declMessage "array" "array" ''Id []
declMessage "addObject" "addObject:" ''() [''Id]
declMessage "objectAtIndex" "objectAtIndex:" ''Id [''CSize]

-- NSDictionary methods
declMessage "allKeys" "allKeys" ''Id []
declMessage "dictionary" "dictionary" ''Id []
declMessage "objectForKey" "objectForKey:" ''Id [''Id]
declMessage "setObjectForKey" "setObject:forKey:" ''() [''Id, ''Id]

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

-- Converts a Seq into an immutable NSArray.
toNSArray :: Seq Id -> IO Id
toNSArray s = do
    arr <- getClass "NSMutableArray" >>= array
    mapM (addObject arr) $ Foldable.toList s

    objc_copy arr

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

-- Converts a HashTable into an immutable NSDictionary.
toNSDictionary :: HashTable Id Id -> IO Id
toNSDictionary tbl = do
    dict <- getClass "NSMutableDictionary" >>= dictionary

    pairs <- HashTable.toList tbl
    mapM (\(k, v) -> setObjectForKey dict v k) pairs

    objc_copy dict
