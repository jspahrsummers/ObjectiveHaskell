{-# LANGUAGE TemplateHaskell #-}

module ObjectiveHaskell.NSDictionary (
        fromNSDictionary, toNSDictionary
    ) where

import Control.Applicative
import Control.Monad
import Data.Foldable as Foldable
import Data.List
import Data.HashTable as HashTable
import Foreign.C.Types
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.NSArray
import ObjectiveHaskell.NSObject
import ObjectiveHaskell.ObjC

-- NSDictionary methods
declMessage "allKeys" "allKeys" ''Id []
declMessage "dictionary" "dictionary" ''Id []
declMessage "objectForKey" "objectForKey:" ''Id [''Id]
declMessage "setObjectForKey" "setObject:forKey:" ''() [''Id, ''Id]

-- Converts an NSDictionary into a HashTable.
fromNSDictionary :: Id -> IO (HashTable Id Id)
fromNSDictionary dict = do
    keys <- Foldable.toList <$> (allKeys dict >>= fromNSArray)
    vals <- mapM (objectForKey dict) keys

    HashTable.fromList hash $ zip keys vals

-- Converts a HashTable into an immutable NSDictionary.
toNSDictionary :: HashTable Id Id -> IO Id
toNSDictionary tbl = do
    dict <- getClass "NSMutableDictionary" >>= dictionary

    pairs <- HashTable.toList tbl
    mapM (\(k, v) -> setObjectForKey dict v k) pairs

    objc_copy dict
