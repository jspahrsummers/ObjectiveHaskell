{-# LANGUAGE FlexibleInstances #-}
module ObjectiveHaskell.NSDictionary (
        fromNSDictionary, toNSDictionary
    ) where

import Control.Applicative
import Control.Monad
import Data.Foldable as Foldable
import Data.List
import Data.Map as Map
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

-- Converts an NSDictionary into a Map.
fromNSDictionary :: Id -> IO (Map Id Id)
fromNSDictionary dict = do
    keys <- Foldable.toList <$> (allKeys dict >>= fromNSArray)
    vals <- mapM (objectForKey dict) keys

    return $ Map.fromList $ zip keys vals

-- Converts a Map into an immutable NSDictionary.
toNSDictionary :: Map Id Id -> IO Id
toNSDictionary tbl = do
    dict <- getClass "NSMutableDictionary" >>= dictionary
    mapM (\(k, v) -> setObjectForKey dict v k) $ Map.toList tbl

    objc_copy dict

instance Bridged (Map Id Id) where
    toObjC = toNSDictionary
    fromObjC = fromNSDictionary
