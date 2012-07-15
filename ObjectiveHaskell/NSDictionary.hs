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
declMessage "allKeys" [t| Id -> IO Id |] "allKeys"
declMessage "dictionary" [t| Id -> IO Id |] "dictionary"
declMessage "objectForKey" [t| Id -> Id -> IO Id |] "objectForKey:"
declMessage "setObjectForKey" [t| Id -> Id -> Id -> IO () |] "setObject:forKey:"

-- Converts an NSDictionary into a Map.
fromNSDictionary :: Id -> IO (Map Id Id)
fromNSDictionary dict = do
    keys <- Foldable.toList <$> (dict @. allKeys >>= fromNSArray)
    vals <- mapM (\k -> dict @. objectForKey k) keys

    return $ Map.fromList $ zip keys vals

-- Converts a Map into an immutable NSDictionary.
toNSDictionary :: Map Id Id -> IO Id
toNSDictionary tbl = do
    dict <- getClass "NSMutableDictionary" >>= dictionary
    mapM (\(k, v) -> dict @. setObjectForKey v k) $ Map.toList tbl

    objc_copy dict

instance Bridged (Map Id Id) where
    toObjC = toNSDictionary
    fromObjC = fromNSDictionary
