{-# LANGUAGE OverloadedStrings #-}

module BridgedObjectTest where

import Control.Applicative
import Data.ByteString.Lazy as ByteString
import Data.Map as Map
import Data.Sequence as Seq
import Data.Text.Lazy as Text
import Foreign.C.Types
import Foreign.Ptr
import ObjectiveHaskell.NSArray
import ObjectiveHaskell.NSData
import ObjectiveHaskell.NSDictionary
import ObjectiveHaskell.NSString
import ObjectiveHaskell.NSValue
import ObjectiveHaskell.ObjC

appendFoobar' :: Id -> IO Id
appendFoobar' nsstr = do
    str <- fromNSString nsstr
    toNSString $ Text.append str "foobar"

addFoobarToArray' :: Id -> IO Id
addFoobarToArray' nsarr = do
    s <- fromNSArray nsarr
    foobar <- toNSString "foobar"

    toNSArray $ s |> foobar

setFooToBar' :: Id -> IO Id
setFooToBar' nsdict = do
    tbl <- fromNSDictionary nsdict
    foo <- toNSString "foo"
    bar <- toNSString "bar"

    toNSDictionary $ insert foo bar tbl

appendByte' :: Id -> CUChar -> IO Id
appendByte' nsdata b = do
    str <- fromNSData nsdata
    toNSData $ ByteString.snoc str (fromIntegral b)

nullNSValue' :: IO Id
nullNSValue' = toNSValue nullPtr

ptrAddress' :: Id -> IO CUIntPtr
ptrAddress' obj = (fromIntegral . ptrToWordPtr) <$> fromNSValue obj

exportFunc "appendFoobar" [t| UnsafeId -> IO UnsafeId |] 'appendFoobar'
exportFunc "addFoobarToArray" [t| UnsafeId -> IO UnsafeId |] 'addFoobarToArray'
exportFunc "setFooToBar" [t| UnsafeId -> IO UnsafeId |] 'setFooToBar'
exportFunc "appendByte" [t| UnsafeId -> CUChar -> IO UnsafeId |] 'appendByte'
exportFunc "nullNSValue" [t| IO UnsafeId |] 'nullNSValue'
exportFunc "ptrAddress" [t| UnsafeId -> IO CUIntPtr |] 'ptrAddress'
