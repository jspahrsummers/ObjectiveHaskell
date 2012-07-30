{-# LANGUAGE Trustworthy #-}

-- | Common NSURL messages
module ObjectiveHaskell.NSURL (
        stringFromUrl, urlFromString
    ) where

import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

-- NSURL methods
declMessage "urlWithString" [t| Id -> Class -> IO Id |] "URLWithString:"

-- | Converts an @NSURL@ into an @NSString@.
declMessage "stringFromUrl" [t| Id -> IO Id |] "absoluteString"

-- | Converts an @NSString@ into an @NSURL@.
urlFromString :: Id -> IO Id
urlFromString str = getClass "NSURL" >>= urlWithString str
