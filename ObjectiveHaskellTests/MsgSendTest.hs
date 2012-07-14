{-# LANGUAGE TemplateHaskell #-}

module MsgSendTest where

import Control.Applicative
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

declMethod "stringWithString" ''Id [''Id]

msgSendTest unsafeStr = do
    str <- retainedId unsafeStr
    cl <- getClass "NSString"
    sel <- selector "stringWithString:"

    stringWithString cl sel str >>= autorelease

foreign export ccall
    msgSendTest :: UnsafeId -> IO UnsafeId
