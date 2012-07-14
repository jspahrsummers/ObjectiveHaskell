{-# LANGUAGE TemplateHaskell #-}

module MsgSendTest where

import Control.Applicative
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

declMethod "stringWithString" "stringWithString:" ''Id [''Id]

msgSendTest unsafeStr = do
    str <- retainedId unsafeStr
    cl <- getClass "NSMutableString"

    cl `stringWithString` str >>= autorelease

foreign export ccall
    msgSendTest :: UnsafeId -> IO UnsafeId
