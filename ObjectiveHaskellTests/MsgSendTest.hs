{-# LANGUAGE TemplateHaskell #-}

module MsgSendTest where

import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

declMethod "stringWithString" ''Id [''Id]

msgSendTest s = do
    sel <- selector "stringWithString:"
    stringWithString (getClass "NSString") sel s

foreign export ccall
    msgSendTest :: Id -> IO Id
