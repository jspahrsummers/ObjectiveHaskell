{-# LANGUAGE TemplateHaskell #-}

module MsgSendTest where

import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

declMethod "stringWithString" ''Id [''Id]

msgSendTest c s = stringWithString c s
foreign export ccall
    msgSendTest :: Id -> Sel -> Id -> IO Id
