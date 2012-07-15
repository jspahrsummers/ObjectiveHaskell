{-# LANGUAGE TemplateHaskell #-}

module MsgSendTest where

import Control.Monad
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

declMessage "stringWithString" "stringWithString:" ''Id [''Id]

-- This is what a Haskell function that invokes Objective-C code might look like.
mutableStringWithString :: Id -> IO Id
mutableStringWithString str = do
    cl <- getClass "NSMutableString"
    cl `stringWithString` str

foreign export ccall
    msgSendTest :: UnsafeId -> IO UnsafeId

-- Bridging points where Objective-C might pass in objects require a bit more work,
-- to map UnsafeId to Id and back.
msgSendTest unsafeStr =
    (mutableStringWithString =<< retainedId unsafeStr) >>= autorelease
