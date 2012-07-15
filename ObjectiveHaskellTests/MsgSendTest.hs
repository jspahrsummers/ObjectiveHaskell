module MsgSendTest where

import Control.Monad
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

declMessage "stringWithString" [t| Id -> Id -> IO Id |] "stringWithString:"

-- This is what a Haskell function that invokes Objective-C code might look like.
mutableStringWithString :: Id -> IO Id
mutableStringWithString str = getClass "NSMutableString" >>= stringWithString str

exportFunc "msgSendTest" [t| UnsafeId -> IO UnsafeId |] 'mutableStringWithString
