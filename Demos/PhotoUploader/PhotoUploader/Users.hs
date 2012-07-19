module Users where

import Data.Aeson
import Foreign.Ptr
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

declMessage "urlWithString" [t| Id -> Class -> IO Id |] "URLWithString:"
declMessage "requestWithURL" [t| Id -> Class -> IO Id |] "requestWithURL:"
declMessage "sendSynchronousRequest" [t| Id -> Ptr UnsafeId -> Ptr UnsafeId -> Class -> IO Id |] "sendSynchronousRequest:returningResponse:error:"

getCurrentUserInfo_hs :: IO Id
getCurrentUserInfo_hs = unretainedId nullPtr

exportFunc "getCurrentUserInfo" [t| IO UnsafeId |] 'getCurrentUserInfo_hs
