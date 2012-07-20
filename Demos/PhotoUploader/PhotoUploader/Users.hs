module Users where

import Data.Aeson
import Foreign.Ptr
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.NSData
import ObjectiveHaskell.NSString
import ObjectiveHaskell.ObjC

declMessage "urlWithString" [t| Id -> Class -> IO Id |] "URLWithString:"
declMessage "requestWithURL" [t| Id -> Class -> IO Id |] "requestWithURL:"
declMessage "sendSynchronousRequest" [t| Id -> Ptr UnsafeId -> Ptr UnsafeId -> Class -> IO Id |] "sendSynchronousRequest:returningResponse:error:"

getCurrentUserInfo_hs :: Id -> IO Id
getCurrentUserInfo_hs tokenObj = do
    token <- fromNSString tokenObj
    urlStr <- toNSString $ "https://api.instagram.com/v1/users/self?access_token=" ++ token

    url <- getClass "NSURL" >>= urlWithString urlStr
    req <- getClass "NSURLRequest" >>= requestWithURL url

    resp <- getClass "NSURLConnection" >>= sendSynchronousRequest req nullPtr nullPtr >>= fromNSData
    toNSString $ show resp

exportFunc "getCurrentUserInfo" [t| UnsafeId -> IO UnsafeId |] 'getCurrentUserInfo_hs
