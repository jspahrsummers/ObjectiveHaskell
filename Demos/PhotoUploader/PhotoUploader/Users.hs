{-# LANGUAGE OverloadedStrings #-}

module Users where

import Control.Applicative
import Control.Monad
import Data.Aeson as Aeson
import Data.ByteString as ByteString
import Data.Text.Lazy as Text
import Foreign.Ptr
import Foreign.StablePtr
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.NSData
import ObjectiveHaskell.NSString
import ObjectiveHaskell.ObjC

declMessage "urlWithString" [t| Id -> Class -> IO Id |] "URLWithString:"
declMessage "requestWithURL" [t| Id -> Class -> IO Id |] "requestWithURL:"
declMessage "sendSynchronousRequest" [t| Id -> Ptr UnsafeId -> Ptr UnsafeId -> Class -> IO Id |] "sendSynchronousRequest:returningResponse:error:"

type MaybePtr a = StablePtr (Maybe a)

data User = User {
    userId :: Integer,
    username :: Text,
    fullName :: Text,
    bio :: Text
} deriving (Eq, Ord, Show)

instance Aeson.FromJSON User where
    parseJSON (Object d) = do
        u <- d .: "data"

        userId <- read <$> u .: "id"
        username <- u .: "username"
        fullName <- u .: "full_name"
        bio <- u .: "bio"

        return $ User { userId = userId, username = username, fullName = fullName, bio = bio }

    parseJSON j = do
        mzero

getFullName :: MaybePtr User -> IO Id
getFullName ptr =
    deRefStablePtr ptr >>= maybe nil (toNSString . fullName)

fetchCurrentUser :: Id -> IO (MaybePtr User)
fetchCurrentUser tokenObj = do
    token <- fromNSString tokenObj
    urlStr <- toNSString $ Text.append "https://api.instagram.com/v1/users/self?access_token=" token

    url <- getClass "NSURL" >>= urlWithString urlStr
    req <- getClass "NSURLRequest" >>= requestWithURL url

    resp <- getClass "NSURLConnection" >>= sendSynchronousRequest req nullPtr nullPtr >>= fromNSData

    let mu = Aeson.decode resp :: Maybe User
    case mu of
        (Just u) -> newStablePtr $ Just u
        _ -> newStablePtr Nothing

exportFunc "user_fetchCurrent" [t| UnsafeId -> IO (MaybePtr User) |] 'fetchCurrentUser
exportFunc "user_fullName" [t| MaybePtr User -> IO UnsafeId |] 'getFullName
