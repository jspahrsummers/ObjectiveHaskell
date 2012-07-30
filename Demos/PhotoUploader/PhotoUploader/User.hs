{-# LANGUAGE OverloadedStrings #-}

module User where

import Control.Applicative
import Control.Monad
import Data.Aeson as Aeson
import Data.ByteString as ByteString
import Data.Text.Lazy as Text
import Foreign.StablePtr
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.NSData
import ObjectiveHaskell.NSString
import ObjectiveHaskell.ObjC

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

maybeFullName :: MaybePtr User -> IO Id
maybeFullName ptr =
    deRefStablePtr ptr >>= maybe nil (toNSString . fullName)

decodeUser :: Id -> IO (MaybePtr User)
decodeUser d = do
    json <- fromNSData d

    let mu = Aeson.decode json :: Maybe User
    case mu of
        (Just u) -> newStablePtr $ Just u
        _ -> newStablePtr Nothing

exportFunc "user_decode" [t| UnsafeId -> IO (MaybePtr User) |] 'decodeUser
exportFunc "user_fullName" [t| MaybePtr User -> IO UnsafeId |] 'maybeFullName
