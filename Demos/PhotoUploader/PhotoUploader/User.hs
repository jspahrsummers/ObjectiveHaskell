{-# LANGUAGE OverloadedStrings #-}

module User where

import Control.Applicative
import Control.Monad
import Data.Aeson as Aeson
import Data.ByteString as ByteString
import Data.Text.Lazy as Text
import Foreign.StablePtr
import ObjectiveHaskell.Model
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.NSData
import ObjectiveHaskell.NSString
import ObjectiveHaskell.ObjC

-- | An Instagram user.
data User = User {
    userId :: Integer,
    username :: Text,
    fullName :: Text,
    bio :: Text,
    photoURL :: Text
} deriving (Eq, Ord, Show)

instance Aeson.FromJSON User where
    parseJSON (Object d) = do
        u <- d .: "data"

        userId <- read <$> u .: "id"
        username <- u .: "username"
        fullName <- u .: "full_name"
        bio <- u .: "bio"
        photoURL <- u .: "profile_picture"

        return $ User { userId = userId, username = username, fullName = fullName, bio = bio, photoURL = photoURL }

    parseJSON j = do
        mzero

-- | Decodes an @NSData@ into a 'User'.
decodeUser :: Id -> IO (MaybePtr User)
decodeUser d = do
    json <- fromNSData d

    let mu = Aeson.decode json :: Maybe User
    case mu of
        (Just u) -> newStablePtr $ Just u
        _ -> newStablePtr Nothing

exportFunc "User_initWithData" [t| UnsafeId -> IO (MaybePtr User) |] 'decodeUser

exportAccessors ''User 'fullName
exportAccessors ''User 'username
exportAccessors ''User 'photoURL
