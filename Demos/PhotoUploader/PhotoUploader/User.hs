{-# LANGUAGE OverloadedStrings #-}

module User where

import Control.Applicative
import Control.Monad
import Data.Aeson as Aeson
import Data.Text.Lazy as Text
import JSONUtils
import ObjectiveHaskell.Model
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

exportFunc "User_initWithData" [t| UnsafeId -> IO (MaybePtr User) |] 'decodeModel

exportAccessors ''User 'fullName
exportAccessors ''User 'username
exportAccessors ''User 'photoURL
