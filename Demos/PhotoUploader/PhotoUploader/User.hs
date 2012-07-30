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

-- | Dereferences a 'MaybePtr', applies the given accessor to its value,
-- | and then converts the result into an Objective-C object.
maybeAccessor
    :: Bridged o
    => (a -> o)     -- ^ An accessor function to lift into the 'Maybe'.
    -> MaybePtr a   -- ^ A pointer to dereference.
    -> IO Id        -- ^ The object result. If the pointer contained 'Nothing', this will be 'nil'.

maybeAccessor f ptr =
    deRefStablePtr ptr >>= maybe nil (toObjC . f)

-- | Returns the full name of a 'User', or 'nil'.
maybeFullName :: MaybePtr User -> IO Id
maybeFullName = maybeAccessor fullName

-- | Returns the username of a 'User', or 'nil'.
maybeUsername :: MaybePtr User -> IO Id
maybeUsername = maybeAccessor username

-- | Returns the profile picture URL of a 'User' (as an @NSString@), or 'nil'.
maybePhotoURL :: MaybePtr User -> IO Id
maybePhotoURL = maybeAccessor photoURL

-- | Decodes an @NSData@ into a 'User'.
decodeUser :: Id -> IO (MaybePtr User)
decodeUser d = do
    json <- fromNSData d

    let mu = Aeson.decode json :: Maybe User
    case mu of
        (Just u) -> newStablePtr $ Just u
        _ -> newStablePtr Nothing

exportFunc "user_decode" [t| UnsafeId -> IO (MaybePtr User) |] 'decodeUser
exportFunc "user_fullName" [t| MaybePtr User -> IO UnsafeId |] 'maybeFullName
exportFunc "user_username" [t| MaybePtr User -> IO UnsafeId |] 'maybeUsername
exportFunc "user_photoURL" [t| MaybePtr User -> IO UnsafeId |] 'maybePhotoURL
