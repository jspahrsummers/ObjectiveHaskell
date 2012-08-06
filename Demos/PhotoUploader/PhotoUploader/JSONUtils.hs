{-# LANGUAGE OverloadedStrings #-}

module JSONUtils (
        decodeModel
    ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Foreign.StablePtr
import ObjectiveHaskell.Model
import ObjectiveHaskell.NSData
import ObjectiveHaskell.ObjC

-- | Parses the @data@ field of an Instagram object.
parseData :: Value -> Parser Value
parseData (Object obj) = obj .: "data"
parseData _ = mzero

-- | Decodes an @NSData@ containing JSON into a 'MaybePtr'.
decodeModel :: FromJSON a => Id -> IO (MaybePtr a)
decodeModel d = do
    json <- fromNSData d

    let mv = decode json >>= parseMaybe (parseData >=> parseJSON)
    case mv of
        (Just v) -> newStablePtr $ Just v
        _ -> newStablePtr Nothing
