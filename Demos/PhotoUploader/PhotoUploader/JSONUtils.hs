module JSONUtils (
        decodeModel
    ) where

import Data.Aeson as Aeson
import Foreign.StablePtr
import ObjectiveHaskell.Model
import ObjectiveHaskell.NSData
import ObjectiveHaskell.ObjC

-- | Decodes an @NSData@ containing JSON into a 'MaybePtr'.
decodeModel :: Aeson.FromJSON a => Id -> IO (MaybePtr a)
decodeModel d = do
    json <- fromNSData d

    let mv = Aeson.decode json
    case mv of
        (Just v) -> newStablePtr $ Just v
        _ -> newStablePtr Nothing
