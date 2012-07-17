module ObjectiveHaskell.NSValue (
    ) where

import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

-- NSValue methods
declMessage "valueWithPointer" [t| Ptr () -> Id -> IO Id |] "valueWithPointer:"
