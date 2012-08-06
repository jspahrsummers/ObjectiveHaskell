{-# LANGUAGE Trustworthy #-}

-- | Tools for building a model layer in Haskell that can interoperate with Objective-C
module ObjectiveHaskell.Model (
        MaybePtr, exportAccessors
    ) where

import Control.Monad
import Foreign.StablePtr
import Language.Haskell.TH
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC
import ObjectiveHaskell.THUtils

-- | A pointer to a 'Maybe' value that can be passed to and from Objective-C safely.
-- | This can be used to represent model data that may or may not exist.
type MaybePtr a = StablePtr (Maybe a)

-- | Defines two functions, callable from Objective-C, which get and set a field on a value within a 'MaybePtr'.
-- |
-- | For a type @Car@ and a field @model@, the following functions will be generated and exported to Objective-C:
-- |
-- |  * A getter @Car_model()@ which accepts a 'MaybePtr' @Car@, reads the @model@ field and converts the result into an
-- |    Objective-C object. If the 'MaybePtr' contains 'Nothing', @nil@ is returned.
-- |  * A setter @Car_setModel()@ which accepts a 'MaybePtr' @Car@ and a new value for the @model@ field, and returns a
-- |    new 'MaybePtr' @Car@, where the field in the @Car@ value has been set to the given value.
exportAccessors
    :: Name     -- ^ The name of the type of value stored in the 'MaybePtr'.
    -> Name     -- ^ The name of the record field to access or set in the value, which should hold a 'Bridged' value.
    -> Q [Dec]  -- ^ Top-level declarations to generate and export the accessors.

exportAccessors typeName field = do
    let getter = (nameBase typeName) ++ "_" ++ (nameBase field)
        t = conT typeName

    -- TODO: export setter
    exportGetter getter t field

exportGetter
    :: String   -- ^ The name of the function to export.
    -> Q Type   -- ^ The type of value stored in the 'MaybePtr'.
    -> Name     -- ^ The name of the record field to access.
    -> Q [Dec]

exportGetter objcName t field = do
    hsName <- newName $ "_hs_" ++ objcName
    ft <- [t| MaybePtr $t -> IO UnsafeId |]

    let getter = varE field
        arg = mkName "ptr"

        decl = ForeignD $ ExportF CCall objcName hsName ft

    def <- singleClauseFunc hsName [arg] [| do
                ptr <- deRefStablePtr $(varE arg)
                obj <- maybe nil (toObjC . $getter) ptr
                autorelease obj |]

    return [decl, def]
