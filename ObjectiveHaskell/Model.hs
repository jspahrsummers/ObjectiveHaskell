{-# LANGUAGE Trustworthy #-}

-- | Tools for building a model layer in Haskell that can interoperate with Objective-C
module ObjectiveHaskell.Model (
        MaybePtr, exportAccessors
    ) where

import Control.Monad
import Data.Char
import Foreign.StablePtr
import Language.Haskell.TH
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC
import ObjectiveHaskell.THUtils

-- | A pointer to a 'Maybe' value that can be passed to and from Objective-C safely.
-- | This can be used to represent model data that may or may not exist.
type MaybePtr a = StablePtr (Maybe a)

-- | Defines two functions, callable from Objective-C, which get and update a field on a value within a 'MaybePtr'.
-- |
-- | For a type @Car@ and a field @model@, the following functions will be generated and exported to Objective-C:
-- |
-- |  * A getter @Car_model()@ which accepts a 'MaybePtr' @Car@, reads the @model@ field and converts the result into an
-- |    Objective-C object. If the 'MaybePtr' contains 'Nothing', @nil@ is returned.
-- |  * An update function @Car_update_model()@ which accepts a 'MaybePtr' @Car@ and an Objective-C value for the @model@ field, and
-- |    returns a new 'MaybePtr' @Car@, where the field in the @Car@ value has been updated to the given value (suitably converted
-- |    to a Haskell type).
exportAccessors
    :: Name     -- ^ The name of the type of value stored in the 'MaybePtr'.
    -> Name     -- ^ The name of the record field to access in the value, which should hold a 'Bridged' value.
    -> Q [Dec]  -- ^ Top-level declarations to generate and export the accessors.

exportAccessors typeName field = do
    let typeStr = nameBase typeName
        fieldStr = nameBase field

        getter = typeStr ++ "_" ++ fieldStr
        setter = typeStr ++ "_update_" ++ fieldStr

        t = conT typeName

    liftM2 (++) (exportGetter getter t field) (exportSetter setter t field)

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
                m <- deRefStablePtr $(varE arg)
                obj <- maybe nil (toObjC . $getter) m
                autorelease obj |]

    return [decl, def]

-- Internally, we use the term "setter", but we really are talking about record updates (which are not mutations).
exportSetter
    :: String   -- ^ The name of the function to export.
    -> Q Type   -- ^ The type of value stored in the 'MaybePtr'.
    -> Name     -- ^ The name of the record field to access.
    -> Q [Dec]

exportSetter objcName t field = do
    hsName <- newName $ "_hs_" ++ objcName
    ft <- [t| MaybePtr $t -> UnsafeId -> IO (MaybePtr $t) |]

    let ptrArg = mkName "ptr"
        valArg = mkName "val"

        decl = ForeignD $ ExportF CCall objcName hsName ft

        -- Record update syntax does not splice well at all.
        upd = recUpdE (varE $ mkName "v") [return (field, VarE $ mkName "obj")]

    def <- singleClauseFunc hsName [ptrArg, valArg] [| do
                m <- deRefStablePtr $(varE ptrArg)
                case m of
                    (Just v) -> do
                        obj <- retainedId $(varE valArg) >>= fromObjC
                        newStablePtr $ Just $upd

                    Nothing -> newStablePtr Nothing |]

    return [decl, def]
