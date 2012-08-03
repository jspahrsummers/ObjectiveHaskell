{-# LANGUAGE Trustworthy #-}

-- | Tools for building a model layer in Haskell that can interoperate with Objective-C
module ObjectiveHaskell.Model (
        MaybePtr, exportAccessor
    ) where

import Control.Monad
import Foreign.StablePtr
import Language.Haskell.TH
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC
import ObjectiveHaskell.THUtils

-- | A pointer to a Maybe value that can be passed to and from Objective-C safely.
-- | This can be used to represent model data that may or may not exist.
type MaybePtr a = StablePtr (Maybe a)

-- | Defines a function, callable from Objective-C, which applies a Haskell function to a value within a 'MaybePtr',
-- | then converts the result into an Objective-C object.
-- |
-- | If the 'MaybePtr' contains 'Nothing', the exported function will return @nil@.
exportAccessor
    :: String   -- ^ The name of the function to export to Objective-C.
    -> Name     -- ^ The name of the type of value stored in the 'MaybePtr'.
    -> Name     -- ^ The name of the Haskell accessor function to apply to the value, returning a Bridged value.
    -> Q [Dec]  -- ^ Top-level declarations to generate and export the accessor.

exportAccessor str valueTypeName accessorName = do
    let accessor = varE accessorName
        arg = mkName "ptr"
        valCon = conT valueTypeName

    name <- newName $ nameBase accessorName ++ "_tramp"
    ft <- [t| MaybePtr $valCon -> IO UnsafeId |]

    let externDecl = ForeignD $ ExportF CCall str name ft
    funcDef <- singleClauseFunc name [arg] [| do
                ptr <- deRefStablePtr $(varE arg)
                obj <- maybe nil (toObjC . $accessor) ptr
                autorelease obj |]

    return [externDecl, funcDef]
