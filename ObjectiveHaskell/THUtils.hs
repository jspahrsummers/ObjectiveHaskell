{-# LANGUAGE Unsafe #-}

-- | Convenience functions and utilities for Template Haskell
module ObjectiveHaskell.THUtils (
        decomposeFunctionType,
        argumentNames, singleClauseFunc
    ) where

import Language.Haskell.TH

-- | Returns a list of the applied types in a function signature.
decomposeFunctionType
    :: Type     -- ^ A type representing a function signature.
    -> [Type]   -- ^ A list of the argument and return types for the function, in order.

decomposeFunctionType (AppT (AppT ArrowT l) r) = l : decomposeFunctionType r
decomposeFunctionType (ForallT _ _ t) = decomposeFunctionType t
decomposeFunctionType t = [t]

-- | Generates unique argument names.
argumentNames
    :: Int      -- ^ The number of argument names to generate.
    -> Q [Name]

argumentNames n = mapM (\s -> newName s) $ take n $ repeat "a"

-- | Declares a function with a single clause.
singleClauseFunc
    :: Name     -- ^ The name to use for the function.
    -> [Name]   -- ^ The names of the function's parameters, if any.
    -> Q Exp    -- ^ An expression to use as the body of the function.
    -> Q Dec

singleClauseFunc name params bodyExp =
    let c = clause (map varP params) (normalB bodyExp) []
    in funD name [c]
