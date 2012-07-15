module ObjectiveHaskell.THUtils (
        decomposeFunctionType,
        argumentNames, singleClauseFunc
    ) where

import Language.Haskell.TH

-- Given a type which represents a function signature,
-- returns a list of the applied types, in order.
decomposeFunctionType :: Type -> [Type]
decomposeFunctionType (AppT (AppT ArrowT l) r) = l : decomposeFunctionType r
decomposeFunctionType (ForallT _ _ t) = decomposeFunctionType t
decomposeFunctionType t = [t]

-- Generates N unique argument names.
argumentNames :: Int -> Q [Name]
argumentNames n = mapM (\s -> newName s) $ take n $ repeat "a"

-- Given a function name, parameter names, and a body expression,
-- returns a function declaration with a single clause.
singleClauseFunc :: Name -> [Name] -> Q Exp -> Q Dec
singleClauseFunc name params bodyExp =
    let c = clause (map varP params) (normalB bodyExp) []
    in funD name [c]
