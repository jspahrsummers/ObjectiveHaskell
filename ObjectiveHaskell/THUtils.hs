module ObjectiveHaskell.THUtils (
        decomposeFunctionType
    ) where

import Language.Haskell.TH

-- Given a type which represents a function signature,
-- returns a list of the applied types, in order.
decomposeFunctionType :: Type -> [Type]
decomposeFunctionType (AppT l (AppT ArrowT r)) = l : decomposeFunctionType r
decomposeFunctionType (AppT _ t) = decomposeFunctionType t
decomposeFunctionType (ForallT _ _ t) = decomposeFunctionType t
decomposeFunctionType t = [t]
