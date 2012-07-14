{-# LANGUAGE TemplateHaskell #-}

module ObjectiveHaskell.MsgSend
    (declMethod)
    where

import Control.Monad
import Data.List
import Foreign.Ptr
import Language.Haskell.TH
import ObjectiveHaskell.ObjC

-- Given a method's non-monadic return type, and a list of its parameter types (excluding those of 'self' and '_cmd'),
-- returns a function type which includes 'self' and '_cmd' and returns a value in the IO monad.
funcTypeFromMethodTypes :: Type -> [Type] -> Q Type
funcTypeFromMethodTypes ret params =
    let paramTypes = (ConT ''Id) : (ConT ''Sel) : params
        ioRet = AppT (ConT ''IO) ret
    in return $ foldr (\l r -> (AppT l r)) ioRet paramTypes

-- Generates N unique argument names.
argumentNames :: Int -> Q [Name]
argumentNames n = mapM (\s -> newName s) $ take n $ repeat "a"

-- Given a function name, a list of parameter names, and a function mapping the argument values to a body expression,
-- returns a function declaration with a single clause.
singleClauseFunc :: Name -> [Name] -> ([Exp] -> Q Exp) -> Q Dec
singleClauseFunc name params f =
    let bodyExp = f $ map VarE params
        mc = clause (map varP params) (normalB bodyExp) []
    in funD name [mc]

-- Turns a non-empty list of expressions into zero or more left-associative function applications
applyl :: [Exp] -> Q Exp
applyl exprs =
    let f :: Exp -> Exp -> Q Exp
        f l r = return $ AppE l r
    in foldM f (head exprs) (tail exprs)

-- Given a name, a return type, and a list of parameter types (without self and _cmd),
-- declares a variant of objc_msgSend with the correct type.
--
-- TODO: accept a selector with which the method should be associated (so it doesn't have to be provided manually each time)
declMethod :: String -> Name -> [Name] -> Q [Dec]
declMethod name ret params = do
    argNames <- argumentNames $ length params

    -- A unique generated name for our dynamic foreign import
    -- (which is just a trampoline from a FunPtr to a callable function)
    dynName <- newName (name ++ "_dyn")

    -- Casts p_objc_msgSend to the FunPtr type accepted by the trampoline
    funcptrCastExp <- [| $(varE dynName) (castFunPtr p_objc_msgSend) |]

    let funcType = funcTypeFromMethodTypes (ConT ret) (map ConT params)

        -- Apply the given arguments to the function returned by the trampoline
        funcBody = (\args -> applyl $ (ParensE funcptrCastExp) : args)
        funcDecl = singleClauseFunc (mkName name) argNames funcBody

        dynDecl = forImpD CCall Safe "dynamic" dynName [t| FunPtr $funcType -> $funcType |]

    sequence [dynDecl, funcDecl]
