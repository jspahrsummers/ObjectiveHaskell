{-# LANGUAGE TemplateHaskell #-}

module ObjectiveHaskell.MsgSend
    (declMethod)
    where

import Control.Monad
import Data.List
import Foreign.ForeignPtr
import Foreign.Ptr
import Language.Haskell.TH
import ObjectiveHaskell.ObjC

data MethodSig = MethodSig Type [Type]
    deriving (Eq, Show)

idType = ConT ''Id
unsafeIdType = ConT ''UnsafeId

-- Returns the function type equivalent to a method signature.
funcTypeFromMethodSig :: MethodSig -> Q Type
funcTypeFromMethodSig (MethodSig ret params) =
    let mapf :: Type -> Q Type
        mapf t | t == idType = return unsafeIdType
               | otherwise = return t

        foldf :: Q Type -> Q Type -> Q Type
        foldf l r = [t| $l -> $r |]

        ioRet = appT (conT ''IO) (mapf ret)
    in foldr foldf ioRet $ map mapf params

-- Generates N unique argument names.
argumentNames :: Int -> Q [Name]
argumentNames n = mapM (\s -> newName s) $ take n $ repeat "a"

-- Given a function name, a list of parameter names, and a body expression,
-- returns a function declaration with a single clause.
singleClauseFunc :: Name -> [Name] -> Q Exp -> Q Dec
singleClauseFunc name params bodyExp =
    let c = clause (map varP params) (normalB bodyExp) []
    in funD name [c]

-- Turns a non-empty list of expressions into zero or more left-associative function applications
applyl :: [Exp] -> Q Exp
applyl exprs =
    let f :: Exp -> Exp -> Q Exp
        f l r = return $ AppE l r
    in foldM f (head exprs) (tail exprs)

applyMethodArgs :: Exp -> [Type] -> [Name] -> Q Exp
applyMethodArgs expr [] [] = return expr
applyMethodArgs expr (t:argTypes) (arg:args)
    | t == idType =
        let compoundExpr = AppE expr (VarE arg)
            lamBody = applyMethodArgs compoundExpr argTypes args
            lamPat = varP arg
            lamExpr = lamE [lamPat] lamBody
        in [| withForeignPtr $(varE arg) $lamExpr |]

    | otherwise =
        let compoundExpr = AppE expr (VarE arg)
        in applyMethodArgs compoundExpr argTypes args

wrapReturnedExpr :: Q Exp -> Type -> Q Exp
wrapReturnedExpr expr t
    | t == idType = [| $expr >>= retainedId |]
    | otherwise = expr

-- Given a name, a return type, and a list of parameter types (without self and _cmd),
-- declares a variant of objc_msgSend with the correct type.
--
-- Any arguments or return values of type Id (and not a synonym thereof) will be automatically memory-managed.
--
-- TODO: accept a selector with which the method should be associated (so it doesn't have to be provided manually each time)
declMethod :: String -> Name -> [Name] -> Q [Dec]
declMethod name ret params = do
    -- Create a method signature from the given types
    let baseRet = ConT ret
        paramTypes = idType : (ConT ''Sel) : (map ConT params)
        methodSig = MethodSig baseRet paramTypes

    uniqArgNames <- argumentNames $ length params

    -- A unique generated name for our dynamic foreign import
    -- (which is just a trampoline from a FunPtr to a callable function)
    dynName <- newName (name ++ "_dyn")

    -- Casts p_objc_msgSend to the FunPtr type accepted by the trampoline
    funcptrCastExp <- [| $(varE dynName) (castFunPtr p_objc_msgSend) |]

    let funcType = funcTypeFromMethodSig methodSig
        dynDecl = forImpD CCall Safe "dynamic" dynName [t| FunPtr $funcType -> $funcType |]

        -- Apply the given arguments to the function returned by the trampoline
        argNames = (mkName "self") : (mkName "_cmd") : uniqArgNames
        funcBody = applyMethodArgs (ParensE funcptrCastExp) paramTypes argNames
        funcDecl = singleClauseFunc (mkName name) argNames $ wrapReturnedExpr funcBody baseRet

    sequence [dynDecl, funcDecl]
