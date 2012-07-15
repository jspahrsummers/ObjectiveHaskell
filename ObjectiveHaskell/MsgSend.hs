module ObjectiveHaskell.MsgSend (
        declMessage, (@.)
    ) where

import Control.Monad
import Data.List
import Foreign.Ptr
import Language.Haskell.TH
import ObjectiveHaskell.ObjC
import ObjectiveHaskell.THUtils

data MethodSig = MethodSig Type [Type]
    deriving (Eq, Show)

{-

Operator to simplify and clarify messaging syntax:

    somethingWithTwoArguments :: Id -> Id -> Id -> IO Id
    somethingWithTwoArguments a b str = …

    f :: Id -> Id -> Id -> IO Id
    f str a b = somethingWithTwoArguments a b str

becomes:

    f str a b = str @. somethingWithTwoArguments a b

-}
(@.) :: Id -> (Id -> b) -> b
(@.) = flip ($)

-- Returns the function type equivalent to a method signature.
funcTypeFromMethodSig :: MethodSig -> Q Type
funcTypeFromMethodSig (MethodSig ret params) =
    let mapf :: Type -> Q Type
        mapf t | t == ConT ''Id = conT ''UnsafeId
               | t == (AppT (ConT ''IO) (ConT ''Id)) = appT (conT ''IO) (conT ''UnsafeId)
               | otherwise = return t

        foldf :: Q Type -> Q Type -> Q Type
        foldf l r = [t| $l -> $r |]
    in foldr foldf (mapf ret) $ map mapf params

-- Given a list of argument types and names, applies each argument to expr in a left-associative fashion.
-- Any arguments of type Id will automatically be mapped to an UnsafeId.
applyMethodArgs :: Exp -> [Type] -> [Name] -> Q Exp
applyMethodArgs expr [] [] = return expr
applyMethodArgs expr (t:argTypes) (arg:args)
    | t == ConT ''Id =
        let compoundExpr = AppE expr (VarE arg)
            lamBody = applyMethodArgs compoundExpr argTypes args
            lamPat = varP arg
            lamExpr = lamE [lamPat] lamBody
        in [| withUnsafeId $(varE arg) $lamExpr |]

    | otherwise =
        let compoundExpr = AppE expr (VarE arg)
        in applyMethodArgs compoundExpr argTypes args

-- Wraps the given expression as necessary to match the given type.
-- This is used to map UnsafeId return values to Id.
wrapReturnedExpr :: Q Exp -> Type -> Q Exp
wrapReturnedExpr expr t
    | t == (AppT (ConT ''IO) (ConT ''Id)) = [| $expr >>= retainedId |]
    | otherwise = expr

-- Creates an expression that returns a IO Sel of the given name.
selectorExpr :: String -> Q Exp
selectorExpr str = [| selector $(litE $ StringL str) |]

-- Given a string name, a function type (without _cmd), and a selector,
-- declares a variant of objc_msgSend which matches the given signature, automatically fills in _cmd,
-- unwraps Ids for Objective-C, and wraps any UnstableId return value for Haskell.
declMessage :: String -> Q Type -> String -> Q [Dec]
declMessage name qt selName = do
    t <- qt

    -- Create a method signature from the given types
    let types = decomposeFunctionType t
        retType = last types
        paramTypes = (ConT ''Id) : (ConT ''Sel) : (init $ init types)
        methodSig = MethodSig retType paramTypes

    -- Don't include the return type or 'self'
    uniqArgNames <- argumentNames $ (length types) - 2

    -- A unique generated name for our dynamic foreign import
    -- (which is just a trampoline from a FunPtr to a callable function)
    dynName <- newName (name ++ "_dyn")

    -- Casts p_objc_msgSend to the FunPtr type accepted by the trampoline
    funcptrCastExp <- [| $(varE dynName) (castFunPtr p_objc_msgSend) |]

    let funcType = funcTypeFromMethodSig methodSig
        dynDecl = forImpD CCall Safe "dynamic" dynName [t| FunPtr $funcType -> $funcType |]

        -- The arguments that will be processed by applyMethodArgs
        -- _cmd here is effectively a "hidden" argument which will be from a 'do' expression
        cmdName = mkName "_cmd"
        selfName = mkName "self"
        argNames = selfName : cmdName : uniqArgNames

        -- The function we'll expose doesn't need a _cmd parameter
        paramNames = uniqArgNames ++ [selfName]

        -- Apply the given arguments, including _cmd, to the function returned by the trampoline
        funcBody = applyMethodArgs (ParensE funcptrCastExp) paramTypes argNames

        -- "do _cmd <- selector fn_name; body >>= retainedId"
        doBody = doE [bindS (varP cmdName) (selectorExpr selName),
                      noBindS $ wrapReturnedExpr funcBody retType]

        funcDecl = singleClauseFunc (mkName name) paramNames doBody

    sequence [dynDecl, funcDecl]
