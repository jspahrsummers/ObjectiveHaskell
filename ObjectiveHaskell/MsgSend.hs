{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

-- | Typed message sending, using Template Haskell
module ObjectiveHaskell.MsgSend (
        declMessage, (@.)
    ) where

import Control.Monad
import Data.List
import Foreign.Ptr
import Language.Haskell.TH
import ObjectiveHaskell.ObjC
import ObjectiveHaskell.THUtils

-- | Represents an Objective-C method signature, with a return type and any number of parameter types.
data MethodSig = MethodSig Type [Type]
    deriving (Eq, Show)

{-|

Operator to simplify and clarify messaging syntax. This code:

@
    somethingWithTwoArguments :: Id -> Id -> Id -> IO Id
    somethingWithTwoArguments a b str = …

    f :: Id -> Id -> Id -> IO Id
    f str a b = somethingWithTwoArguments a b str
@

becomes:

@
    f str a b = str @. somethingWithTwoArguments a b
@

-}
(@.) :: Id -> (Id -> b) -> b
(@.) = flip ($)

-- | Returns the Haskell function type that is equivalent to a method signature.
-- | This will change any 'Id' types into 'UnsafeId' types for bridging purposes.
funcTypeFromMethodSig :: MethodSig -> Q Type
funcTypeFromMethodSig (MethodSig ret params) =
    let mapf :: Type -> Q Type
        mapf t | t == ConT ''Id = conT ''UnsafeId
               | t == (AppT (ConT ''IO) (ConT ''Id)) = appT (conT ''IO) (conT ''UnsafeId)
               | otherwise = return t

        foldf :: Q Type -> Q Type -> Q Type
        foldf l r = [t| $l -> $r |]
    in foldr foldf (mapf ret) $ map mapf params

-- | Applies method arguments to an expression in a left-associated fashion,
-- | mapping any arguments of type 'Id' to 'UnsafeId'.
applyMethodArgs
    :: Exp      -- ^ The expression that the method arguments should be applied to.
    -> [Type]   -- ^ The types of the arguments being applied.
    -> [Name]   -- ^ The variable or binding names being applied.
    -> Q Exp

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

applyMethodArgs expr _ _ = return expr

-- | Wraps the return value of an expression to match a desired return type.
-- | This is used to map 'UnsafeId' return values to 'Id'.
wrapReturnedExpr
    :: Q Exp    -- ^ An expression which might needs its return value wrapped.
    -> Type     -- ^ The type of value which should result.
    -> Q Exp

wrapReturnedExpr expr t
    | t == (AppT (ConT ''IO) (ConT ''Id)) = [| $expr >>= retainedId |]
    | otherwise = expr

-- | Generates an expression that returns a @IO 'Sel'@ of the given name.
selectorExpr :: String -> Q Exp
selectorExpr str = [| selector $(litE $ StringL str) |]

-- | Declares a variant of objc_msgSend which can be invoked like a Haskell function,
-- | and which will automatically unwrap 'Id' values as 'UnsafeId' for Objective-C,
-- | and wrap any @IO 'UnsafeId'@ return value as @IO 'Id'@ for Haskell.
declMessage
    :: String   -- ^ The name of the Haskell function to define.
    -> Q Type   -- ^ The type signature for the Haskell function. This should not include any @_cmd@ parameter, and the parameter corresponding to @self@ should appear at the end.
    -> String   -- ^ The selector to use for the message send.
    -> Q [Dec]  -- ^ Top-level declarations to generate the function which will invoke @objc_msgSend@.

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
        
        -- Defines the Haskell function and a type signature for it
        funcName = mkName name
        funcSig = sigD funcName qt
        funcDecl = singleClauseFunc funcName paramNames doBody

    sequence [dynDecl, funcSig, funcDecl]
