module ObjectiveHaskell.MsgSend
    (declMessage)
    where

import Control.Monad
import Data.List
import Foreign.Ptr
import Language.Haskell.TH
import ObjectiveHaskell.ObjC

data MethodSig = MethodSig Type [Type]
    deriving (Eq, Show)

-- Returns the function type equivalent to a method signature.
funcTypeFromMethodSig :: MethodSig -> Q Type
funcTypeFromMethodSig (MethodSig ret params) =
    let mapf :: Type -> Q Type
        mapf t | t == ConT ''Id = return $ ConT ''UnsafeId
               | otherwise = return t

        foldf :: Q Type -> Q Type -> Q Type
        foldf l r = [t| $l -> $r |]

        ioRet = appT (conT ''IO) (mapf ret)
    in foldr foldf ioRet $ map mapf params

-- Generates N unique argument names.
argumentNames :: Int -> Q [Name]
argumentNames n = mapM (\s -> newName s) $ take n $ repeat "a"

-- Given a function name, parameter names, and a body expression,
-- returns a function declaration with a single clause.
singleClauseFunc :: Name -> [Name] -> Q Exp -> Q Dec
singleClauseFunc name params bodyExp =
    let c = clause (map varP params) (normalB bodyExp) []
    in funD name [c]

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
    | t == ConT ''Id = [| $expr >>= retainedId |]
    | otherwise = expr

-- Creates an expression that returns a IO Sel of the given name.
selectorExpr :: String -> Q Exp
selectorExpr str = [| selector $(litE $ StringL str) |]

-- Given a string name, selector, a return type, and a list of parameter types (without self and _cmd),
-- declares a variant of objc_msgSend which accepts the parameter types and a final self parameter.
-- _cmd is automatically filled in.
--
-- Any arguments or return values of type Id (and not a synonym thereof) will be automatically memory-managed.
declMessage :: String -> String -> Name -> [Name] -> Q [Dec]
declMessage name selName ret params = do
    -- Create a method signature from the given types
    let baseRet = ConT ret
        paramTypes = (ConT ''Id) : (ConT ''Sel) : (map ConT params)
        methodSig = MethodSig baseRet paramTypes

    uniqArgNames <- argumentNames $ length params

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
                      noBindS $ wrapReturnedExpr funcBody baseRet]

        funcDecl = singleClauseFunc (mkName name) paramNames doBody

    sequence [dynDecl, funcDecl]
