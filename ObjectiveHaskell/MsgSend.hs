{-# LANGUAGE TemplateHaskell #-}

module ObjectiveHaskell.MsgSend
    (declMethod)
    where

import Control.Monad
import Data.List
import Foreign.Ptr
import Language.Haskell.TH
import ObjectiveHaskell.ObjC

-- Given a name, a return type, and a list of argument types (without self and _cmd),
-- declares a variant of objc_msgSend with the correct type.
declMethod :: String -> Name -> [Name] -> DecsQ
declMethod name ret methodArgs = do
    let args = ''Id : ''Sel : methodArgs
        ioRet = [t| IO $(conT ret) |]
        pieces = (map conT args) ++ [ioRet]
 
        funType = foldr (\l r -> [t| $l -> $r |]) (last pieces) (init pieces)
        funptrType = [t| FunPtr $funType |]

        funName = (mkName name)

    factoryName <- newName (name ++ "_factory")
    dynType <- [t| $funptrType -> $funType |]

    funptrCastExp <- [| $(varE factoryName) (castFunPtr p_objc_msgSend :: $funptrType) |]

    argNames <- mapM (\_ -> newName "a") args
    argVars <- mapM varE argNames

    let bodyExp = foldM (\l r -> appE (return l) (return r)) (ParensE funptrCastExp) argVars
        patterns = map varP argNames
        c = clause patterns (normalB bodyExp) []
        funDecl = funD funName [c]

        factoryDecl = return (ForeignD $ ImportF CCall Safe "dynamic" factoryName dynType)

    sequence [factoryDecl, funDecl]
