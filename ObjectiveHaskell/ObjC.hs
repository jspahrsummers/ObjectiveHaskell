module ObjectiveHaskell.ObjC (
        Sel, Class, Id, UnsafeId,
        ObjCBool, NSUInteger,
        Bridged, toObjC, fromObjC,
        selector, getClass,
        retainedId, unretainedId, autorelease, withUnsafeId,
        p_objc_msgSend, objc_hash,
        exportFunc
    ) where

import Control.Applicative
import Control.Monad
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr.Safe
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Unsafe
import Foreign.Ptr
import Language.Haskell.TH
import ObjectiveHaskell.THUtils

type ObjCBool = CSChar
type NSUInteger = CSize

type Sel = Ptr ()
type UnsafeId = Ptr ()
type Imp = FunPtr (UnsafeId -> Sel -> IO UnsafeId)

newtype Id = Id (ForeignPtr ())
    deriving Show

instance Eq Id where
    -- This depends on the return value of -isEqual: remaining the same across calls.
    (Id a) == (Id b) =
        unsafeLocalState $ do
            sel <- selector "isEqual:"

            let unsafeA = unsafeForeignPtrToPtr a
                unsafeB = unsafeForeignPtrToPtr b
            
            eq <- isEqual_dyn (castFunPtr p_objc_msgSend) unsafeA sel unsafeB
            return $ if eq == 0 then False else True

instance Ord Id where
    -- This is a silly way to do this, but Objective-C doesn't really have generalized comparison.
    compare a b = compare (objc_hash a) (objc_hash b)

type Class = Id

-- Represents any value that can be bridged with Objective-C.
class Bridged a where
    fromObjC :: Id -> IO a
    toObjC :: a -> IO Id

instance Bridged Id where
    fromObjC = return
    toObjC = return

-- Retains an UnsafeId, and converts it into a Id, which will be released when the last reference to it disappears.
retainedId :: UnsafeId -> IO Id
retainedId obj = Id <$> newForeignPtr p_release obj <* retain obj

-- Converts an UnsafeId into an Id, without any memory management.
unretainedId :: UnsafeId -> IO Id
unretainedId obj = Id <$> newForeignPtr_ obj

-- Retains then autoreleases an Id, returning an UnsafeId.
-- The resulting UnsafeId can be used safely until the autorelease pool is drained.
autorelease :: Id -> IO UnsafeId
autorelease obj =
    withUnsafeId obj $ \obj -> do
        u <- retain obj
        sel <- selector "autorelease"
        autorelease_dyn (castFunPtr p_objc_msgSend) u sel

-- Applies the given function to the actual pointer of the given object.
withUnsafeId :: Id -> (UnsafeId -> IO a) -> IO a
withUnsafeId (Id fptr) = withForeignPtr fptr

-- Maps a string to a selector.
selector :: String -> IO Sel
selector s = withCString s sel_registerName

-- Returns the class by the given name.
getClass :: String -> IO Class
getClass name = withCString name (unretainedId . objc_getClass)

-- Returns the -hash of an object.
-- This depends on an object's -hash remaining the same across calls.
objc_hash :: Id -> NSUInteger
objc_hash (Id obj) =
    let unsafeObj = unsafeForeignPtrToPtr obj
        ioHash = hash_dyn (castFunPtr p_objc_msgSend) unsafeObj =<< selector "hash"
    in unsafeLocalState ioHash 

-- Within a do expression, applies the given variable to retainedId if necessary,
-- binding it to the same name. The resulting statement is added to the given list.
bindUnsafeIds :: (Type, Name) -> [Stmt] -> [Stmt]
bindUnsafeIds (t, v) stmts
    | t == ConT ''UnsafeId =
        let retainExp = AppE (VarE 'retainedId) (VarE v)
        in BindS (VarP v) retainExp : stmts

    | otherwise = NoBindS (VarE v) : stmts

-- Given a string name to export to Objective-C, an exported type signature, and the name of the function which should be invoked,
-- defines a trampoline which will automatically wrap UnstableId values for Haskell, and unwrap any Id return value for Objective-C.
exportFunc :: String -> Q Type -> Name -> Q [Dec]
exportFunc tramp qt funcName = do
    t <- qt

    let types = decomposeFunctionType t
        paramTypes = init types
        retType = last types

    argNames <- argumentNames $ length paramTypes
    resultName <- newName "result"

    let applyExpr = foldl AppE (VarE funcName) $ map VarE argNames

        -- If we're returning an IO UnsafeId, we should autorelease the result of the function
        bodyStmts = if retType == (AppT (ConT ''IO) (ConT ''UnsafeId))
                    then let binding = BindS (VarP resultName) applyExpr
                             autoreleaseExp = AppE (VarE 'autorelease) (VarE resultName)
                         in [binding, NoBindS autoreleaseExp]
                    else [NoBindS applyExpr]

        -- Map all UnsafeId arguments to Ids and bind them to the same names
        bodyExpr = DoE $ foldr bindUnsafeIds bodyStmts $ zip paramTypes argNames
        funcDef = singleClauseFunc (mkName tramp) argNames $ return bodyExpr

        foreignDecl = ForeignD $ ExportF CCall tramp (mkName tramp) t

    sequence $ (return foreignDecl) : [funcDef]

-- Objective-C runtime functions
foreign import ccall safe "objc/runtime.h &objc_msgSend"
    p_objc_msgSend :: Imp

foreign import ccall unsafe "objc/runtime.h sel_registerName"
    sel_registerName :: CString -> IO Sel

foreign import ccall unsafe "objc/runtime.h objc_getClass"
    objc_getClass :: CString -> UnsafeId

foreign import ccall unsafe "CoreFoundation/CoreFoundation.h CFRetain"
    retain :: UnsafeId -> IO UnsafeId

foreign import ccall safe "CoreFoundation/CoreFoundation.h &CFRelease"
    p_release :: FunPtr (UnsafeId -> IO ())

foreign import ccall unsafe "dynamic"
    autorelease_dyn :: FunPtr (UnsafeId -> Sel -> IO UnsafeId) -> (UnsafeId -> Sel -> IO UnsafeId)

foreign import ccall unsafe "dynamic"
    isEqual_dyn :: FunPtr (UnsafeId -> Sel -> UnsafeId -> IO ObjCBool) -> (UnsafeId -> Sel -> UnsafeId -> IO ObjCBool)

foreign import ccall unsafe "dynamic"
    hash_dyn :: FunPtr (UnsafeId -> Sel -> IO NSUInteger) -> (UnsafeId -> Sel -> IO NSUInteger)
