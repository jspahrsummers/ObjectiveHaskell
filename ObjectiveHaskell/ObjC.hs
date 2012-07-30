{-# LANGUAGE Trustworthy #-}

-- | Objective-C bridging primitives
module ObjectiveHaskell.ObjC (
        Sel, Class, Id, UnsafeId,
        ObjCBool, NSUInteger,
        Bridged, toObjC, fromObjC,
        selector, getClass,
        retainedId, unretainedId, nil, autorelease, withUnsafeId,
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

-- | An Objective-C @BOOL@.
type ObjCBool = CSChar

-- | Equivalent to Cocoa's @NSUInteger@.
type NSUInteger = CSize

-- | An Objective-C @SEL@.
type Sel = Ptr ()

-- | An object reference that is not being memory-managed by Haskell.
-- | This type should only be used at bridging points -- 'Id' is better for almost every other case.
type UnsafeId = Ptr ()

-- | Represents an Objective-C @IMP@ (although it is not variadic).
type Imp = FunPtr (UnsafeId -> Sel -> IO UnsafeId)

-- | An object reference that is being memory-managed by Haskell.
-- | Objects of type Id will be retained for as long as any Haskell code holds a reference.
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

-- | As in Objective-C, a Class is just an 'Id', but better documents its purpose in an function signature.
type Class = Id

-- | Represents any value that can be bridged to and from Objective-C.
class Bridged a where
    fromObjC :: Id -> IO a
    toObjC :: a -> IO Id

instance Bridged Id where
    fromObjC = return
    toObjC = return

instance Bridged a => Bridged (Maybe a) where
    toObjC mv = maybe nil toObjC mv
    fromObjC obj =
        withUnsafeId obj $ \ptr ->
            if ptr == nullPtr
                then return Nothing
                else Just <$> fromObjC obj

-- | An 'Id' value representing Objective-C @nil@.
nil :: IO Id
nil = unretainedId nullPtr

-- | Retains an 'UnsafeId' and converts it into an 'Id', which will be released when the last reference to it disappears.
retainedId :: UnsafeId -> IO Id
retainedId obj
    | obj == nullPtr = nil
    | otherwise = Id <$> newForeignPtr p_release obj <* retain obj

-- | Converts an 'UnsafeId' into an 'Id', without any memory management.
-- | This should only be used for objects that do not need to be retained (like 'Class' objects).
unretainedId :: UnsafeId -> IO Id
unretainedId obj = Id <$> newForeignPtr_ obj

-- | Retains then autoreleases an 'Id', returning an 'UnsafeId'.
-- | The resulting 'UnsafeId' can be used safely (by Haskell or Objective-C code) until the autorelease pool is drained.
autorelease :: Id -> IO UnsafeId
autorelease obj =
    withUnsafeId obj $ \obj ->
        if obj == nullPtr
        then return $ obj
        else do
            u <- retain obj
            sel <- selector "autorelease"
            autorelease_dyn (castFunPtr p_objc_msgSend) u sel

-- | Applies a function to the 'UnsafeId' corresponding to an 'Id'.
-- | This can be used to temporarily manipulate an 'UnsafeId' without sacrificing safety.
withUnsafeId
    :: Id                   -- ^ The object to manipulate as an 'UnsafeId'.
    -> (UnsafeId -> IO a)   -- ^ An action to apply to the unwrapped 'UnsafeId'.
    -> IO a                 -- ^ The action returned by the function.

withUnsafeId (Id fptr) = withForeignPtr fptr

-- | Registers a selector with the Objective-C runtime.
selector
    :: String   -- ^ A string representing the selector to register.
    -> IO Sel   -- ^ A new or existing unique selector for the given string.

selector s = withCString s sel_registerName

-- | Returns the Objective-C class by a given name.
getClass :: String -> IO Class
getClass name =
    withCString name $ \name ->
        objc_getClass name >>= unretainedId

-- | Returns the @-hash@ of an object.
-- | This function assumes that the object's hash will remain the same across calls.
objc_hash :: Id -> NSUInteger
objc_hash (Id obj) =
    let unsafeObj = unsafeForeignPtrToPtr obj
        ioHash = hash_dyn (castFunPtr p_objc_msgSend) unsafeObj =<< selector "hash"
    in unsafeLocalState ioHash 

-- | Creates statements that wrap 'UnsafeId' arguments as 'Id' values, rebinding them to the same name.
-- | This function is meant to be used with a right fold.
bindUnsafeIds
    :: (Type, Name) -- ^ The type and name of the argument to process.
    -> [Stmt]       -- ^ A list of existing statements in the @do@ expression.
    -> [Stmt]       -- ^ The new list of statements to use in the @do@ expression.

bindUnsafeIds (t, v) stmts
    | t == ConT ''UnsafeId =
        let retainExp = AppE (VarE 'retainedId) (VarE v)
        in BindS (VarP v) retainExp : stmts

    | otherwise = stmts

-- | Defines a trampoline function which will automatically wrap 'UnstableId' values into 'Id' for Haskell,
-- | and unwrap any 'Id' return value as an 'UnstableId' for Objective-C.
exportFunc
    :: String   -- ^ The name of the trampoline function to export to Objective-C.
    -> Q Type   -- ^ The type signature of the trampoline function.
    -> Name     -- ^ The name of the Haskell function which should be invoked by the trampoline.
    -> Q [Dec]  -- ^ Top-level declarations to generate and export the trampoline.

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

-- | A pointer to an unspecialized objc_msgSend.
-- | This function pointer should not be invoked before being cast to the appropriate type.
foreign import ccall safe "objc/runtime.h &objc_msgSend"
    p_objc_msgSend :: Imp

foreign import ccall safe "objc/runtime.h sel_registerName"
    sel_registerName :: CString -> IO Sel

foreign import ccall safe "objc/runtime.h objc_getClass"
    objc_getClass :: CString -> IO UnsafeId

foreign import ccall safe "CoreFoundation/CoreFoundation.h CFRetain"
    retain :: UnsafeId -> IO UnsafeId

foreign import ccall safe "CoreFoundation/CoreFoundation.h &CFRelease"
    p_release :: FunPtr (UnsafeId -> IO ())

-- | Creates a trampoline function from a function pointer that matches the type of @-autorelease@.
foreign import ccall safe "dynamic"
    autorelease_dyn :: FunPtr (UnsafeId -> Sel -> IO UnsafeId) -> (UnsafeId -> Sel -> IO UnsafeId)

-- | Creates a trampoline function from a function pointer that matches the type of @-isEqual:@.
foreign import ccall safe "dynamic"
    isEqual_dyn :: FunPtr (UnsafeId -> Sel -> UnsafeId -> IO ObjCBool) -> (UnsafeId -> Sel -> UnsafeId -> IO ObjCBool)

-- | Creates a trampoline function from a function pointer that matches the type of @-hash@.
foreign import ccall safe "dynamic"
    hash_dyn :: FunPtr (UnsafeId -> Sel -> IO NSUInteger) -> (UnsafeId -> Sel -> IO NSUInteger)
