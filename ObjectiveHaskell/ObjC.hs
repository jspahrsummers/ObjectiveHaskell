module ObjectiveHaskell.ObjC (
        Sel, Class, Id, UnsafeId,
        selector, getClass,
        retainedId, unretainedId, autorelease, withUnsafeId,
        p_objc_msgSend, objc_hash
    ) where

import Control.Applicative
import Control.Monad
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr.Safe
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Unsafe
import Foreign.Ptr

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
