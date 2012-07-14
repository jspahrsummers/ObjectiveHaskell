{-# LANGUAGE ForeignFunctionInterface #-}

module ObjectiveHaskell.ObjC (
        Sel, Class, Id, UnsafeId,
        retainedId, unretainedId, autorelease,
        p_objc_msgSend,
        selector, getClass
    ) where

import Control.Applicative
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr.Safe
import Foreign.Ptr

type Sel = Ptr ()
type UnsafeId = Ptr ()
type Imp = FunPtr (UnsafeId -> Sel -> IO UnsafeId)

type Id = ForeignPtr ()
type Class = Id

-- Retains an UnsafeId, and converts it into a Id, which will be released when the last reference to it disappears.
retainedId :: UnsafeId -> IO Id
retainedId obj = newForeignPtr p_release obj <* retain obj

-- Converts an UnsafeId into an Id, without any memory management.
unretainedId :: UnsafeId -> IO Id
unretainedId obj = newForeignPtr_ obj

-- Retains then autoreleases an Id, returning an UnsafeId.
-- The resulting UnsafeId can be used safely until the autorelease pool is drained.
autorelease :: Id -> IO UnsafeId
autorelease obj =
    withForeignPtr obj $ \obj -> do
        u <- retain obj
        sel <- selector "autorelease"
        autorelease_dyn (castFunPtr p_objc_msgSend) u sel

-- Maps a string to a selector.
selector :: String -> IO Sel
selector s = withCString s sel_registerName

-- Returns the class by the given name.
getClass :: String -> IO Class
getClass name = withCString name (unretainedId . objc_getClass)

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
