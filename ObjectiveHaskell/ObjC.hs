{-# LANGUAGE ForeignFunctionInterface #-}

module ObjectiveHaskell.ObjC
    (Sel, Id, p_objc_msgSend)
    where

import Foreign.C.Types
import Foreign.Ptr

type Sel = Ptr ()
-- TODO: this should really be a ForeignPtr with retain/release
type Id = Ptr ()

foreign import ccall "objc/runtime.h &objc_msgSend"
    p_objc_msgSend :: FunPtr (Id -> Sel -> IO Id)
