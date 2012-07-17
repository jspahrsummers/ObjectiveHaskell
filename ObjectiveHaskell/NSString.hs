{-# LANGUAGE FlexibleInstances #-}

-- | Bridging to and from @NSString@
module ObjectiveHaskell.NSString (
        fromNSString, toNSString
    ) where

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import ObjectiveHaskell.ObjC

-- | A Core Foundation @Boolean@.
type Boolean = CUChar

-- | A Core Foundation @CFIndex@.
type CFIndex = CLong

-- | A Core Foundation @CFStringEncoding@.
type CFStringEncoding = CInt

foreign import ccall unsafe "CoreFoundation/CoreFoundation.h CFStringCreateWithBytes"
    newCFString :: Ptr () -> Ptr Word8 -> CFIndex -> CFStringEncoding -> Boolean -> IO UnsafeId

foreign import ccall unsafe "CoreFoundation/CoreFoundation.h CFStringGetCStringPtr"
    getCStringPtr :: UnsafeId -> CFStringEncoding -> IO CString

foreign import ccall unsafe "CoreFoundation/CoreFoundation.h CFStringGetCString"
    getCString :: UnsafeId -> CString -> CFIndex -> CFStringEncoding -> IO Boolean

foreign import ccall unsafe "CoreFoundation/CoreFoundation.h CFStringGetLength"
    getLength :: UnsafeId -> IO CFIndex

kCFStringEncodingUTF8 :: CFStringEncoding
kCFStringEncodingUTF8 = 0x08000100

-- | Converts an @NSString@ into a 'String'.
fromNSString :: Id -> IO String
fromNSString obj =
    withUnsafeId obj $ \obj -> do
        len <- getLength obj
        ptr <- getCStringPtr obj kCFStringEncodingUTF8

        if ptr /= nullPtr
            -- TODO: This may read in a string with an incorrect encoding (see toNSString).
            then peekCStringLen (ptr, fromIntegral len)
            else copyNSString obj $ fromIntegral len

-- | Converts an unwrapped @NSString@ into a 'String' using a temporary buffer.
copyNSString
    :: UnsafeId     -- ^ The @NSString@ to convert.
    -> Int          -- ^ The length of the string.
    -> IO String

copyNSString obj len = do
    buf <- mallocBytes (len + 1)

    -- In practice, this should never fail.
    getCString obj buf (fromIntegral len + 1) kCFStringEncodingUTF8
    
    -- TODO: This may read in a string with an incorrect encoding (see toNSString).
    str <- peekCStringLen (buf, len)
    free buf

    return str

-- | Converts a String into an immutable @NSString@.
toNSString :: String -> IO Id
toNSString str =
    let len = fromIntegral (length str) :: CFIndex
    in withCString str $ \ptr -> do
        -- TODO: This encoding is probably not correct,
        -- as withCString uses "the current locale", which may not be UTF-8.
        --
        -- We can get the system encoding using CFStringGetSystemEncoding(), but even that may change on a per-application basis.
        obj <- newCFString nullPtr (castPtr ptr) len kCFStringEncodingUTF8 0
        retainedId obj

instance Bridged [Char] where
    toObjC = toNSString
    fromObjC = fromNSString
