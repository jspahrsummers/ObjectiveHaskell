{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

-- | Bridging to and from @NSString@
module ObjectiveHaskell.NSString (
        fromNSString, toNSString
    ) where

import Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy.UTF8
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

declMessage "utf8String" [t| Id -> IO (Ptr CChar) |] "UTF8String"
declMessage "stringWithUtf8String" [t| Ptr CChar -> Class -> IO Id |] "stringWithUTF8String:"

-- | Converts an @NSString@ into a lazy 'String' value.
-- | Note that this /does not/ reuse the internal storage of the @NSString@, and so may not be suitable for large strings.
fromNSString :: Id -> IO String
fromNSString obj = do
    ptr <- obj @. utf8String
    arr <- peekArray0 0 (castPtr ptr) :: IO [Word8]

    return $ toString $ ByteString.pack arr

-- | Converts a 'String' value into an immutable @NSString@.
toNSString :: String -> IO Id
toNSString txt =
    let arr = (ByteString.unpack $ fromString txt) ++ [0]
    in withArray arr $ \ptr ->
        getClass "NSString" >>= stringWithUtf8String (castPtr ptr)

instance Bridged String where
    toObjC = toNSString
    fromObjC = fromNSString

fromNSStringObjC :: Id -> IO (StablePtr String)
fromNSStringObjC obj = fromNSString obj >>= newStablePtr

toNSStringObjC :: StablePtr String -> IO Id
toNSStringObjC ptr = deRefStablePtr ptr >>= toNSString

exportFunc "OHHaskellPtrFromNSString" [t| UnsafeId -> IO (StablePtr String) |] 'fromNSStringObjC
exportFunc "OHNSStringFromHaskellPtr" [t| StablePtr String -> IO UnsafeId |] 'toNSStringObjC
