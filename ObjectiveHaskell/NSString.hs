{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy #-}

-- | Bridging to and from @NSString@
module ObjectiveHaskell.NSString (
        fromNSString, toNSString
    ) where

import Data.ByteString.Lazy as ByteString
import Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

declMessage "utf8String" [t| Id -> IO (Ptr CChar) |] "UTF8String"
declMessage "stringWithUtf8String" [t| Ptr CChar -> Class -> IO Id |] "stringWithUTF8String:"

-- | Converts an @NSString@ into a lazy 'Text' value.
-- | Note that this /does not/ reuse the internal storage of the @NSString@, and so may not be suitable for large strings.
fromNSString :: Id -> IO Text
fromNSString obj = do
    ptr <- obj @. utf8String
    arr <- peekArray0 0 (castPtr ptr) :: IO [Word8]

    return $ decodeUtf8 $ ByteString.pack arr

-- | Converts a 'Text' value into an immutable @NSString@.
toNSString :: Text -> IO Id
toNSString txt =
    let arr = ByteString.unpack $ encodeUtf8 txt
    in withArray arr $ \ptr ->
        getClass "NSString" >>= stringWithUtf8String (castPtr ptr)

instance Bridged Text where
    toObjC = toNSString
    fromObjC = fromNSString

instance Bridged [Char] where
    toObjC = toNSString . Text.pack
    fromObjC obj = do
        txt <- fromNSString obj
        return $ Text.unpack txt
