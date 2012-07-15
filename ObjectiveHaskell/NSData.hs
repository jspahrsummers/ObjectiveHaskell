module ObjectiveHaskell.NSData (
        fromNSData, toNSData
    ) where

import Control.Applicative
import Data.ByteString as ByteString
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.ObjC

-- NSData methods
declMessage "dataWithBytes" [t| Ptr () -> NSUInteger -> Id -> IO Id |] "dataWithBytes:length:"
declMessage "objc_length" [t| Id -> IO NSUInteger |] "length"
declMessage "bytes" [t| Id -> IO (Ptr ()) |] "bytes"

-- Converts an NSData into a ByteString.
fromNSData :: Id -> IO ByteString
fromNSData dat = do
    sz <- objc_length dat
    ptr <- bytes dat

    pack <$> peekArray (fromIntegral sz) (castPtr ptr)

-- Converts a ByteString into an immutable NSData.
toNSData :: ByteString -> IO Id
toNSData str =
    withArray (unpack str) $ \ptr ->
        getClass "NSData" >>= dataWithBytes (castPtr ptr) (fromIntegral $ ByteString.length str)

instance Bridged ByteString where
    toObjC = toNSData
    fromObjC = fromNSData
