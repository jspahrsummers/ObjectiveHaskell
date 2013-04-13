{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

-- | Bridging to and from @NSData@
module ObjectiveHaskell.NSData (
        fromNSData, toNSData
    ) where

import Control.Applicative
import Data.ByteString.Lazy as ByteString
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import ObjectiveHaskell.TH.MsgSend
import ObjectiveHaskell.TH.ObjC

-- NSData methods
declMessage "dataWithBytes" [t| Ptr () -> NSUInteger -> Class -> IO Id |] "dataWithBytes:length:"
declMessage "objc_length" [t| Id -> IO NSUInteger |] "length"
declMessage "bytes" [t| Id -> IO (Ptr ()) |] "bytes"

-- | Converts an @NSData@ object into a lazy 'ByteString'.
-- | Note that this /does not/ reuse the internal storage of the @NSData@ object, and so may not be suitable for large blobs.
fromNSData :: Id -> IO ByteString
fromNSData dat = do
    sz <- objc_length dat
    ptr <- bytes dat

    pack <$> peekArray (fromIntegral sz) (castPtr ptr)

-- | Converts a lazy 'ByteString' into an immutable @NSData@ object.
-- | Note that this /does not/ reuse the internal storage of the 'ByteString', and so may not be suitable for large blobs.
toNSData :: ByteString -> IO Id
toNSData str =
    withArray (unpack str) $ \ptr ->
        getClass "NSData" >>= dataWithBytes (castPtr ptr) (fromIntegral $ ByteString.length str)

instance Bridged ByteString where
    toObjC = toNSData
    fromObjC = fromNSData

fromNSDataObjC :: Id -> IO (StablePtr ByteString)
fromNSDataObjC obj = fromNSData obj >>= newStablePtr

toNSDataObjC :: StablePtr ByteString -> IO Id
toNSDataObjC ptr = deRefStablePtr ptr >>= toNSData

exportFunc "OHHaskellPtrFromNSData" [t| UnsafeId -> IO (StablePtr ByteString) |] 'fromNSDataObjC
exportFunc "OHNSDataFromHaskellPtr" [t| StablePtr ByteString -> IO UnsafeId |] 'toNSDataObjC
