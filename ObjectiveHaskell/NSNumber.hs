{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy #-}

-- | Bridging to and from @NSNumber@
module ObjectiveHaskell.NSNumber (
        fromNSNumber, toNSNumber
    ) where

import Control.Applicative
import Data.Char
import Data.Ratio
import Data.Text.Lazy as Text
import Foreign.C.String
import Foreign.C.Types
import ObjectiveHaskell.MsgSend
import ObjectiveHaskell.NSString
import ObjectiveHaskell.ObjC

-- NSNumber methods
declMessage "numberWithLongLong" [t| CLLong -> Class -> IO Id |] "numberWithLongLong:"
declMessage "numberWithUnsignedLongLong" [t| CULLong -> Class -> IO Id |] "numberWithUnsignedLongLong:"
declMessage "numberWithDouble" [t| CDouble -> Class -> IO Id |] "numberWithDouble:"

declMessage "doubleValue" [t| Id -> IO CDouble |] "doubleValue"
declMessage "longLongValue" [t| Id -> IO CLLong |] "longLongValue"
declMessage "unsignedLongLongValue" [t| Id -> IO CULLong |] "unsignedLongLongValue"
declMessage "objCType" [t| Id -> IO CString |] "objCType"

-- | Converts an @NSNumber@ into a 'Rational'.
fromNSNumber :: Id -> IO Rational
fromNSNumber obj = do
    t <- obj @. objCType >>= peekCString
    
    if (t == "f") || (t == "d")
    then toRational <$> obj @. doubleValue

    -- Signed type encodings are lowercase.
    else if isLower $ Prelude.head t
            then toRational <$> obj @. longLongValue
            else toRational <$> obj @. unsignedLongLongValue

-- | Converts a 'Rational' into an @NSNumber@.
toNSNumber :: Rational -> IO Id
toNSNumber rat
    | denominator rat /= 1 = getClass "NSNumber" >>= numberWithDouble (fromRational rat)
    | numerator rat < 0 = getClass "NSNumber" >>= numberWithLongLong (fromIntegral $ numerator rat)
    | otherwise = getClass "NSNumber" >>= numberWithUnsignedLongLong (fromIntegral $ numerator rat)

instance Bridged Rational where
    toObjC = toNSNumber
    fromObjC = fromNSNumber
