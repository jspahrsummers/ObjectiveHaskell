{-# LANGUAGE TemplateHaskell #-}

module CollectionsTest where

import Data.Map as Map
import Data.Sequence as Seq
import ObjectiveHaskell.NSArray
import ObjectiveHaskell.NSDictionary
import ObjectiveHaskell.NSString
import ObjectiveHaskell.ObjC

foreign export ccall
    addFoobarToArray :: UnsafeId -> IO UnsafeId

foreign export ccall
    setFooToBar :: UnsafeId -> IO UnsafeId

foreign export ccall
    appendFoobar :: UnsafeId -> IO UnsafeId

appendFoobar unsafeObj =
    let appendFoobar' :: Id -> IO Id
        appendFoobar' nsstr = do
            str <- fromNSString nsstr
            toNSString $ str ++ "foobar"

    in (appendFoobar' =<< unretainedId unsafeObj) >>= autorelease

addFoobarToArray unsafeObj =
    let addFoobarToArray' :: Id -> IO Id
        addFoobarToArray' nsarr = do
            s <- fromNSArray nsarr
            foobar <- toNSString "foobar"

            toNSArray $ s |> foobar

    in (addFoobarToArray' =<< unretainedId unsafeObj) >>= autorelease

setFooToBar unsafeObj =
    let setFooToBar' :: Id -> IO Id
        setFooToBar' nsdict = do
            tbl <- fromNSDictionary nsdict
            foo <- toNSString "foo"
            bar <- toNSString "bar"

            toNSDictionary $ insert foo bar tbl

    in (setFooToBar' =<< unretainedId unsafeObj) >>= autorelease
