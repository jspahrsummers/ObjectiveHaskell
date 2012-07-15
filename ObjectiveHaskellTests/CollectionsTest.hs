module CollectionsTest where

import Data.Map as Map
import Data.Sequence as Seq
import ObjectiveHaskell.NSArray
import ObjectiveHaskell.NSDictionary
import ObjectiveHaskell.NSString
import ObjectiveHaskell.ObjC

appendFoobar' :: Id -> IO Id
appendFoobar' nsstr = do
    str <- fromNSString nsstr
    toNSString $ str ++ "foobar"

addFoobarToArray' :: Id -> IO Id
addFoobarToArray' nsarr = do
    s <- fromNSArray nsarr
    foobar <- toNSString "foobar"

    toNSArray $ s |> foobar

setFooToBar' :: Id -> IO Id
setFooToBar' nsdict = do
    tbl <- fromNSDictionary nsdict
    foo <- toNSString "foo"
    bar <- toNSString "bar"

    toNSDictionary $ insert foo bar tbl

exportFunc "appendFoobar" [t| UnsafeId -> IO UnsafeId |] 'appendFoobar'
exportFunc "addFoobarToArray" [t| UnsafeId -> IO UnsafeId |] 'addFoobarToArray'
exportFunc "setFooToBar" [t| UnsafeId -> IO UnsafeId |] 'setFooToBar'
