-- | Determines the build flags necessary for a list of Cabal packages, and writes them to stdout as a single line.
-- |
-- | There are three modes of operation:
-- |    1. The @--compiler@ flag determines the build flags necessary to pass to @clang@ (but does not include linker flags).
-- |    2. The @--linker@ flag determines the build flags necessary to pass to @ld@.
-- |    3. If neither of the above is specified, or the @--both@ flag is given, the program will print the build flags for both the compiler and linker (which would be passed to a Clang invocation that includes a linking step).
-- |
-- | All other arguments are assumed to be package names (e.g. @hlint@) or package names with versions (e.g. @hlint/1.8.43@).

module Main where

import Data.Char
import Data.List
import Data.Version
import Debug.Trace
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Simple.Compiler
import Distribution.Simple.GHC
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Verbosity
import System.Console.GetOpt
import System.Environment

data Mode = CompilerMode
          | LinkerMode
          | BothMode
          deriving (Eq, Show)

options :: [OptDescr Mode]
options =
    [ Option ['c'] ["compiler"] (NoArg CompilerMode)    "print out the build flags necessary for compilation (but not linking)"
    , Option ['l'] ["linker"]   (NoArg LinkerMode)      "print out the build flags necessary for linking"
    , Option ['b'] ["both"]     (NoArg BothMode)        "print out the build flags necessary for compilation and linking"
    ]

lookupPackage :: PackageIndex -> String -> IO InstalledPackageInfo
lookupPackage index str =
    let lookupPackage' :: [InstalledPackageInfo]
        lookupPackage' =
            case elemIndex '/' str of
                (Just i) ->
                    let (name, ver) = splitAt i str
                        -- TODO: Fix version parsing.
                        id = PackageIdentifier { pkgName = PackageName name, pkgVersion = read $ drop 1 ver }
                    in lookupSourcePackageId index id

                Nothing ->
                    let pkgList = lookupPackageName index $ PackageName str
                    in snd $ maximumBy (\x y -> compare (fst x) (fst y)) pkgList

    in case lookupPackage' of
        (info : _) -> return info
        [] -> ioError $ userError $ "Could not find package \"" ++ str ++ "\""

compilerFlags :: InstalledPackageInfo -> [String]
compilerFlags info =
    let idirs = map ((++) "-I") $ includeDirs info
    in ccOptions info ++ idirs

linkerFlags :: InstalledPackageInfo -> [String]
linkerFlags info =
    let link = map ((++) "-l")
        ldirs = map ((++) "-L") $ libraryDirs info
    in ldOptions info ++ ldirs ++ link (hsLibraries info) ++ link (extraLibraries info)

runInMode :: Mode -> [String] -> IO ()
runInMode m pkgs = do
    conf <- configureAllKnownPrograms normal defaultProgramConfiguration
    requireProgram normal ghcProgram conf
    requireProgram normal ghcPkgProgram conf

    index <- getInstalledPackages normal [GlobalPackageDB, UserPackageDB] conf
    infos <- mapM (lookupPackage index) pkgs

    let flags :: InstalledPackageInfo -> [String]
        flags info = 
            case m of
                CompilerMode -> compilerFlags info
                LinkerMode -> linkerFlags info
                BothMode -> compilerFlags info ++ linkerFlags info

        allFlags = nub $ concatMap flags infos

    putStrLn $ intercalate " " allFlags

main :: IO ()
main = do
    args <- getArgs

    case getOpt Permute options args of
        (opts, pkgs, []) -> runInMode (last $ BothMode : opts) pkgs
        (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo "Usage: flagsForPackage [-clb] libraries..." options
