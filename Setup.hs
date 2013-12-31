module Main (main) where

import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Ar
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Verbosity

import Distribution.System
         ( OS(..), buildOS )

import System.IO
import System.Process
import System.FilePath
import System.Directory
import System.Exit

import Control.Applicative
import Control.Monad

import Data.List
import Data.Monoid
import Data.Maybe

-------------------------------------------------
-- Horrible mess of semi-general code
--

-- Not particularly thread safe, but the whole notion of a current directory isn't either
inDirectory :: FilePath -> IO r -> IO r
inDirectory dir action = do
  old <- getCurrentDirectory
  setCurrentDirectory dir
  res <- action
  setCurrentDirectory old
  return res


openBLASBuildArgs = case buildOS of 
        OSX -> ["DYNAMIC_ARCH=1", "CC=clang", "USE_THREAD=1", "NO_SHARED=1"] -- clang should be used on OS X
        _ -> ["DYNAMIC_ARCH=1",  "USE_THREAD=1", "NO_SHARED=1"]

-- TODO: support Windows nicely
runOrBomb :: FilePath -> [String] -> IO ()
runOrBomb cmd args = do
  (c, out, err) <- readProcessWithExitCode cmd args ""
  case c of
    ExitSuccess -> return ()
    ExitFailure e -> do
      hPutStrLn stderr $ "Command \"" ++ unwords (cmd:args) ++ "\" failed with exit code: " ++ show e
      hPutStrLn stdout $ out
      hPutStrLn stderr $ err
      exitWith $ ExitFailure e


getDist = do
  -- let Flag relDistDir = Flag defaultDistPref `mappend` configDistPref flags
  -- canonicalizePath relDistDir
  canonicalizePath "dist" -- ugly. There must be a safer way to get the absolute path of the dist dir

openBlASVersion = "head" -- we're not using this yet
openBlASroot = "OpenBLAS"        

makeOpenBLAS :: FilePath -> IO ()
makeOpenBLAS distDir =
  inDirectory openBlASroot $ do
    putStrLn $ "--> Building OpenBLAS ..." -- openBLASVersion
    runOrBomb "make" openBLASBuildArgs
    runOrBomb "make" ["install"]


openBLASHooks :: UserHooks
openBLASHooks = autoconfUserHooks
    { preConf   = openBLASPreConf
    , postConf  = openBLASPostConf
    , confHook  = openBLASConfHook
    , preBuild  = openBLASPreBuild
    , postBuild = openBLASPostBuild
    , postClean = openBLASPostClean
    }
  where
  openBLASConfHook (pkg, pbi) flags = do
    distDir <- getDist
    lbi <- confHook autoconfUserHooks (pkg, pbi) flags
    let     lpd = localPkgDescr lbi
            lib = fromJust (library lpd)
            libbi = libBuildInfo lib
            libbi' = libbi { extraLibDirs = (distDir </> "lib") : extraLibDirs libbi }
            lib' = lib { libBuildInfo = libbi' }
            lpd' = lpd { library = Just lib' }
    return lbi { localPkgDescr = lpd' }
 
  -- We need to create the "include" directory at some point, but we're doing it this early to make cabal
  -- shut up about it not being present.
  openBLASPreConf args flags = do
        distDir <- getDist
        createDirectory' $ distDir </> "include"
        createDirectory' $ distDir </> "lib"
        createDirectory' $ distDir </> "tmp"
        return emptyHookedBuildInfo -- ?

  openBLASPostConf args flags pkg_descr lbi = do
        (postConf simpleUserHooks) args flags pkg_descr lbi
        --configureOpenBLAS =<< getDist

  openBLASPreBuild args flags = do
        (preBuild simpleUserHooks) args flags
        distDir <- getDist
        makeOpenBLAS distDir 
        --putStrLn $ "Determining MPFR constants..."
        --programExists <- doesFileExist $ distDir </> "mkMpfrDerivedConstants"
      
        return emptyHookedBuildInfo --?

  openBLASPostBuild args flags pkg_descr lbi = do
        distDir <- getDist
        (ar, _) <- requireProgram silent arProgram defaultProgramDb

        putStrLn "Mangling static library..."
        inDirectory (distDir </> "tmp") $ do
            runOrBomb "ar" ["-x", distDir </> "build" </> "libHSrounded-0.0.1.a"]
            runOrBomb "ar" ["-x", distDir </> "lib" </> "libOpenBLAS.a"]

        objects <- map ((distDir </> "tmp") </>) <$> filter (".o" `isSuffixOf`) <$> getDirectoryContents (distDir </> "tmp")

        createArLibArchive silent ar (distDir </> "build" </> "libHShOpenBLAS-0.1.0.0.a") objects

        (postBuild simpleUserHooks) args flags pkg_descr lbi

  openBLASPostClean args flags pkg_descr _ = do
    inDirectory mpfrRoot (readProcessWithExitCode "make" ["distclean"] "")
    return ()