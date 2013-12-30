{-# LANGUAGE Rank2Types, DeriveFunctor, CPP  #-}

import Control.Applicative
import Distribution.Simple

import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.Types
import Distribution.Simple.Program

import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup

import System.Directory
import System.Cmd 
import Data.List 

--import System.Process 
--import System.Environment
--import System.SetEnv

import Control.Exception (SomeException, try)
import Control.Monad
import Data.Maybe

import Data.Monoid


import Distribution.Version
import System.FilePath
import Distribution.System
 

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
infixl 1 &
infixr 4 .~, %~
infixl 8 ^.

-- get
s ^. l = getConst (l Const s)
x & f = f x
-- functional set
(%~) l f s = l (\a () -> f a) s ()
-- strong update
(.~) l b s = l (\_ () -> b) s ()

-- make DYNAMIC_ARCH=1 CC=clang USE_THREAD=1 -j1 NO_SHARED=1


--main =    defaultMainWithHooks myhook 


#if darwin_HOST_OS 
buildOpenBLAS = "make DYNAMIC_ARCH=1 CC=clang USE_THREAD=1 -j1 NO_SHARED=1"
#else
buildOpenBLAS= "make DYNAMIC_ARCH=1  USE_THREAD=1 -j1 NO_SHARED=1 "
#endif


myhook = simpleUserHooks & _preConf %~ (\f args confargs ->  
            do buildBLIS ; f args confargs ) 

--(ldLibraryPathVar, ldLibraryPathSep) = 
--        case buildOS of
--          OSX -> ("DYLD_LIBRARY_PATH",":")
          --_ -> ("LD_LIBRARY_PATH",":")

--addToLdLibraryPath s = do
--     v <- try $ getEnv ldLibraryPathVar :: IO (Either SomeException String)
--     setEnv ldLibraryPathVar (s ++ either (const "") (ldLibraryPathSep ++) v)




--addOpenBLAStoLdLibraryPath  = do
--    libDir <- makeRelativeToCurrentDirectory "OpenBLAS/"
--    addToLdLibraryPath libDir

adjustLinking cwd =  combine cwd "OpenBLAS/libopenblas.a"    

main = do defaultMainWithHooks myhooks


getAllTheObjs = do 
        cwd <-getCurrentDirectory
        getDirectoryContents "priv-objs"

myhooks = simpleUserHooks {

    confHook = \(genericPackageDescription, hookedBuildInfo) configFlags -> do
        cwd <- getCurrentDirectory 
        includeDirs <-   return  [combine cwd "OpenBLAS/"]-- liftM lines $ llvmConfig ["--includedir"]
        libDirs@[libDir] <- return [combine cwd  "OpenBLAS/"]  -- liftM lines $ llvmConfig 
        putStrLn $ show libDirs
        let configFlags' = configFlags {
            configExtraLibDirs = libDirs ++ configExtraLibDirs configFlags,
            configExtraIncludeDirs = includeDirs ++ configExtraIncludeDirs configFlags
                }
        putStrLn $ show $ configExtraIncludeDirs configFlags'
        putStrLn $ show  $ configExtraLibDirs configFlags'
        allTheObjs <- getAllTheObjs
        (confHook simpleUserHooks )  (genericPackageDescription,(\(hbi,rest )-> 
            ( fmap (\bi ->  bi{ldOptions =allTheObjs ++[ adjustLinking cwd] ++ ldOptions bi }) hbi 
                , rest)) hookedBuildInfo) configFlags'

    ,buildHook = \packageDescription localBuildInfo userHooks buildFlags -> do

        (buildHook simpleUserHooks ) packageDescription localBuildInfo userHooks buildFlags
    ,postBuild = \ args buildFlags packageDescription localBuildInfo -> 
            do 
                --- THIS MAY BE WILDLY UNPORTABLE AND NEED OS / ARCH SPECIFIC HACKS
                staticLibs <- fmap (filter (\x -> isSuffixOf ".a" x )) $ getDirectoryContents "dist/build/"
                -- dyLibs <- fmap (filter (\x -> isSuffixOf ".dylib" x ) $ getDirectoryContents "dist/build/"    -- ?
                -- theObjs <- getAllTheObjs
                mapM_ (\libname -> 
                     system  ( "libtool -static -o " ++ ("dist/build/"++libname) ++ " " ++ ("dist/build/"++libname) ++ " OpenBLAS/libopenblas.a") ) staticLibs
                (postBuild simpleUserHooks) args buildFlags packageDescription localBuildInfo

    ,testHook = \packageDescription localBuildInfo userHooks testFlags -> do
        (testHook simpleUserHooks) packageDescription localBuildInfo userHooks testFlags

    --,haddockHook = \packageDescription localBuildInfo userHooks haddockFlags -> do
    --    let v = "GHCRTS"
    --    oldGhcRts <- try $ getEnv v :: IO (Either SomeException String)
    --    setEnv v (either (const id) (\o n -> o ++ " " ++ n) oldGhcRts "-K32M")
    --    haddockHook simpleUserHooks packageDescription localBuildInfo userHooks haddockFlags
    --    either (const (unsetEnv v)) (setEnv v) oldGhcRts
   }  & _preConf %~ (\f args confargs ->  
            do buildBLIS ; f args confargs ) 

--for now this likely won't work on windows, but patches welcome

buildBLIS = do  putStrLn "OpenBLAS is built at configure time. This can take a while! Make sure you have gfortran installed too."

                freshBlis <- doesDirectoryExist "OpenBLAS"
                if  not  freshBlis then system "git clone  git@github.com:xianyi/OpenBLAS.git"
                    else do  system "cd OpenBLAS ; git pull origin develop"
                preBuilt <- doesFileExist "OpenBLAS/libopenblas.a"
                if not preBuilt  then 
                    do  (system $ "cd OpenBLAS ; "++ buildOpenBLAS );
                        (system  $ "cd OpenBLAS ; "++ buildOpenBLAS ); -- some weird build error happens if its only once
                        return () 
                    else putStrLn "OpenBLAS already built"
  

-- 
_preConf f conf = fmap (\el -> conf{preConf=el}) (f $ preConf $ conf)
