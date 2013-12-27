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


main =    defaultMainWithHooks myhook 


#if darwin_HOST_OS 
buildOpenBLAS = "make DYNAMIC_ARCH=1 CC=clang USE_THREAD=1 -j1 NO_SHARED=1"
#else
buildOpenBLAS= "make DYNAMIC_ARCH=1  USE_THREAD=1 -j1 NO_SHARED=1 "
#endif


myhook = simpleUserHooks & _preConf %~ (\f args confargs ->  
            do buildBLIS ; f args confargs ) 




--for now this likely won't work on windows, but patches welcome
buildBLIS = do  putStrLn "OpenBLAS is built at configure time. This can take a while! Make sure you have gfortran installed too."
                freshBlis <- doesDirectoryExist "OpenBLAS"
                if  freshBlis then system "git clone  git@github.com:xianyi/OpenBLAS.git"
                    else do  system "cd OpenBLAS ; git pull origin develop"
                system  $ "cd OpenBLAS ; "++ buildOpenBLAS
  

-- 
_preConf f conf = fmap (\el -> conf{preConf=el}) (f $ preConf $ conf)
