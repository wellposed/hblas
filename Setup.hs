{-# LANGUAGE Rank2Types, DeriveFunctor #-}

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

myhook = simpleUserHooks & _preConf %~ (\f args confargs -> do buildBLIS ; f args confargs)

--for now this likely won't work on windows, but patches welcome
buildBLIS = do  freshBlis <- doesDirectoryExist "blis"
                if  freshBlis then system "git clone  git@github.com:xianyi/OpenBLAS.git"
                 -- ; cd OpenBLAS ; git checkout master"                 
                
                    -- for now lets just clone from the main repo
                    else do  system "cd OpenBLAS ; git pull origin develop"
                    system "cd OpenBLAS ;  make DYNAMIC_ARCH=1 CC=clang USE_THREAD=1 -j1 NO_SHARED=1 "
                    -- yess, doing it tt
                    system "make DYNAMIC_ARCH=1 CC=clang USE_THREAD=1 -j1 NO_SHARED=1"

_preConf f conf = fmap (\el -> conf{preConf=el}) (f $ preConf $ conf)
