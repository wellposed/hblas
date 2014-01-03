
[![Build Status](https://secure.travis-ci.org/wellposed/hOpenBLAS.png?branch=master)](http://travis-ci.org/wellposed/hybrid-vectors)

# About

hOpenBLAS is a self contained full BLAS and LAPACK binding that provides the 
full BLAS and LAPACKE APIs in a simple, unopinionated, Haskell wrapper. 

This library is *NOT* meant to be used by end users, it is designed to be 
an unopinionated, simple, portable, easy to install BLAS/LAPACK substrate for higher level numerical
computing libraries to build upon. Morever, this library is strictly a wrapper,
and simply makes using the functionality of OpenBLAS more accessible.

hOpenBLAS requires having gfortran or another fortran compiler installed in 
order to build, along with OpenBLAS itself.  For now, only OpenBLAS is supported, because it is the only 
actively maintained BLAS that is multi-threaded and threadsafe by default. 
Patches to add support for other thread safe BLAS/LAPACK variants is welcome. 

## how to install
* On OS X systems, ```brew tap homebrew/science ; brew install openblas ``` will handle 
all the prereqs for using hOpenBLAS.
* On Linux, BSD, and other unix systems, a dev version of OpenBLAS is often 
available via the system package manager. __ NOTE: __ some linux package managers (i'm looking at you, debian / ubuntu) don't provide a LAPACK built with OpenBLAS. Don't use these packages. 
Many BLAS / LAPACK variants,  including the reference ones and Atlas, aren't threadsafe! 
OpenBLAS is thread safe, and thus  when LAPACK is built using OpenBLAS 
(as in the standard OpenBLAS build), it is threadsafe too.  
* Otherwise, read the documentation at https://github.com/xianyi/OpenBLAS for
how to install on your system. This will typically involve:
```git clone https://github.com/xianyi/OpenBLAS ; cd OpenBLAS ; make ; make install PREFIX=$yourPrefixHere``` 

once openblas is installed, hOpenBLAS can be easily installed just like any other haskell library,
if your cloned this repo, ``` cabal install``` will work, 
otherwise ``` cabal install hOpenBLAS``` will do the trick.

## getting involved
patches, bug reports,  and other contributions welcome.
