# About

hOpenBLAS is a self contained full BLAS and LAPACK binding that provides the 
full BLAS and LAPACKE APIs in a simple, unopinionated, Haskell wrapper. 

This library is *NOT* meant to be used by end users, it is designed to be 
an unopinionated, simple, portable, easy to install BLAS/LAPACK substrate for higher level numerical
computing libraries to build upon. Morever, this library is strictly a wrapper,
and simply makes using the functionality of OpenBLAS more accessible.

hOpenBLAS requires having gfortran or another fortran compiler installed in 
order to build, along with openBLAS itself. 

## how to install
* On OS X systems, ```brew tap homebrew/science ; brew install openblas ``` will handle 
all the prereqs for using hOpenBLAS.
* On Linux, BSD, and other unix systems, a dev version of OpenBLAS is often available via the system package manager.
* Otherwise, read the documentation at https://github.com/xianyi/OpenBLAS for
how to install on your system. This will typically involve:
```git clone https://github.com/xianyi/OpenBLAS ; cd OpenBLAS ; make ; make install``` 

once openblas is installed, hOpenBLAS can be easily installed just like any other haskell library,
if your cloned this repo, ``` cabal install``` will work, otherwise ``` cabal install hOpenBLAS``` will do the trick.

## getting involved
patches, bug reports,  and other contributions welcome.

[![Build Status](https://secure.travis-ci.org/wellposed/hOpenBLAS.png?branch=master)](http://travis-ci.org/wellposed/hybrid-vectors)