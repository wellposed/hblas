[![Wellposed](http://www.wellposed.com/mini.png)](http://www.wellposed.com)® 

# About hOpenBLAS

hOpenBLAS is an open source component of the [Wellposed](http://www.wellposed.com)® mathematical software suite. 

[![Build Status](https://secure.travis-ci.org/wellposed/hOpenBLAS.png?branch=master)](http://travis-ci.org/wellposed/hOpenBLAS)

hOpenBLAS is a self contained full (well, not quite yet) BLAS and LAPACK binding that provides the 
full BLAS and LAPACKE APIs in a simple, unopinionated, Haskell wrapper. 

This library is *NOT* meant to be used by end users, it is designed to be 
an unopinionated, simple, portable, easy to install BLAS/LAPACK substrate for higher level numerical
computing libraries to build upon. Morever, this library is strictly a wrapper,
and simply makes using the functionality of OpenBLAS more accessible.

This library is *NOT* meant to be used a standalone array library (except in desperation),
but rather should be used by a higher level numerical array library to provide 
high performance linear algebra routines. 

hOpenBLAS requires having gfortran or another fortran compiler installed in 
order to build, along with OpenBLAS itself.  For now, only OpenBLAS is supported, because it is the only 
actively maintained BLAS that is multi-threaded and threadsafe by default. 
Patches to add support for other threadsafe BLAS/LAPACK variants is welcome. 

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
```
git clone https://github.com/xianyi/OpenBLAS
cd OpenBLAS 
make
make install PREFIX=$yourPrefixHere # eg PREFIX=/usr on linux systems
``` 
note that if you plan to deploy applications on multiple machines
that have different cpu versions, you may want to build openblas with
``` make DYNAMIC_ARCH=1 NO_SHARED=1 ; make install NO_SHARED=1 PREFIX=$blah```
and on OSX systems you may want to also pass ```CC=clang``` to the build command.


once openblas is installed, hOpenBLAS can be easily installed just like any other haskell library,
if your cloned this repo, ``` cabal install``` will work, 
otherwise ``` cabal install hOpenBLAS``` will do the trick.

## getting involved
patches, bug reports,  and other contributions welcome.
