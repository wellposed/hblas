[![Wellposed](http://www.wellposed.com/mini.png)](http://www.wellposed.com)® 

# About hblas

hblas is an open source component of the [Wellposed](http://www.wellposed.com)® mathematical software suite. 

Members of the numerical haskell open source community can be found on irc at  `#numerical-haskell` on freenode, 
and via the [numericalhaskell mailing list](https://groups.google.com/forum/#!forum/numericalhaskell). 


[![Build Status](https://secure.travis-ci.org/wellposed/hblas.png?branch=master)](http://travis-ci.org/wellposed/hblas)


hblas is a self contained full (well, not quite yet) BLAS and LAPACK binding that provides the 
full BLAS and LAPACKE APIs in a simple, unopinionated, Haskell wrapper. 

This library is *NOT* meant to be used by end users, it is designed to be 
an unopinionated, simple, portable, easy to install BLAS/LAPACK substrate for higher level numerical
computing libraries to build upon. Morever, this library is strictly a wrapper,
and simply makes using the functionality of BLAS and LAPACK more accessible.

This library is *NOT* meant to be used a standalone array library (except in desperation),
but rather should be used by a higher level numerical array library to provide 
high performance linear algebra routines. 


## how to install (using openblas)
By default, hblas will assume you have OpenBLAS built and installed, including
the LAPACKE interfaces for LAPACK, somewhere in your standard library path.

* On OSX systems, 




* On OS X systems, things will just work.
* On linux and bsd systems, the equivalent of 
```
sudo apt-get install libblas liblapack
```
is all you should have to do before hand

## getting involved
patches, bug reports, tests,  and other contributions welcome.

Want to add a new routine, check out the ones listed in the [lapack section](http://software.intel.com/sites/products/documentation/hpc/mkl/mklman/index.htm) of the Intel MKL manual to get some human
readable documentation.