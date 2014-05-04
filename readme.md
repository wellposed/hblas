[![Wellposed](http://www.wellposed.com/mini.png)](http://www.wellposed.com)™

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

## Install

By default, hblas will assume you have BLAS and LAPACK built and installed.

### OSX

On OS X systems, things will just work.

```bash
$ cabal install
```

### Linux

On linux and bsd systems, you will need to manually install the BLAS and LAPACK libraries beforehand.

```bash
$ sudo apt-get install libblas liblapack
$ cabal install
```

## Testing

To run the test suite execute:

```bash
$ cabal test
```

## Linking
If you get an error like `undefined reference to 'cblas_sdsdot'` when building or running an HBLAS program,
you might be on a system that builds BLAS and CBLAS separately, such as Arch Linux.

In which case, be sure to install CBLAS and invoke `cabal install hblas -fCBLAS`
to make sure `hblas` links to CBLAS properly.

## Usage

API is subject to change.

```haskell
import Foreign.Storable
import Numerical.HBLAS.BLAS
import Numerical.HBLAS.MatrixTypes

-- Generate the constant mutable square matrix of the given type and dimensions.
constMatrix :: Storable a => Int -> a -> IO (IODenseMatrix Row a)
constMatrix n k = generateMutableDenseMatrix SRow (n,n) (const k)

example_dgemm :: IO ()
example_dgemm = do
    left  <- constMatrix 2 (2 :: Double)
    right <- constMatrix 2 (3 :: Double)
    out   <- constMatrix 2 (0 :: Double)

    dgemm NoTranspose NoTranspose 1.0 1.0 left right res

    resulting <- mutableVectorToList $ _bufferDenMutMat out
    print resulting
```

## Getting Involved

Patches, bug reports, tests, and other contributions welcome.

If you want to add a new routine, check out the ones listed in the [lapack section](http://software.intel.com/sites/products/documentation/hpc/mkl/mklman/index.htm) of the Intel MKL manual to get some human
readable documentation.

## Commercial Support

*I have > 32bit size arrays, help!*

Congrats, you have ``big compute on big data'', contact [Carter](http://www.wellposed.com)
and we'll try to help you out.
