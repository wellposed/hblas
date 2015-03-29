{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}

{- | The 'Numerical.HBLAS.BLAS' module provides a fully general
yet type safe BLAS API.

When in doubt about the semantics of an operation,
consult your system's BLAS api documentation, or just read the documentation
for
<https://software.intel.com/sites/products/documentation/hpc/mkl/mklman/index.htm the Intel MKL BLAS distribution>

A few basic notes about how to invoke BLAS routines.

Many BLAS operations take one or more arguments of type 'Transpose'.
'Tranpose' has the following different constructors, which tell BLAS
routines what transformation to implicitly apply to an input matrix @mat@ with dimension @n x m@.

*  'NoTranspose' leaves the matrix @mat@ as is.

* 'Transpose' treats the @mat@ as being implicitly transposed, with dimension
    @m x n@. Entry @mat(i,j)@ being treated as actually being the entry
    @mat(j,i)@. For Real matrices this is also the matrix adjoint operation.
    ie @Tranpose(mat)(i,j)=mat(j,i)@

*  'ConjNoTranspose' will implicitly conjugate @mat@, which is a no op for Real ('Float' or 'Double') matrices, but for
'Complex Float' and 'Complex Double' matrices, a given matrix entry @mat(i,j)==x':+'y@
will be treated as actually being  @conjugate(mat)(i,j)=y':+'x@.

* 'ConjTranpose' will implicitly transpose and conjugate the input matrix.
ConjugateTranpose acts as matrix adjoint for both real and complex matrices.



The *gemm operations  work as follows (using 'sgemm' as an example):

* @'sgemm trLeft trRight alpha beta left right result'@, where @trLeft@ and @trRight@
are values of type 'Transpose' that respectively act on the matrices @left@ and @right@.

* the generalized matrix computation thusly formed can be viewed as being
@result = alpha * trLeft(left) * trRight(right) + beta * result@


the *gemv operations are akin to the *gemm operations, but with @right@ and @result@
being vectors rather than matrices.


the *trsv operations solve for @x@ in the equation @A x = y@ given @A@ and @y@.
The 'MatUpLo' argument determines if the matrix should be treated as upper or
lower triangular and 'MatDiag' determines if the triangular solver should treat
the diagonal of the matrix as being all 1's or not.  A general pattern of invocation
would be @'strsv' matuplo  tranposeMatA  matdiag  matrixA  xVector@.
A key detail to note is that the input vector is ALSO the result vector,
ie 'strsv' and friends updates the vector place.

-}

module Numerical.HBLAS.BLAS(
        GemvFun
        ,GemmFun
        ,SymmFun
        ,TrsvFun

        ,dgemm
        ,sgemm
        ,cgemm
        ,zgemm

        ,ssymm
        ,dsymm
        ,csymm
        ,zsymm

        ,sgemv
        ,dgemv
        ,cgemv
        ,zgemv

        ,sger
        ,dger

        ,strsv
        ,dtrsv
        ,ctrsv
        ,ztrsv
            ) where


import Numerical.HBLAS.UtilsFFI
import Numerical.HBLAS.BLAS.FFI
import Numerical.HBLAS.BLAS.Internal
import Control.Monad.Primitive
import Data.Complex




sgemm :: PrimMonad m=>  GemmFun Float  orient  (PrimState m) m
sgemm =  gemmAbstraction "sgemm"  cblas_sgemm_safe cblas_sgemm_unsafe (\x f -> f x )


dgemm :: PrimMonad m=>  GemmFun  Double orient  (PrimState m) m
dgemm = gemmAbstraction "dgemm"  cblas_dgemm_safe cblas_dgemm_unsafe  (\x f -> f x )


cgemm :: PrimMonad m=>  GemmFun (Complex Float) orient  (PrimState m) m
cgemm = gemmAbstraction "cgemm" cblas_cgemm_safe cblas_cgemm_unsafe  withRStorable_

zgemm :: PrimMonad m=>  GemmFun (Complex Double) orient  (PrimState m) m
zgemm = gemmAbstraction "zgemm"  cblas_zgemm_safe cblas_zgemm_unsafe withRStorable_

ssymm :: PrimMonad m=>  SymmFun Float orient (PrimState m) m
ssymm = symmAbstraction "ssymm" cblas_ssymm_safe cblas_ssymm_unsafe (\x f -> f x)

dsymm :: PrimMonad m=>  SymmFun Double orient (PrimState m) m
dsymm = symmAbstraction "dsymm" cblas_dsymm_safe cblas_dsymm_unsafe (\x f -> f x)

csymm :: PrimMonad m=>  SymmFun (Complex Float) orient (PrimState m) m
csymm = symmAbstraction "csymm" cblas_csymm_safe cblas_csymm_unsafe withRStorable_

zsymm :: PrimMonad m=>  SymmFun (Complex Double) orient (PrimState m) m
zsymm = symmAbstraction "zsymm" cblas_zsymm_safe cblas_zsymm_unsafe withRStorable_

sgemv :: PrimMonad m => GemvFun Float orient (PrimState m) m
sgemv = gemvAbstraction "sgemv" cblas_sgemv_safe cblas_sgemv_unsafe (flip ($))

dgemv :: PrimMonad m => GemvFun Double orient (PrimState m) m
dgemv = gemvAbstraction "dgemv" cblas_dgemv_safe cblas_dgemv_unsafe (flip ($))

cgemv :: PrimMonad m => GemvFun (Complex Float) orient (PrimState m) m
cgemv = gemvAbstraction "cgemv" cblas_cgemv_safe cblas_cgemv_unsafe withRStorable_

zgemv :: PrimMonad m => GemvFun (Complex Double) orient (PrimState m) m
zgemv = gemvAbstraction "zgemv" cblas_zgemv_safe cblas_zgemv_unsafe withRStorable_
strsv :: PrimMonad m => TrsvFun Float orient (PrimState m) m
strsv = trsvAbstraction "strsv" cblas_strsv_safe cblas_strsv_unsafe

dtrsv :: PrimMonad m => TrsvFun Double orient (PrimState m) m
dtrsv = trsvAbstraction "dtrsv" cblas_dtrsv_safe cblas_dtrsv_unsafe

ctrsv :: PrimMonad m => TrsvFun (Complex Float) orient (PrimState m) m
ctrsv = trsvAbstraction "ctrsv" cblas_ctrsv_safe cblas_ctrsv_unsafe

ztrsv :: PrimMonad m => TrsvFun (Complex Double) orient (PrimState m) m
ztrsv = trsvAbstraction "ztrsv" cblas_ztrsv_safe cblas_ztrsv_unsafe

sger :: PrimMonad m => GerFun Float orient (PrimState m) m
sger = gerAbstraction "sger" cblas_sger_safe cblas_sger_unsafe

dger :: PrimMonad m => GerFun Double orient (PrimState m) m
dger = gerAbstraction "dger" cblas_dger_safe cblas_dger_unsafe
