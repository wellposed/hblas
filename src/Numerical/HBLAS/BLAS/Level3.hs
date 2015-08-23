{-# LANGUAGE BangPatterns , RankNTypes, GADTs, DataKinds #-}

{- | The 'Numerical.HBLAS.BLAS.Level3' module provides a fully general
yet type safe Level3 BLAS API.

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

module Numerical.HBLAS.BLAS.Level3(
        dgemm
        ,sgemm
        ,cgemm
        ,zgemm

        ,chemm
        ,zhemm

        ,cherk
        ,zherk

        ,cher2k
        ,zher2k

        ,ssymm
        ,dsymm
        ,csymm
        ,zsymm

        ,ssyrk
        ,dsyrk
        ,csyrk
        ,zsyrk

        ,ssyr2k
        ,dsyr2k
        ,csyr2k
        ,zsyr2k

        ,strmm
        ,dtrmm
        ,ctrmm
        ,ztrmm

        ,strsm
        ,dtrsm
        ,ctrsm
        ,ztrsm
            ) where


import Numerical.HBLAS.UtilsFFI
import Numerical.HBLAS.BLAS.FFI.Level3
import Numerical.HBLAS.BLAS.Internal.Level3
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

chemm :: PrimMonad m=>  HemmFun (Complex Float) orient (PrimState m) m
chemm = hemmAbstraction "chemm" cblas_chemm_safe cblas_chemm_unsafe withRStorable_

zhemm :: PrimMonad m=>  HemmFun (Complex Double) orient (PrimState m) m
zhemm = hemmAbstraction "zhemm" cblas_zhemm_safe cblas_zhemm_unsafe withRStorable_

cherk :: PrimMonad m=>  HerkFun Float (Complex Float) orient (PrimState m) m
cherk = herkAbstraction "cherk" cblas_cherk_safe cblas_cherk_unsafe (\x f -> f x)

zherk :: PrimMonad m=>  HerkFun Double (Complex Double) orient (PrimState m) m
zherk = herkAbstraction "zherk" cblas_zherk_safe cblas_zherk_unsafe (\x f -> f x)

cher2k :: PrimMonad m=>  Her2kFun Float (Complex Float) orient (PrimState m) m
cher2k = her2kAbstraction "cher2k" cblas_cher2k_safe cblas_cher2k_unsafe withRStorable_

zher2k :: PrimMonad m=>  Her2kFun Double (Complex Double) orient (PrimState m) m
zher2k = her2kAbstraction "zher2k" cblas_zher2k_safe cblas_zher2k_unsafe withRStorable_

ssymm :: PrimMonad m=>  SymmFun Float orient (PrimState m) m
ssymm = symmAbstraction "ssymm" cblas_ssymm_safe cblas_ssymm_unsafe (\x f -> f x)

dsymm :: PrimMonad m=>  SymmFun Double orient (PrimState m) m
dsymm = symmAbstraction "dsymm" cblas_dsymm_safe cblas_dsymm_unsafe (\x f -> f x)

csymm :: PrimMonad m=>  SymmFun (Complex Float) orient (PrimState m) m
csymm = symmAbstraction "csymm" cblas_csymm_safe cblas_csymm_unsafe withRStorable_

zsymm :: PrimMonad m=>  SymmFun (Complex Double) orient (PrimState m) m
zsymm = symmAbstraction "zsymm" cblas_zsymm_safe cblas_zsymm_unsafe withRStorable_

ssyrk :: PrimMonad m=>  SyrkFun Float orient (PrimState m) m
ssyrk = syrkAbstraction "ssyrk" cblas_ssyrk_safe cblas_ssyrk_unsafe (\x f -> f x)

dsyrk :: PrimMonad m=>  SyrkFun Double orient (PrimState m) m
dsyrk = syrkAbstraction "dsyrk" cblas_dsyrk_safe cblas_dsyrk_unsafe (\x f -> f x)

csyrk :: PrimMonad m=>  SyrkFun (Complex Float) orient (PrimState m) m
csyrk = syrkAbstraction "csyrk" cblas_csyrk_safe cblas_csyrk_unsafe withRStorable_

zsyrk :: PrimMonad m=>  SyrkFun (Complex Double) orient (PrimState m) m
zsyrk = syrkAbstraction "zsyrk" cblas_zsyrk_safe cblas_zsyrk_unsafe withRStorable_

ssyr2k :: PrimMonad m=>  Syr2kFun Float orient (PrimState m) m
ssyr2k = syr2kAbstraction "ssyr2k" cblas_ssyr2k_safe cblas_ssyr2k_unsafe (\x f -> f x)

dsyr2k :: PrimMonad m=>  Syr2kFun Double orient (PrimState m) m
dsyr2k = syr2kAbstraction "dsyr2k" cblas_dsyr2k_safe cblas_dsyr2k_unsafe (\x f -> f x)

csyr2k :: PrimMonad m=>  Syr2kFun (Complex Float) orient (PrimState m) m
csyr2k = syr2kAbstraction "csyr2k" cblas_csyr2k_safe cblas_csyr2k_unsafe withRStorable_

zsyr2k :: PrimMonad m=>  Syr2kFun (Complex Double) orient (PrimState m) m
zsyr2k = syr2kAbstraction "zsyr2k" cblas_zsyr2k_safe cblas_zsyr2k_unsafe withRStorable_

strmm :: PrimMonad m=> TrmmFun Float orient (PrimState m) m
strmm = trmmAbstraction "strmm" cblas_strmm_safe cblas_strmm_unsafe (\x f -> f x)

dtrmm :: PrimMonad m=> TrmmFun Double orient (PrimState m) m
dtrmm = trmmAbstraction "dtrmm" cblas_dtrmm_safe cblas_dtrmm_unsafe (\x f -> f x)

ctrmm :: PrimMonad m=> TrmmFun (Complex Float) orient (PrimState m) m
ctrmm = trmmAbstraction "ctrmm" cblas_ctrmm_safe cblas_ctrmm_unsafe withRStorable_

ztrmm :: PrimMonad m=> TrmmFun (Complex Double) orient (PrimState m) m
ztrmm = trmmAbstraction "ztrmm" cblas_ztrmm_safe cblas_ztrmm_unsafe withRStorable_

strsm :: PrimMonad m=> TrsmFun Float orient (PrimState m) m
strsm = trsmAbstraction "strsm" cblas_strsm_safe cblas_strsm_unsafe (\x f -> f x)

dtrsm :: PrimMonad m=> TrsmFun Double orient (PrimState m) m
dtrsm = trsmAbstraction "dtrsm" cblas_dtrsm_safe cblas_dtrsm_unsafe (\x f -> f x)

ctrsm :: PrimMonad m=> TrsmFun (Complex Float) orient (PrimState m) m
ctrsm = trsmAbstraction "ctrsm" cblas_ctrsm_safe cblas_ctrsm_unsafe withRStorable_

ztrsm :: PrimMonad m=> TrsmFun (Complex Double) orient (PrimState m) m
ztrsm = trsmAbstraction "ztrsm" cblas_ztrsm_safe cblas_ztrsm_unsafe withRStorable_
