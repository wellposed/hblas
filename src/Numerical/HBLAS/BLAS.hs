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
        AsumFun
        ,AxpyFun
        ,GemvFun
        ,GemmFun
        ,SymmFun
        ,TrsvFun

        -- Level 1
        ,sasum
        ,dasum
        ,scasum
        ,dzasum

        ,saxpy
        ,daxpy
        ,caxpy
        ,zaxpy

        ,scopy
        ,dcopy
        ,ccopy
        ,zcopy

        ,sdot
        ,ddot
        ,sdsdot
        ,dsdot

        ,cdotu
        ,cdotc
        ,zdotu
        ,zdotc

        ,snrm2
        ,dnrm2
        ,scnrm2
        ,dznrm2

        ,srot
        ,drot

        ,srotg
        ,drotg

        ,srotm
        ,drotm

        ,srotmg
        ,drotmg

        ,sscal
        ,dscal
        ,cscal
        ,zscal
        ,csscal
        ,zdscal

        ,sswap
        ,dswap
        ,cswap
        ,zswap

        ,isamax
        ,idamax
        ,icamax
        ,izamax

{-
        ,isamin
        ,idamin
        ,icamin
        ,izamin
-}

        -- Level 2
        ,sgbmv
        ,dgbmv
        ,cgbmv
        ,zgbmv

        ,sger
        ,dger
        ,cgerc
        ,zgerc
        ,cgeru
        ,zgeru

        ,chbmv
        ,zhbmv

        ,chemv
        ,zhemv

        ,cher
        ,zher
        ,cher2
        ,zher2

        ,chpmv
        ,zhpmv

        ,chpr
        ,zhpr
        ,chpr2
        ,zhpr2

        ,ssbmv
        ,dsbmv

        ,sspmv
        ,dspmv

        ,sspr
        ,dspr
        ,sspr2
        ,dspr2

        ,ssymv
        ,dsymv

        ,ssyr
        ,dsyr
        ,ssyr2
        ,dsyr2

        ,stbmv
        ,dtbmv
        ,ctbmv
        ,ztbmv

        ,stbsv
        ,dtbsv
        ,ctbsv
        ,ztbsv

        ,stpmv
        ,dtpmv
        ,ctpmv
        ,ztpmv

        ,stpsv
        ,dtpsv
        ,ctpsv
        ,ztpsv

        ,strmv
        ,dtrmv
        ,ctrmv
        ,ztrmv

        ,strsv
        ,dtrsv
        ,ctrsv
        ,ztrsv

        -- Level 3
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
            ) where


import Numerical.HBLAS.UtilsFFI
import Numerical.HBLAS.BLAS.FFI
import Numerical.HBLAS.BLAS.Internal
import Numerical.HBLAS.BLAS.FFI.Level1
import Numerical.HBLAS.BLAS.FFI.Level2
import Numerical.HBLAS.BLAS.Internal.Level1
import Numerical.HBLAS.BLAS.Internal.Level2
import Control.Monad.Primitive
import Data.Complex


-- Level 1
sasum :: PrimMonad m => AsumFun Float (PrimState m) m Float
sasum = asumAbstraction "sasum" cblas_sasum_safe cblas_sasum_unsafe

dasum :: PrimMonad m => AsumFun Double (PrimState m) m Double
dasum = asumAbstraction "dasum" cblas_dasum_safe cblas_dasum_unsafe

scasum :: PrimMonad m => AsumFun (Complex Float) (PrimState m) m Float
scasum = asumAbstraction "scasum" cblas_scasum_safe cblas_scasum_unsafe

dzasum :: PrimMonad m => AsumFun (Complex Double) (PrimState m) m Double
dzasum = asumAbstraction "dzasum" cblas_dzasum_safe cblas_dzasum_unsafe

saxpy :: PrimMonad m => AxpyFun Float (PrimState m) m
saxpy = axpyAbstraction "saxpy" cblas_saxpy_safe cblas_saxpy_unsafe (\x f -> f x)

daxpy :: PrimMonad m => AxpyFun Double (PrimState m) m
daxpy = axpyAbstraction "daxpy" cblas_daxpy_safe cblas_daxpy_unsafe (\x f -> f x)

caxpy :: PrimMonad m => AxpyFun (Complex Float) (PrimState m) m
caxpy = axpyAbstraction "caxpy" cblas_caxpy_safe cblas_caxpy_unsafe withRStorable_

zaxpy :: PrimMonad m => AxpyFun (Complex Double) (PrimState m) m
zaxpy = axpyAbstraction "zaxpy" cblas_zaxpy_safe cblas_zaxpy_unsafe withRStorable_

scopy :: PrimMonad m => CopyFun Float (PrimState m) m
scopy = copyAbstraction "scopy" cblas_scopy_safe cblas_scopy_unsafe

dcopy :: PrimMonad m => CopyFun Double (PrimState m) m
dcopy = copyAbstraction "dcopy" cblas_dcopy_safe cblas_dcopy_unsafe

ccopy :: PrimMonad m => CopyFun (Complex Float) (PrimState m) m
ccopy = copyAbstraction "ccopy" cblas_ccopy_safe cblas_ccopy_unsafe

zcopy :: PrimMonad m => CopyFun (Complex Double) (PrimState m) m
zcopy = copyAbstraction "zcopy" cblas_zcopy_safe cblas_zcopy_unsafe

sdot :: PrimMonad m => NoScalarDotFun Float (PrimState m) m Float
sdot = noScalarDotAbstraction "sdot" cblas_sdot_safe cblas_sdot_unsafe

ddot :: PrimMonad m => NoScalarDotFun Double (PrimState m) m Double
ddot = noScalarDotAbstraction "ddot" cblas_ddot_safe cblas_ddot_unsafe

sdsdot :: PrimMonad m => ScalarDotFun Float (PrimState m) m Float
sdsdot = scalarDotAbstraction "sdsdot" cblas_sdsdot_safe cblas_sdsdot_unsafe

dsdot :: PrimMonad m => NoScalarDotFun Float (PrimState m) m Double
dsdot = noScalarDotAbstraction "dsdot" cblas_dsdot_safe cblas_dsdot_unsafe

cdotu :: PrimMonad m => ComplexDotFun (Complex Float) (PrimState m) m
cdotu = complexDotAbstraction "cdotu" cblas_cdotu_safe cblas_cdotu_unsafe

cdotc :: PrimMonad m => ComplexDotFun (Complex Float) (PrimState m) m
cdotc = complexDotAbstraction "cdotc" cblas_cdotc_safe cblas_cdotc_unsafe

zdotu :: PrimMonad m => ComplexDotFun (Complex Double) (PrimState m) m
zdotu = complexDotAbstraction "zdotu" cblas_zdotu_safe cblas_zdotu_unsafe

zdotc :: PrimMonad m => ComplexDotFun (Complex Double) (PrimState m) m
zdotc = complexDotAbstraction "zdotc" cblas_zdotc_safe cblas_zdotc_unsafe

snrm2 :: PrimMonad m => Nrm2Fun Float (PrimState m) m Float
snrm2 = norm2Abstraction "snrm2" cblas_snrm2_safe cblas_snrm2_unsafe

dnrm2 :: PrimMonad m => Nrm2Fun Double (PrimState m) m Double
dnrm2 = norm2Abstraction "dnrm2" cblas_dnrm2_safe cblas_dnrm2_unsafe

scnrm2 :: PrimMonad m => Nrm2Fun (Complex Float) (PrimState m) m Float
scnrm2 = norm2Abstraction "scnrm2" cblas_scnrm2_safe cblas_scnrm2_unsafe

dznrm2 :: PrimMonad m => Nrm2Fun (Complex Double) (PrimState m) m Double
dznrm2 = norm2Abstraction "dznrm2" cblas_dznrm2_safe cblas_dznrm2_unsafe

srot :: PrimMonad m => RotFun Float (PrimState m) m
srot = rotAbstraction "srot" cblas_srot_safe cblas_srot_unsafe

drot :: PrimMonad m => RotFun Double (PrimState m) m
drot = rotAbstraction "drot" cblas_drot_safe cblas_drot_unsafe

srotg :: PrimMonad m => RotgFun Float (PrimState m) m
srotg = rotgAbstraction "srotg" cblas_srotg_safe cblas_srotg_unsafe

drotg :: PrimMonad m => RotgFun Double (PrimState m) m
drotg = rotgAbstraction "drotg" cblas_drotg_safe cblas_drotg_unsafe

srotm :: PrimMonad m => RotmFun Float (PrimState m) m
srotm = rotmAbstraction "srotm" cblas_srotm_safe cblas_srotm_unsafe

drotm :: PrimMonad m => RotmFun Double (PrimState m) m
drotm = rotmAbstraction "drotm" cblas_drotm_safe cblas_drotm_unsafe

srotmg :: PrimMonad m => RotmgFun Float (PrimState m) m
srotmg = rotmgAbstraction "srotmg" cblas_srotmg_safe cblas_srotmg_unsafe

drotmg :: PrimMonad m => RotmgFun Double (PrimState m) m
drotmg = rotmgAbstraction "drotmg" cblas_drotmg_safe cblas_drotmg_unsafe

sscal :: PrimMonad m => ScalFun Float Float (PrimState m) m
sscal = scalAbstraction "sscal" cblas_sscal_safe cblas_sscal_unsafe (\x f -> f x )

dscal :: PrimMonad m => ScalFun Double Double (PrimState m) m
dscal = scalAbstraction "dscal" cblas_dscal_safe cblas_dscal_unsafe (\x f -> f x )

cscal :: PrimMonad m => ScalFun (Complex Float) (Complex Float) (PrimState m) m
cscal = scalAbstraction "cscal" cblas_cscal_safe cblas_cscal_unsafe withRStorable_

zscal :: PrimMonad m => ScalFun (Complex Double) (Complex Double) (PrimState m) m
zscal = scalAbstraction "zscal" cblas_zscal_safe cblas_zscal_unsafe withRStorable_

csscal :: PrimMonad m => ScalFun Float (Complex Float) (PrimState m) m
csscal = scalAbstraction "csscal" cblas_csscal_safe cblas_csscal_unsafe (\x f -> f x )

zdscal :: PrimMonad m => ScalFun Double (Complex Double) (PrimState m) m
zdscal = scalAbstraction "zdscal" cblas_zdscal_safe cblas_zdscal_unsafe (\x f -> f x )

sswap :: PrimMonad m => SwapFun Float (PrimState m) m
sswap = swapAbstraction "sswap" cblas_sswap_safe cblas_sswap_unsafe

dswap :: PrimMonad m => SwapFun Double (PrimState m) m
dswap = swapAbstraction "dswap" cblas_dswap_safe cblas_dswap_unsafe

cswap :: PrimMonad m => SwapFun (Complex Float) (PrimState m) m
cswap = swapAbstraction "cswap" cblas_cswap_safe cblas_cswap_unsafe

zswap :: PrimMonad m => SwapFun (Complex Double) (PrimState m) m
zswap = swapAbstraction "zswap" cblas_zswap_safe cblas_zswap_unsafe

isamax :: PrimMonad m => IamaxFun Float (PrimState m) m
isamax = iamaxAbstraction "isamax" cblas_isamax_safe cblas_isamax_unsafe

idamax :: PrimMonad m => IamaxFun Double (PrimState m) m
idamax = iamaxAbstraction "idamax" cblas_idamax_safe cblas_idamax_unsafe

icamax :: PrimMonad m => IamaxFun (Complex Float) (PrimState m) m
icamax = iamaxAbstraction "icamax" cblas_icamax_safe cblas_icamax_unsafe

izamax :: PrimMonad m => IamaxFun (Complex Double)(PrimState m) m
izamax = iamaxAbstraction "izamax" cblas_izamax_safe cblas_izamax_unsafe

{-
isamin :: PrimMonad m => IaminFun Float (PrimState m) m
isamin = iaminAbstraction "isamin" cblas_isamin_safe cblas_isamin_unsafe

idamin :: PrimMonad m => IaminFun Double (PrimState m) m
idamin = iaminAbstraction "idamin" cblas_idamin_safe cblas_idamin_unsafe

icamin :: PrimMonad m => IaminFun (Complex Float) (PrimState m) m
icamin = iaminAbstraction "icamin" cblas_icamin_safe cblas_icamin_unsafe

izamin :: PrimMonad m => IaminFun (Complex Double)(PrimState m) m
izamin = iaminAbstraction "izamin" cblas_izamin_safe cblas_izamin_unsafe
-}

-- Level 2
sgbmv :: PrimMonad m => GbmvFun Float orient (PrimState m) m
sgbmv = gbmvAbstraction "sgbmv" cblas_sgbmv_safe cblas_sgbmv_unsafe (\x f -> f x)

dgbmv :: PrimMonad m => GbmvFun Double orient (PrimState m) m
dgbmv = gbmvAbstraction "dgbmv" cblas_dgbmv_safe cblas_dgbmv_unsafe (\x f -> f x)

cgbmv :: PrimMonad m => GbmvFun (Complex Float) orient (PrimState m) m
cgbmv = gbmvAbstraction "cgbmv" cblas_cgbmv_safe cblas_cgbmv_unsafe withRStorable_

zgbmv :: PrimMonad m => GbmvFun (Complex Double) orient (PrimState m) m
zgbmv = gbmvAbstraction "zgbmv" cblas_zgbmv_safe cblas_zgbmv_unsafe withRStorable_

sger :: PrimMonad m => GerFun Float orient (PrimState m) m
sger = gerAbstraction "sger" cblas_sger_safe cblas_sger_unsafe (\x f -> f x)

dger :: PrimMonad m => GerFun Double orient (PrimState m) m
dger = gerAbstraction "dger" cblas_dger_safe cblas_dger_unsafe (\x f -> f x)

cgerc :: PrimMonad m => GerFun (Complex Float) orient (PrimState m) m
cgerc = gerAbstraction "cgerc" cblas_cgerc_safe cblas_cgerc_unsafe withRStorable_

zgerc :: PrimMonad m => GerFun (Complex Double) orient (PrimState m) m
zgerc = gerAbstraction "zgerc" cblas_zgerc_safe cblas_zgerc_unsafe withRStorable_

cgeru :: PrimMonad m => GerFun (Complex Float) orient (PrimState m) m
cgeru = gerAbstraction "cgeru" cblas_cgeru_safe cblas_cgeru_unsafe withRStorable_

zgeru :: PrimMonad m => GerFun (Complex Double) orient (PrimState m) m
zgeru = gerAbstraction "zgeru" cblas_zgeru_safe cblas_zgeru_unsafe withRStorable_

chbmv :: PrimMonad m => HbmvFun (Complex Float) orient (PrimState m) m
chbmv = hbmvAbstraction "chbmv" cblas_chbmv_safe cblas_chbmv_unsafe withRStorable_

zhbmv :: PrimMonad m => HbmvFun (Complex Double) orient (PrimState m) m
zhbmv = hbmvAbstraction "zhbmv" cblas_zhbmv_safe cblas_zhbmv_unsafe withRStorable_

chemv :: PrimMonad m => HemvFun (Complex Float) orient (PrimState m) m
chemv = hemvAbstraction "chemv" cblas_chemv_safe cblas_chemv_unsafe withRStorable_

zhemv :: PrimMonad m => HemvFun (Complex Double) orient (PrimState m) m
zhemv = hemvAbstraction "zhemv" cblas_zhemv_safe cblas_zhemv_unsafe withRStorable_

cher :: PrimMonad m => HerFun Float (Complex Float) orient (PrimState m) m
cher = herAbstraction "cher" cblas_cher_safe cblas_cher_unsafe (\x f -> f x)

zher :: PrimMonad m => HerFun Double (Complex Double) orient (PrimState m) m
zher = herAbstraction "zher" cblas_zher_safe cblas_zher_unsafe (\x f -> f x)

cher2 :: PrimMonad m => Her2Fun (Complex Float) orient (PrimState m) m
cher2 = her2Abstraction "cher2" cblas_cher2_safe cblas_cher2_unsafe withRStorable_

zher2 :: PrimMonad m => Her2Fun (Complex Double) orient (PrimState m) m
zher2 = her2Abstraction "zher2" cblas_zher2_safe cblas_zher2_unsafe withRStorable_

chpmv :: PrimMonad m => HpmvFun (Complex Float) orient (PrimState m) m
chpmv = hpmvAbstraction "chpmv" cblas_chpmv_safe cblas_chpmv_unsafe withRStorable_

zhpmv :: PrimMonad m => HpmvFun (Complex Double) orient (PrimState m) m
zhpmv = hpmvAbstraction "zhpmv" cblas_zhpmv_safe cblas_zhpmv_unsafe withRStorable_

chpr :: PrimMonad m => HprFun Float (Complex Float) orient (PrimState m) m
chpr = hprAbstraction "chpr" cblas_chpr_safe cblas_chpr_unsafe (\x f -> f x)

zhpr :: PrimMonad m => HprFun Double (Complex Double) orient (PrimState m) m
zhpr = hprAbstraction "zhpr" cblas_zhpr_safe cblas_zhpr_unsafe (\x f -> f x)

chpr2 :: PrimMonad m => Hpr2Fun (Complex Float) orient (PrimState m) m
chpr2 = hpr2Abstraction "chpr2" cblas_chpr2_safe cblas_chpr2_unsafe withRStorable_

zhpr2 :: PrimMonad m => Hpr2Fun (Complex Double) orient (PrimState m) m
zhpr2 = hpr2Abstraction "zhpr2" cblas_zhpr2_safe cblas_zhpr2_unsafe withRStorable_

ssbmv :: PrimMonad m => SbmvFun Float orient (PrimState m) m
ssbmv = sbmvAbstraction "ssbmv" cblas_ssbmv_safe cblas_ssbmv_unsafe (\x f -> f x)

dsbmv :: PrimMonad m => SbmvFun Double orient (PrimState m) m
dsbmv = sbmvAbstraction "dsbmv" cblas_dsbmv_safe cblas_dsbmv_unsafe (\x f -> f x)

sspmv :: PrimMonad m => SpmvFun Float orient (PrimState m) m
sspmv = spmvAbstraction "sspmv" cblas_sspmv_safe cblas_sspmv_unsafe (\x f -> f x)

dspmv :: PrimMonad m => SpmvFun Double orient (PrimState m) m
dspmv = spmvAbstraction "dspmv" cblas_dspmv_safe cblas_dspmv_unsafe (\x f -> f x)

sspr :: PrimMonad m => SprFun Float orient (PrimState m) m
sspr = sprAbstraction "sspr" cblas_sspr_safe cblas_sspr_unsafe (\x f -> f x)

dspr :: PrimMonad m => SprFun Double orient (PrimState m) m
dspr = sprAbstraction "dspr" cblas_dspr_safe cblas_dspr_unsafe (\x f -> f x)

sspr2 :: PrimMonad m => Spr2Fun Float orient (PrimState m) m
sspr2 = spr2Abstraction "sspr2" cblas_sspr2_safe cblas_sspr2_unsafe (\x f -> f x)

dspr2 :: PrimMonad m => Spr2Fun Double orient (PrimState m) m
dspr2 = spr2Abstraction "dspr2" cblas_dspr2_safe cblas_dspr2_unsafe (\x f -> f x)

ssymv :: PrimMonad m => SymvFun Float orient (PrimState m) m
ssymv = symvAbstraction "ssymv" cblas_ssymv_safe cblas_ssymv_unsafe (\x f -> f x)

dsymv :: PrimMonad m => SymvFun Double orient (PrimState m) m
dsymv = symvAbstraction "dsymv" cblas_dsymv_safe cblas_dsymv_unsafe (\x f -> f x)

ssyr :: PrimMonad m => SyrFun Float orient (PrimState m) m
ssyr = syrAbstraction "ssyr" cblas_ssyr_safe cblas_ssyr_unsafe (\x f -> f x)

dsyr :: PrimMonad m => SyrFun Double orient (PrimState m) m
dsyr = syrAbstraction "dsyr" cblas_dsyr_safe cblas_dsyr_unsafe (\x f -> f x)

ssyr2 :: PrimMonad m => Syr2Fun Float orient (PrimState m) m
ssyr2 = syr2Abstraction "ssyr2" cblas_ssyr2_safe cblas_ssyr2_unsafe (\x f -> f x)

dsyr2 :: PrimMonad m => Syr2Fun Double orient (PrimState m) m
dsyr2 = syr2Abstraction "dsyr2" cblas_dsyr2_safe cblas_dsyr2_unsafe (\x f -> f x)

stbmv :: PrimMonad m => TbmvFun Float orient (PrimState m) m
stbmv = tbmvAbstraction "stbmv" cblas_stbmv_safe cblas_stbmv_unsafe

dtbmv :: PrimMonad m => TbmvFun Double orient (PrimState m) m
dtbmv = tbmvAbstraction "dtbmv" cblas_dtbmv_safe cblas_dtbmv_unsafe

ctbmv :: PrimMonad m => TbmvFun (Complex Float) orient (PrimState m) m
ctbmv = tbmvAbstraction "ctbmv" cblas_ctbmv_safe cblas_ctbmv_unsafe

ztbmv :: PrimMonad m => TbmvFun (Complex Double) orient (PrimState m) m
ztbmv = tbmvAbstraction "ztbmv" cblas_ztbmv_safe cblas_ztbmv_unsafe

stbsv :: PrimMonad m => TbsvFun Float orient (PrimState m) m
stbsv = tbsvAbstraction "stbsv" cblas_stbsv_safe cblas_stbsv_unsafe

dtbsv :: PrimMonad m => TbsvFun Double orient (PrimState m) m
dtbsv = tbsvAbstraction "dtbsv" cblas_dtbsv_safe cblas_dtbsv_unsafe

ctbsv :: PrimMonad m => TbsvFun (Complex Float) orient (PrimState m) m
ctbsv = tbsvAbstraction "ctbsv" cblas_ctbsv_safe cblas_ctbsv_unsafe

ztbsv :: PrimMonad m => TbsvFun (Complex Double) orient (PrimState m) m
ztbsv = tbsvAbstraction "ztbsv" cblas_ztbsv_safe cblas_ztbsv_unsafe

stpmv :: PrimMonad m => TpmvFun Float orient (PrimState m) m
stpmv = tpmvAbstraction "stpmv" cblas_stpmv_safe cblas_stpmv_unsafe

dtpmv :: PrimMonad m => TpmvFun Double orient (PrimState m) m
dtpmv = tpmvAbstraction "dtpmv" cblas_dtpmv_safe cblas_dtpmv_unsafe

ctpmv :: PrimMonad m => TpmvFun (Complex Float) orient (PrimState m) m
ctpmv = tpmvAbstraction "ctpmv" cblas_ctpmv_safe cblas_ctpmv_unsafe

ztpmv :: PrimMonad m => TpmvFun (Complex Double) orient (PrimState m) m
ztpmv = tpmvAbstraction "ztpmv" cblas_ztpmv_safe cblas_ztpmv_unsafe

stpsv :: PrimMonad m => TpsvFun Float orient (PrimState m) m
stpsv = tpsvAbstraction "stpsv" cblas_stpsv_safe cblas_stpsv_unsafe

dtpsv :: PrimMonad m => TpsvFun Double orient (PrimState m) m
dtpsv = tpsvAbstraction "dtpsv" cblas_dtpsv_safe cblas_dtpsv_unsafe

ctpsv :: PrimMonad m => TpsvFun (Complex Float) orient (PrimState m) m
ctpsv = tpsvAbstraction "ctpsv" cblas_ctpsv_safe cblas_ctpsv_unsafe

ztpsv :: PrimMonad m => TpsvFun (Complex Double) orient (PrimState m) m
ztpsv = tpsvAbstraction "ztpsv" cblas_ztpsv_safe cblas_ztpsv_unsafe

strmv :: PrimMonad m => TrmvFun Float orient (PrimState m) m
strmv = trmvAbstraction "strmv" cblas_strmv_safe cblas_strmv_unsafe

dtrmv :: PrimMonad m => TrmvFun Double orient (PrimState m) m
dtrmv = trmvAbstraction "dtrmv" cblas_dtrmv_safe cblas_dtrmv_unsafe

ctrmv :: PrimMonad m => TrmvFun (Complex Float) orient (PrimState m) m
ctrmv = trmvAbstraction "ctrmv" cblas_ctrmv_safe cblas_ctrmv_unsafe

ztrmv :: PrimMonad m => TrmvFun (Complex Double) orient (PrimState m) m
ztrmv = trmvAbstraction "ztrmv" cblas_ztrmv_safe cblas_ztrmv_unsafe

strsv :: PrimMonad m => TrsvFun Float orient (PrimState m) m
strsv = trsvAbstraction "strsv" cblas_strsv_safe cblas_strsv_unsafe

dtrsv :: PrimMonad m => TrsvFun Double orient (PrimState m) m
dtrsv = trsvAbstraction "dtrsv" cblas_dtrsv_safe cblas_dtrsv_unsafe

ctrsv :: PrimMonad m => TrsvFun (Complex Float) orient (PrimState m) m
ctrsv = trsvAbstraction "ctrsv" cblas_ctrsv_safe cblas_ctrsv_unsafe

ztrsv :: PrimMonad m => TrsvFun (Complex Double) orient (PrimState m) m
ztrsv = trsvAbstraction "ztrsv" cblas_ztrsv_safe cblas_ztrsv_unsafe

-- Level 3
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
