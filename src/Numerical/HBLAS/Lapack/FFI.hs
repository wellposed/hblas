

module Numerical.HBLAS.Lapack.FFI where
import Foreign.Ptr
import Foreign()
import Foreign.C.Types
import Data.Complex
import Data.Int


{-?ges

-}
{-

stylenote: we will not use the LAPACKE_* operations, only the
LAPACKE_*_work variants that require an explicitly provided work buffer.

This is to ensure that solver routine allocation behavior is transparent


-}



{-
void LAPACK_dgesvx( char* fact, char* trans, lapack_int* n, lapack_int* nrhs,
                    double* a, lapack_int* lda, double* af, lapack_int* ldaf,
                    lapack_int* ipiv, char* equed, double* r, double* c,
                    double* b, lapack_int* ldb, double* x, lapack_int* ldx,
                    double* rcond, double* ferr, double* berr, double* work,
                    lapack_int* iwork, lapack_int *info );



-}


{-
    fortran FFI conventions!
-}

--type Stride_C =
newtype JobTy = JBT CChar
newtype UploTy = UPLT CChar
newtype Info = Info Int32


newtype Fact_C = Fact_C CChar
newtype Trans_C = Trans_C CChar
newtype Stride_C = Stride_C Int32
newtype Equilib_C = Equilib_C CChar

type Fun_FFI_GESVX el = Ptr Fact_C  {- fact -}-> Ptr Trans_C {- trans -}
    -> Ptr Int32  {-n -}-> Ptr Int32 {- NRHS -}->
    Ptr el {- a -} -> Ptr Stride_C {- lda -} -> Ptr Double {- af -} -> Ptr Stride_C  {- ldf-}->
    Ptr Int32 -> Ptr Equilib_C {- equed -} -> Ptr el {- r -} -> Ptr el  ->
    Ptr el {- b -} -> Ptr Stride_C {- ld b   -} -> Ptr el {- x -} -> Ptr Stride_C {- ldx -}->
    Ptr el {-rcond -}-> Ptr el {- ferr-} -> Ptr el {-berr-} -> Ptr el {-work-}->
    Ptr Int32 {-iwork -}-> Ptr Info {-info  -} -> IO ()

{-

the prefixes mean s=single,d=double,c=complex float,d=complex double


fact will be a 1 character C string
either
    "F", then the inputs af and ipiv already contain the permuted LU factorization
        (act as input rather than result params)
    "E", Matrix input A will be equilibriated if needed, then copied to AF and Factored
    "N", matrix input A will be copied to AF

-}


{-
im assuming for now that any real use of *gesvx routines, or any other
n^3 complexity algs from LAPACK, are on inputs typically  n>=15, which means > 1000 flops,
which is > 1µs, and thus ok to always use the Safe FFI convention
-}

--need to get around to wrapping these, but thats for another day
foreign import ccall  "sgesvx_"  sgesvx :: Fun_FFI_GESVX Float
foreign import ccall  "dgesvx_"  dgesvx :: Fun_FFI_GESVX Double
foreign import ccall  "cgesvx_"  cgesvx :: Fun_FFI_GESVX (Complex Float)
foreign import ccall  "zgesvx_"  zgesvx :: Fun_FFI_GESVX (Complex Double)



--lapack_int ?syev_(  char *jobz, char *uplo, lapack_int *n, ?* a, lapack_int * lda, ?* w );
-- ? is Double or Float



--basic symmetric eigen value solvers
type SYEV_FUN_FFI elem = Ptr JobTy -> Ptr UploTy -> Ptr Int32  -> Ptr elem -> Ptr Int32 -> Ptr elem -> Ptr Info-> IO ()
foreign import ccall "ssyev_" ssyev_ffi :: SYEV_FUN_FFI Float
foreign import ccall "dsyev_" dsyev_ffi :: SYEV_FUN_FFI Double

{-unsafe versions of lapack routines are meant to ONLY be used for workspace queries-}
foreign import ccall unsafe "ssyev_" ssyev_ffi_unsafe :: SYEV_FUN_FFI Float
foreign import ccall unsafe "dsyev_" dsyev_ffi_unsafe :: SYEV_FUN_FFI Double

--lapack_int LAPACKE_<?>gesv( int matrix_order, lapack_int n, lapack_int nrhs, <datatype>* a, lapack_int lda, lapack_int* ipiv, <datatype>* b, lapack_int ldb );
--call sgesv( n, nrhs, a, lda, ipiv, b, ldb, info )

type GESV_FUN_FFI elem = Ptr Int32  {- n -} -> Ptr Int32 {- nrhs -} -> Ptr elem {- a -}
            -> Ptr Int32 {-  lda -} -> Ptr Int32 {- permutation vector -}
            -> Ptr elem  {- b -} -> Ptr Int32 {- ldb -} -> Ptr Info -> IO ()
-- | basic Linear system solvers. they act inplace
foreign import ccall  "sgesv_"  sgesv_ffi  ::GESV_FUN_FFI Float
foreign import ccall  "dgesv_"  dgesv_ffi :: GESV_FUN_FFI Double
foreign import ccall  "cgesv_"  cgesv_ffi :: GESV_FUN_FFI (Complex Float)
foreign import ccall  "zgesv_"  zgesv_ffi :: GESV_FUN_FFI (Complex Double)

-- / not sure if linear solvers ever are run in < 1 microsecond size instances in practice
foreign import ccall unsafe "sgesv_"  sgesv_ffi_unsafe  ::GESV_FUN_FFI Float
foreign import ccall unsafe "dgesv_"  dgesv_ffi_unsafe :: GESV_FUN_FFI Double
foreign import ccall unsafe "cgesv_"  cgesv_ffi_unsafe :: GESV_FUN_FFI (Complex Float)
foreign import ccall unsafe "zgesv_"  zgesv_ffi_unsafe :: GESV_FUN_FFI (Complex Double)

{- only provide  -}




