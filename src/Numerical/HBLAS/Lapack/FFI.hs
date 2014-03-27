

module Numerical.HBLAS.Lapack.FFI where




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

type Stride_C =
type Fun_FFI_GESVX el = Ptr Fact_C  {- fact -}-> Ptr Trans_C {- trans -} 
    -> Ptr Int32  {-n -}-> Ptr Int32 {- NRHS -}-> 
    Ptr el {- a -} -> Ptr Stride_C {- lda -} -> Ptr Double {- af -} -> Ptr Stride_C  {- ldf-}->
    Ptr Int32 -> Ptr Equilib_C {- equed -} -> Ptr el {- r -} -> Ptr el  ->
    Ptr el {- b -} -> Ptr Stride_C {- ld b   -} -> Ptr el {- x -} -> Ptr Stride_C {- ldx -}-> 
    Ptr el {-rcond -}-> Ptr el {- ferr-} -> Ptr el {-berr-} -> Ptr el {-work-}->
    Ptr Int32 {-iwork -}-> Ptr Int32 {-info  -}

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
Xgesvx  is the s -sing

-}

foreign import ccall  "sgesvx_"  sgesvx :: Fun_FFI_GESVX Float 
foreign import ccall  "dgesvx_"  dgesvx :: Fun_FFI_GESVX Double
foreign import ccall  "cgesvx_"  cgesvx :: Fun_FFI_GESVX (Complex Float)
foreign import ccall  "zgesvx_"  zgesvx :: Fun_FFI_GESVX (Complex Double)